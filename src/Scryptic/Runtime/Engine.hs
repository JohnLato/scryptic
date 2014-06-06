{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS -Wall #-}
module Scryptic.Runtime.Engine (
  ScryptEngine(..),
  ScryptThreadM(..),
  startScryptEngine,
  joinScryptEngine,

  defaultOneshotContext,
  defaultInteractiveContext,
  runThread,

  dumpScrypt,
  doWithFlow,
  evalLine,

  errCxt,
) where

import Scryptic.Runtime.Options
import Scryptic.Parse
import Scryptic.Scrypt
import Scryptic.Types
import Scryptic.Weak

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Catch as E
import Control.Monad.Reader

import qualified Data.Foldable as Fold
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Typeable

import System.IO (hPutStrLn, stderr)
import System.IO.Error (isIllegalOperation)
import Foreign.C.Error (errnoToIOError, eBADF)

-- runtime state shared between the main engine and all executing contexts.
data RuntimeState = RuntimeState
    { _rsInpMap :: TVar (Map Key (Weak Input))
    , _rsOutMap :: TVar (Map Key Output)
    }

data ScryptEngine = ScryptEngine
    { _seState  :: RuntimeState
    , _seOpts   :: ScryptOpt
    , _seNextId :: TVar Int
    }

-- all necessary data for a single scrypt context.
-- Currently we don't bother to multiplex multiple contexts here, instead
-- we leave it up to the Haskell runtime.
data ScryptThreadCxt = ScryptThreadCxt
    { _stcState  :: RuntimeState
    , _stcId     :: Int
    , _stcOpts   :: TVar ScryptOpt
    , _stcStdout :: Text -> IO ()
    , _stcStderr :: Text -> IO ()
    , _stcStdin  :: IO (Maybe Scrypt)
    , _stcErrCxt :: [Text]
    }

$(makeLenses ''RuntimeState)
$(makeLenses ''ScryptEngine)
$(makeLenses ''ScryptThreadCxt)

--------------------------------------------------------------
-- engine-related functions

-- | top-level entrypoint.  Call this initially with global options.
--
-- Calling this multiple times will return new, unshared ScryptEngines.
startScryptEngine :: ScryptOpt -> IO ScryptEngine
startScryptEngine opt0 = do
    inpMapRef <- newTVarIO Map.empty
    outMapRef <- newTVarIO Map.empty
    threadIdRef <- newTVarIO 0

    let state0 = RuntimeState inpMapRef outMapRef
        sEngine = ScryptEngine state0 opt0 threadIdRef
    return sEngine

-- | modify the inputs/outputs available to the ScryptEngine.
--
-- thread-safe, can be called multiple times.  Each new call
-- will hook in additional inputs/outputs.
--
-- It is an error to attempt to create a new input or output using
-- an already-existing name (inputs and outputs are in separate namespaces
-- however)
joinScryptEngine :: ScryptEngine -> ScryptHooks -> IO ()
joinScryptEngine sEngine scryptic = do
    let rState = sEngine^.seState
        remKey mp key = atomically $ modifyTVar mp (Map.delete key)

    wkInpMap <- imapM (\key i@(Input tv) ->
                  mkWeakTVarKey tv i (Just $ remKey (rState^.rsInpMap) key))
                  (scryptic^.inpMap)

    atomically $ do
        modifyTVar (rState^.rsInpMap) (merge "input"  wkInpMap)
        modifyTVar (rState^.rsOutMap) (merge "output" (scryptic^.outMap))
        imapMOf_ (itraversed.outputFinalizer)
            (\key -> ($ remKey (rState^.rsOutMap) key)) (scryptic^.outMap)
    return ()

newThreadId :: ScryptEngine -> IO Int
newThreadId engine  = atomically $
    readTVar ref <* modifyTVar' ref (+1)
  where
    ref = engine ^. seNextId

--------------------------------------------------------------
-- thread-related functions

-- monad for an executing thread context
newtype ScryptThreadM a = ScryptThreadM
    { runScryptThread :: ReaderT ScryptThreadCxt IO a }
    deriving (Functor, Monad, Applicative, MonadIO, MonadThrow, MonadCatch)

instance MonadReader ScryptThreadCxt ScryptThreadM where
    ask = ScryptThreadM ask
    local f = ScryptThreadM . local f . runScryptThread
    reader = ScryptThreadM . reader

defaultInteractiveContext :: ScryptEngine -> IO ScryptThreadCxt
defaultInteractiveContext engine = do
    tId <- newThreadId engine
    optRef <- engine ^! seOpts . act newTVarIO
    let getInput = do
            (parseScript <$> Text.getLine) >>= \case
              Right scrypt -> return (Just scrypt)
              Left err -> hPutStrLn stderr ("scryptic: error parsing input: " ++ err) >> return Nothing

    return $ ScryptThreadCxt
                { _stcState = engine ^. seState
                , _stcId    = tId
                , _stcOpts  = optRef
                , _stcStdout = Text.putStrLn
                , _stcStderr = Text.hPutStrLn stderr
                , _stcStdin  = getInput
                , _stcErrCxt = ["scryptic"]
                }

defaultOneshotContext :: ScryptEngine -> Scrypt -> IO ScryptThreadCxt
defaultOneshotContext engine scrypt = do
    tId <- newThreadId engine
    optRef <- engine ^! seOpts . act newTVarIO
    inp <- newTMVarIO $ scrypt
    let getInput = do
            x <- atomically (tryTakeTMVar inp)
            when (isNothing x) $ E.throwM $ errnoToIOError "defaultOneshotContext" eBADF Nothing Nothing
            return x

    return $ ScryptThreadCxt
                { _stcState = engine ^. seState
                , _stcId    = tId
                , _stcOpts  = optRef
                , _stcStdout = Text.putStrLn
                , _stcStderr = Text.hPutStrLn stderr
                , _stcStdin  = getInput
                , _stcErrCxt = ["scryptic"]
                }

runThread :: Text -> ScryptThreadCxt -> IO ()
runThread lbl stc = do
    runReaderT (runScryptThread runner) stc `E.finally` cleanup stc
  where
    runner = outerCxt doWithFlow
    outerCxt = if Text.null lbl then id else errCxt lbl

cleanup :: ScryptThreadCxt -> IO ()
cleanup stc = do
    inputMap <- stc ^! stcState . rsInpMap . act readTVarIO
    let threadId = stc^.stcId
        cleanRef (Input innerRef) = atomically $ do
            innerMap <- readTVar innerRef
            when ( innerMap^.imWatching.to (IntMap.member threadId)
                || innerMap^.imWaiting.to (IntMap.member threadId)) $
                writeTVar innerRef
                    $ mapBoth (IntMap.delete threadId) innerMap
    Fold.mapM_ (perform (act deRefWeak . _Just . act cleanRef)) inputMap

-- currently the only flow control construct I have is the script title.
-- I guess more will likely be added if more power is needed.
doWithFlow :: ScryptThreadM ()
doWithFlow = do
    getNext <- view stcStdin

    let loop = liftIO (E.try getNext) >>= \case
            Right (Just scrypt) -> do
                dumpScrypt scrypt
                mapM_ doBlock $ getBlocks scrypt
                loop
            Right Nothing -> loop
            Left e | isIllegalOperation e -> return ()
                   | otherwise -> E.throwM e
    loop
  where
    doBlock (Block opts stmts) =
        let cfg = Fold.foldMap mkConfig opts
        in  runCfg cfg stmts
    runCfg bcCfg stmts = case bcCfg^.bcTitle.to getLast of
        Nothing -> mapM_ checkLine stmts
        Just title -> errCxt (Text.pack title) (mapM_ checkLine stmts)
    checkLine stmt = evalLine stmt `E.catch` \(BadKey _) -> return ()

evalLine :: Stmt -> ScryptThreadM ()
evalLine ln = case ln of
    Wait expr -> stepTrace traceMsg . errCxt "wait"
                 $ runExprWaiter (return ()) expr
        where traceMsg = Text.concat ["wait: ", "expr: ADD SHOW" ]

    Write (sKey->key) val's -> stepTrace traceMsg . errCxt "write"
        $ withOutKey key $ \expectType out -> case sOptVal val's of
            Just x  -> liftIO . void $ out x
            Nothing -> doError $ Text.concat
                  [ "can't read input `", Text.pack $ show val's
                  , "' as type ", Text.pack $ show expectType ]
        where traceMsg = Text.concat
                ["write ", keyStr key,"; ",valStr $ Text.pack $ show val's]

    WriteSync (sKey->key) val's expr ->
        stepTrace traceMsg . errCxt "write" $
          errCxt "sync" $ withOutKey key $ \expectType out ->
              case sOptVal val's of
                Just x  -> runExprWaiter (out x) expr
                Nothing -> doError $ Text.concat
                                [ "can't trigger type "
                                , Text.pack $ show expectType ]
        where traceMsg = Text.concat
                  ["write ", keyStr key,"; "
                  ,prettyPair "sync" $ Text.pack $ show expr]


    Watch (sKey->key) -> stepTrace traceMsg . errCxt "watch"
        $ watchInputKey key $ \watch _unwatch -> do
            printer <- view stcStdout
            liftIO . atomically . watch $ printMsg printer
        where traceMsg =
                  Text.concat [ "watch: ", keyStr key ]
              printMsg printer x = printer
                  $ Text.concat [ "<", unKey key, "> " , Text.pack $ show x]

    Unwatch (sKey->key) -> stepTrace traceMsg . errCxt "unwatch"
        $ watchInputKey key $ \_watch unwatch -> liftIO $ atomically unwatch
        where traceMsg = "unwatch: " <> keyStr key

    SetOpt (keyStr.sKey->key) ((^.identS)->val) ->
        stepTrace traceMsg . errCxt "setopt" $ case getValuedOptionSetter key val of
          Left err -> doError $ "can't find option setter: " <> err
          Right f -> do
              optRef <- view $ stcOpts
              liftIO . atomically $ modifyTVar optRef (unSOA f)
        where traceMsg = Text.concat
                ["setopt ", key,"; ",valStr $ Text.pack val]

    Sleep (sNumD->nSecs) -> stepTrace ("sleep: " <> Text.pack (show nSecs))
          . errCxt "sleep" . liftIO $ threadDelay (floor (nSecs*1000000))

-- For complicated expressions, we want to ensure
--  1. We always check a coherent snapshot of values
--  2. Every time a value is updated, we re-check the expression
--  3. No updates are missed.
--
--  The solution is to use a simple semaphore.  The scrypt engine (which
--  is in an isolated thread) waits on the semaphore then checks the
--  expression.  If the check fails we loop, otherwise we pass out of
--  the expression.  Every time an input changes, it also frees the
--  semaphore.  This guarantees we don't miss any updates (since the sem
--  will block if the current values haven't been checked).
runExprWaiter :: IO a -> Expr -> ScryptThreadM ()
runExprWaiter preWait expr = case evalExpr expr of
    Right cmpExpr -> do
        stepExc <- liftIO newEmptyTMVarIO
        E.try (installExprWaiter stepExc cmpExpr) >>= \case
            Right stm   ->
                let loop = do
                        goodStep <- atomically $ do
                            takeTMVar stepExc
                            (True <$ stm) `orElse` return False
                        when (not goodStep) loop
                in liftIO $ preWait >> loop
            Left (BadKey key) -> doError $ Text.concat
                [ "aborted expression <"
                , Text.pack $ show expr
                , "> due to errors with key "
                , key ]
    Left err      -> doError $ Text.unlines
        ["errors compiling expression: " <> Text.pack err
        , Text.pack $ show expr
        ]

installExprWaiter :: TMVar () -> CompiledExpr -> ScryptThreadM (STM ())
installExprWaiter stepExc CompiledExpr{ceKey, cePred} = waitInputKey ceKey $ \wait unwait -> liftIO $ do
    let p = maybe (throwSTM $ BadCast $ unKey ceKey) check . cePred
    valRef <- newTVarIO Nothing
    let akt x =
            atomically $ putTMVar stepExc () >> writeTVar valRef (Just x)
    atomically $ wait akt
    return $ do
        unwait
        maybe retry p =<< readTVar valRef

installExprWaiter stepExc (CmpOrExpr a b) =
    orElse <$> installExprWaiter stepExc a <*> installExprWaiter stepExc b
installExprWaiter stepExc (CmpAndExpr a b) =
    (>>) <$> installExprWaiter stepExc a <*> installExprWaiter stepExc b

errCxt :: Text -> ScryptThreadM a -> ScryptThreadM a
errCxt err = local (over stcErrCxt (err:))

watchInputKey
    :: Key
    -> (forall a.  (Typeable a, Read a, Show a, Ord a)
                => ((a -> IO ()) -> STM ()) -- watch on this key
                -> (STM ())                 -- unwatch this key
                -> ScryptThreadM b)
    -> ScryptThreadM b
watchInputKey = withInpKey' imWatching

waitInputKey
    :: Key
    -> (forall a.  (Typeable a, Read a, Show a, Ord a)
                => ((a -> IO ()) -> STM ()) -- watch on this key
                -> (STM ())                 -- unwatch this key
                -> ScryptThreadM b)
    -> ScryptThreadM b
waitInputKey = withInpKey' imWaiting

withInpKey' :: (forall x. Typeable x => Setting (->) (InputMap x) (InputMap x) (IntMap.IntMap (x->IO())) (IntMap.IntMap (x->IO())))
           -> Key
           -> (forall a. (Typeable a, Read a, Show a, Ord a)
                      => ((a -> IO ()) -> STM ()) -- watch on this key
                      -> (STM ())                 -- unwatch this key
                      -> ScryptThreadM b)
           -> ScryptThreadM b
withInpKey' sel key akt = do
    inpRef <- view $ stcState . rsInpMap
    threadId <- view stcId
    inpRef^! act (liftIO . readTVarIO) . atKey key >>= \case
            Nothing -> do
                iMap <- liftIO $ readTVarIO inpRef
                writeDebug $ Text.concat
                    [ "input keys: ["
                    , Text.intercalate ", " $ map unKey (Map.keys iMap)
                    , "]" ]
                doError $ "can't find key " <> unKey key
                E.throwM $ BadKey $ unKey key
            Just wkref -> liftIO (deRefWeak wkref) >>= \case
                Just (Input ref) -> do
                    let watch x = modifyTVar ref
                                  $ over sel (IntMap.insert threadId x)
                        unwatch = modifyTVar ref
                                  $ over sel (IntMap.delete threadId)
                    akt watch unwatch
                Nothing  -> do
                    doError $ "internal: weak ref expired: " <> unKey key
                    E.throwM $ BadKey $ unKey key

withOutKey :: Key
           -> (forall a. (Typeable a, Read a)
                => TypeRep -> (a -> IO ()) -> ScryptThreadM ())
           -> ScryptThreadM ()
withOutKey key akt = do
    outRef <- view $ stcState . rsOutMap
    outRef^! act (liftIO . readTVarIO) . atKey key >>= \case
            Nothing -> do
                oMap <- liftIO $ readTVarIO outRef
                writeDebug $ Text.concat
                    [ "output keys: ["
                    , Text.intercalate ", " $ map unKey (Map.keys oMap)
                    , "]"
                    ]
                doError $ "can't find key " <> unKey key
            Just (Output expectType out _) -> akt expectType out

dumpScrypt :: Scrypt -> ScryptThreadM ()
dumpScrypt scrypt = do
    opts <- view $ stcOpts
    printer <- view stcStderr
    showDump <- opts^!act (liftIO . readTVarIO) . soDumpScrypts
    when showDump $ liftIO . printer . Text.pack $ show scrypt

doError :: Text -> ScryptThreadM ()
doError err = do
    opts <- view $ stcOpts
    justDie <- opts^!act (liftIO . readTVarIO) . soDieOnErr
    if justDie
        then do
            cxt <- view stcErrCxt
            error . Text.unpack $ Text.intercalate ": "
                $ reverse (err:"fatal (dieOnErr)":cxt)
        else errCxt "runtime error" $ writeTrace err

-- | 
stepTrace :: Text -> ScryptThreadM a -> ScryptThreadM a
stepTrace msg akt = do
    optsRef <- view $ stcOpts
    doTrace <- optsRef^! act (liftIO . readTVarIO) . soTrace
    when doTrace $ errCxt "stepping" $ writeTrace msg
    akt

-- just output the message, with the current context stack.
writeTrace :: Text -> ScryptThreadM ()
writeTrace msg = do
    cxt     <- view stcErrCxt
    printer <- view stcStderr
    liftIO . printer $ (Text.intercalate ": " $ reverse (msg:cxt))

-- | print a debug message
writeDebug :: Text -> ScryptThreadM ()
writeDebug msg = do
    optsRef <- view $ stcOpts
    opts <- liftIO $ readTVarIO optsRef
    when (opts^.soDebug) $ writeTrace msg

prettyPair :: Text -> Text -> Text
prettyPair spec lbl = Text.concat [spec," <",lbl,">"]

keyStr :: Key -> Text
keyStr = prettyPair "key" . unKey

valStr :: Text -> Text
valStr = prettyPair "val"
