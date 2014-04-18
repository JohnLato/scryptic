{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS -Wall #-}
module Scryptic.Runtime (
  ScryptEngine,
  startScryptEngine,
  runScrypt,
  joinScryptEngine,
) where

import Scryptic.RuntimeOptions
import Scryptic.Scrypt
import Scryptic.Types
import Scryptic.Weak

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Reader
import Data.Foldable (foldMap)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Typeable
import Control.Monad.Catch as E

data RuntimeState = RuntimeState
    { _rsInpMap :: TVar (Map Key (Weak Input))
    , _rsOutMap :: TVar (Map Key Output)
    , _rsOpts   :: TVar ScryptOpt
    , _rsErrCxt :: [String]
    }

data ScryptEngine = ScryptEngine
    { _seState :: RuntimeState
    }

$(makeLenses ''RuntimeState)
$(makeLenses ''ScryptEngine)

startScryptEngine :: ScryptOpt -> IO ScryptEngine
startScryptEngine opt0 = do
    inpMapRef <- newTVarIO Map.empty
    outMapRef <- newTVarIO Map.empty
    optRef <- newTVarIO opt0
    let state0 = RuntimeState inpMapRef outMapRef optRef ["scryptic"]
        sEngine = ScryptEngine state0
    return sEngine

runScrypt :: String -> Scrypt -> ScryptEngine -> IO ()
runScrypt lbl scrypt sEngine = do
    runReaderT (unSEM runner) (sEngine^.seState)
  where
    runner = outerCxt $ dumpScrypt scrypt >> doWithFlow scrypt
    outerCxt = if null lbl then id else errCxt lbl

-- modify the inputs/outputs available to the ScryptEngine.
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
    imapMOf_ (itraversed.outputFinalizer._Just)
            (\key -> ($ remKey (rState^.rsOutMap) key)) (scryptic^.outMap)
    return ()

newtype ScryptEngineM a = ScryptEngineM
    { unSEM :: ReaderT RuntimeState IO a }
    deriving (Functor, Monad, Applicative, MonadIO, MonadThrow, MonadCatch)

instance MonadReader RuntimeState ScryptEngineM where
    ask = ScryptEngineM ask
    local f = ScryptEngineM . local f . unSEM
    reader = ScryptEngineM . reader

-- currently the only flow control construct I have is the script title.
-- I guess more will likely be added if more power is needed.
doWithFlow :: Scrypt -> ScryptEngineM ()
doWithFlow = mapM_ doBlock . getBlocks
  where
    doBlock (Block opts stmts) =
        let cfg = foldMap mkConfig opts
        in  runCfg cfg stmts
    runCfg bcCfg stmts = case bcCfg^.bcTitle.to getLast of
        Nothing -> mapM_ checkLine stmts
        Just title -> errCxt title (mapM_ checkLine stmts)
    checkLine stmt = evalLine stmt `E.catch` \(BadKey _) -> return ()

evalLine :: Stmt -> ScryptEngineM ()
evalLine ln = case ln of
    Wait expr -> stepTrace traceMsg . errCxt "wait"
                 $ runExprWaiter (return ()) expr
        where traceMsg = concat ["wait: ", "expr: ADD SHOW" ]

    Write (sKey->key) val's -> stepTrace traceMsg . errCxt "write"
        $ withOutKey key $ \expectType out -> case sOptVal val's of
            Just x  -> liftIO . void $ out x
            Nothing -> doError $ concat
                  [ "can't read input `", show val's
                  , "' as type ", show expectType ]
        where traceMsg = concat ["write ", keyStr key,"; ",valStr $ show val's]

    WriteSync (sKey->key) val's expr ->
        stepTrace traceMsg . errCxt "write" $
          errCxt "sync" $ withOutKey key $ \expectType out ->
              case sOptVal val's of
                Just x  -> runExprWaiter (out x) expr
                Nothing -> doError $ concat
                                [ "can't trigger type ", show expectType ]
        where traceMsg = concat
                  ["write ", keyStr key,"; ",prettyPair "sync" $ show expr]


    Watch (sKey->key) -> stepTrace traceMsg . errCxt "watch"
        $ withInpKey key $ \ref ->
            liftIO . atomically $ writeTVar ref printMsg
        where traceMsg =
                  concat [ "watch: ", keyStr key ]
              printMsg x = putStrLn
                  $ concat [ "<", unKey key, "> " , show x]

    Unwatch (sKey->key) -> stepTrace traceMsg . errCxt "unwatch"
        $ withInpKey key $ \ref ->
            liftIO . atomically $ writeTVar ref nullAkt
        where traceMsg = "unwatch: " ++ keyStr key

    SetOpt (keyStr.sKey->key) ((^.identS)->val) ->
        stepTrace traceMsg . errCxt "setopt" $ case getValuedOptionSetter key val of
          Left err -> doError $ concat
                                [ "can't find option setter: ", err ]
          Right f -> do
              optRef <- view rsOpts
              liftIO . atomically $ modifyTVar optRef (unSOA f)
        where traceMsg = concat ["setopt ", key,"; ",valStr val]

    Sleep (sNumD->nSecs) -> stepTrace ("sleep: " ++ show nSecs)
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
runExprWaiter :: IO a -> Expr -> ScryptEngineM ()
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
            Left (BadKey key) -> doError $
                "aborted expression <" ++ show expr ++ "> due to errors with key " ++ key
    Left err      -> doError $ unlines
        ["errors compiling expression: " ++ err
        , show expr
        ]

installExprWaiter :: TMVar () -> CompiledExpr -> ScryptEngineM (STM ())
installExprWaiter stepExc CompiledExpr{ceKey, cePred} = withInpKey ceKey $ \ref -> liftIO $ do
    let p = maybe (throwSTM $ BadCast $ show ceKey) check . cePred
    valRef <- newTVarIO Nothing
    -- we want to run the prior handler in addition to the new handler,
    -- then reinstall just the prior handler after this expression
    -- exits successfully.
    let akt prev x = do
            _ <- prev x
            atomically $ putTMVar stepExc () >> writeTVar valRef (Just x)
    prev <- atomically $ do
            prev <- readTVar ref
            writeTVar ref (akt prev)
            return prev
    return $ do
        writeTVar ref prev
        maybe retry p =<< readTVar valRef
installExprWaiter stepExc (CmpOrExpr a b) =
    orElse <$> installExprWaiter stepExc a <*> installExprWaiter stepExc b
installExprWaiter stepExc (CmpAndExpr a b) =
    (>>) <$> installExprWaiter stepExc a <*> installExprWaiter stepExc b

errCxt :: String -> ScryptEngineM a -> ScryptEngineM a
errCxt err = local (over rsErrCxt (err:))

withInpKey :: Key
           -> (forall a. (Typeable a, Read a, Show a, Ord a)
                      => TVar (a -> IO ()) -> ScryptEngineM b)
           -> ScryptEngineM b
withInpKey key akt = do
    inpRef <- view rsInpMap
    inpRef^! act (liftIO . readTVarIO) . atKey key >>= \case
            Nothing -> do
                iMap <- liftIO $ readTVarIO inpRef
                writeDebug $ "input keys: " ++ show (Map.keys iMap)
                doError $ "can't find key " ++ unKey key
                E.throwM $ BadKey $ unKey key
            Just wkref -> liftIO (deRefWeak wkref) >>= \case
                Just (Input ref) -> akt ref
                Nothing  -> do
                    doError $ "internal: weak ref expired: " ++ unKey key
                    E.throwM $ BadKey $ unKey key

withOutKey :: Key
           -> (forall a. (Typeable a, Read a)
                => TypeRep -> (a -> IO ()) -> ScryptEngineM ())
           -> ScryptEngineM ()
withOutKey key akt = do
    outRef <- view rsOutMap
    outRef^! act (liftIO . readTVarIO) . atKey key >>= \case
            Nothing -> do
                oMap <- liftIO $ readTVarIO outRef
                writeDebug $ "output keys: " ++ show (Map.keys oMap)
                doError $ "can't find key " ++ unKey key
            Just (Output expectType out _) -> akt expectType out

dumpScrypt :: Scrypt -> ScryptEngineM ()
dumpScrypt scrypt = do
    opts <- view rsOpts
    showDump <- opts^!act (liftIO . readTVarIO) . soDumpScrypts
    when showDump $ liftIO . putStrLn $ show scrypt

doError :: String -> ScryptEngineM ()
doError err = do
    opts <- view rsOpts
    justDie <- opts^!act (liftIO . readTVarIO) . soDieOnErr
    if justDie
        then do
            cxt <- view rsErrCxt
            error $ intercalate ": "
                $ reverse (err:"fatal (dieOnErr)":cxt)
        else errCxt "runtime error" $ writeTrace err

-- | 
stepTrace :: String -> ScryptEngineM a -> ScryptEngineM a
stepTrace msg akt = do
    optsRef <- view rsOpts
    doTrace <- optsRef^! act (liftIO . readTVarIO) . soTrace
    when doTrace $ errCxt "stepping" $ writeTrace msg
    akt

-- just output the message, with the current context stack.
writeTrace :: String -> ScryptEngineM ()
writeTrace msg = do
    cxt <- view rsErrCxt
    liftIO . putStrLn $ (intercalate ": " $ reverse (msg:cxt))

-- | print a debug message
writeDebug :: String -> ScryptEngineM ()
writeDebug msg = do
    optsRef <- view rsOpts
    opts <- liftIO $ readTVarIO optsRef
    when (opts^.soDebug) $ writeTrace msg

prettyPair :: String -> String -> String
prettyPair spec lbl = concat [spec," <",lbl,">"]

keyStr :: Key -> String
keyStr = prettyPair "key" . unKey
valStr :: String -> String
valStr = prettyPair "val"

nullAkt :: a -> IO ()
nullAkt _ = return ()
