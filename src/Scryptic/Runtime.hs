{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

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
    deriving (Functor, Monad, Applicative, MonadIO)

instance MonadReader RuntimeState ScryptEngineM where
    ask = ScryptEngineM ask
    local f = ScryptEngineM . local f . unSEM
    reader = ScryptEngineM . reader

-- currently the only flow control construct I have is the script title.
-- I guess more will likely be added if more power is needed.
doWithFlow :: Scrypt -> ScryptEngineM ()
doWithFlow = mapM_ doBlock
  where
    doBlock x = case x of
        ScryptBlock stmts     -> mapM_ evalLine stmts
        TitledBlock str stmts -> errCxt str (mapM_ evalLine stmts)

evalLine :: ScryptStatement -> ScryptEngineM ()
evalLine ln = case ln of
    Wait key -> errCxt "wait" $ withInpKey key $ \ref -> stepTrace traceMsg
        $ liftIO . void $ installInputWaiter ref (return ())
        where traceMsg = keyStr key

    Write key val's -> errCxt "write" $ withOutKey key $
        \expectType out -> stepTrace traceMsg $ case mRead val's of
            Just x  -> liftIO . void $ out x
            Nothing -> doError $ concat
                  [ "can't read input `", val's
                  , "' as type ", show expectType ]
        where traceMsg = concat [keyStr key,"; ",valStr val's]

    WriteSync key val's sKey -> errCxt "write" $ withInpKey sKey $ \inRef ->
        errCxt "sync" $ withOutKey key $ \expectType out ->
            stepTrace traceMsg $ case mRead val's of
                Just x  -> liftIO . void $ installInputWaiter inRef (out x)
                Nothing -> doError $ concat
                            [ "can't trigger type ", show expectType ]
        where traceMsg = concat [keyStr key,"; ",prettyPair "sync" $ unKey sKey]


    Watch key -> errCxt "watch" $ withInpKey key $ \ref ->
        stepTrace traceMsg $ liftIO . atomically $ writeTVar ref printMsg
        where traceMsg = keyStr key
              printMsg x = putStrLn
                  $ concat [ "<", unKey key, "> " , show x]

    Unwatch key -> errCxt "unwatch" $ withInpKey key $ \ref ->
        stepTrace traceMsg $ liftIO . atomically $ writeTVar ref nullAkt
        where traceMsg = keyStr key

    SetOpt f lbl -> errCxt "setopt" $ stepTrace lbl $ do
          optRef <- view rsOpts
          liftIO . atomically $ modifyTVar optRef (unSOA f)

    Sleep nSecs -> errCxt "sleep" $ stepTrace ""
        $ liftIO $ threadDelay (floor (nSecs*1000000))

installInputWaiter :: TVar (b -> IO ()) -> IO a -> IO b
installInputWaiter ref preWait = do
    sem <- newEmptyTMVarIO
    let akt = atomically . putTMVar sem
    atomically $ writeTVar ref akt
    void preWait
    atomically $ do
        writeTVar ref nullAkt
        takeTMVar sem

errCxt :: String -> ScryptEngineM a -> ScryptEngineM a
errCxt err = local (over rsErrCxt (err:))

withInpKey :: Key
           -> (forall a. (Typeable a, Read a, Show a)
                      => TVar (a -> IO ()) -> ScryptEngineM ())
           -> ScryptEngineM ()
withInpKey key akt = do
    inpRef <- view rsInpMap
    inpRef^! act (liftIO . readTVarIO) . atKey key >>= \case
            Nothing -> do
                iMap <- liftIO $ readTVarIO inpRef
                writeDebug $ "input keys: " ++ show (Map.keys iMap)
                doError $ "can't find key " ++ unKey key
            Just wkref -> liftIO (deRefWeak wkref) >>= \case
                Just (Input ref) -> akt ref
                Nothing  -> doError $ "internal: weak ref expired: " ++ unKey key

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
    when showDump $ liftIO . putStrLn . unlines $ map show scrypt

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
