{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Scryptic.Monad (
  -- * Monadic interface for hooking up the ScryptEngine
  ScrypticM(..),

  -- * Creating hooks
  scryptInput,
  scryptOutput,
  setNamespace,
  subNamespace,
  -- ** Using hooks
  joinToEngine,
  getScryptHooks,
) where

import Scryptic.Types
import qualified Scryptic.Pure as Pure
import Scryptic.Runtime

import Control.Applicative
import Control.Lens
import Control.Monad.State

import qualified Data.Map as Map
import Data.Monoid
import Data.Typeable

data MState = MState
    { _mScryptHooks :: ScryptHooks
    , _mNamespace   :: String
    }

$(makeLenses ''MState)

instance Monoid MState where
    mempty = MState mempty mempty
    l `mappend` r = MState (l^.mScryptHooks <> r^.mScryptHooks)
                           (r^.mNamespace)
        -- namespace is notionally the same as 'Last String'

newtype ScrypticM m a = ScrypticM { runSM :: StateT MState m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadState MState (ScrypticM m) where
    get = ScrypticM get
    put = ScrypticM . put
    state = ScrypticM . state

getScryptHooks :: Monad m => ScrypticM m a -> m (a,ScryptHooks)
getScryptHooks m = (liftM . fmap) (view mScryptHooks)
    $ runStateT (runSM m) mempty

-- | convenience function to build a 'ScryptHooks' and join it
-- to an existing engine.
joinToEngine :: MonadIO m => ScryptEngine -> ScrypticM m a -> m a
joinToEngine engine m = do
    (a,scryptHooks) <- getScryptHooks m
    liftIO $ joinScryptEngine scryptHooks engine
    return a

scryptInput :: (Monad m, Typeable a, Read a) => String -> (a -> IO ())
            -> ScrypticM m ()
scryptInput key akt = do
    key' <- applyNamespace key
    mScryptHooks <>= Pure.scryptInput (key') akt

scryptOutput :: (MonadIO m, Typeable a, Read a, Show a) => String
             -> ScrypticM m (a->IO())
scryptOutput key = do
    key' <- applyNamespace key
    (akt,scryptHooks) <- liftIO $ Pure.scryptOutput key'
    mScryptHooks <>= scryptHooks
    return akt

-- | Set a namespace context for following bindings.  Namespaces are
-- accessed in scrypts as 'namespace.key'
setNamespace :: Monad m => String -> ScrypticM m ()
setNamespace nm = mNamespace .= nm

-- | Run a 'ScrypticM' under the current namespace.
subNamespace :: Monad m => ScrypticM m a -> ScrypticM m a
subNamespace m = do
    s0 <- get
    let nm0 = s0^.mNamespace
    put mempty
    r <- m
    newHooks <- use mScryptHooks
    let newHooks'  = newHooks & over inpMap (Map.mapKeysMonotonic modKey)
                              & over outMap (Map.mapKeysMonotonic modKey)
        modKey key = concat [nm0,".",key]
    put $ s0 & mScryptHooks <>~ newHooks'
    return r

applyNamespace :: Monad m => String -> ScrypticM m String
applyNamespace key = do
    nm <- use mNamespace
    if null nm then return key else return $ concat [nm, ".", key]
