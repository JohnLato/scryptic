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

newtype ScrypticM a = ScrypticM { runSM :: StateT MState IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState MState ScrypticM where
    get = ScrypticM get
    put = ScrypticM . put
    state = ScrypticM . state

getScryptHooks :: ScrypticM a -> IO (a,ScryptHooks)
getScryptHooks m = (fmap . fmap) (view mScryptHooks)
    $ runStateT (runSM m) mempty

-- | convenience function to build a 'ScryptHooks' and join it
-- to an existing engine.
joinToEngine :: ScryptEngine -> ScrypticM a -> IO a
joinToEngine engine m = do
    (a,scryptHooks) <- getScryptHooks m
    joinScryptEngine scryptHooks engine
    return a

scryptInput :: (Typeable a, Read a) => String -> (a -> IO ()) -> ScrypticM ()
scryptInput key akt = do
    nm <- use mNamespace
    mScryptHooks <>= Pure.scryptInput (nm ++ key) akt

scryptOutput :: (Typeable a, Read a, Show a) => String -> ScrypticM (a->IO())
scryptOutput key = do
    nm <- use mNamespace
    (akt,scryptHooks) <- liftIO $ Pure.scryptOutput (nm ++ key)
    mScryptHooks <>= scryptHooks
    return akt

setNamespace :: String -> ScrypticM ()
setNamespace nm = mNamespace .= nm
