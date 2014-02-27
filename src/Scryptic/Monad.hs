{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Scryptic.Monad (
  ScrypticM(..),
  getScryptHooks,
  scryptInput,
  scryptOutput,
  joinToEngine,
) where

import Scryptic.Types
import qualified Scryptic.Pure as Pure
import Scryptic.Runtime

import Control.Applicative
import Control.Monad.State

import Data.Monoid
import Data.Typeable

newtype ScrypticM a = ScrypticM { runSM :: StateT ScryptHooks IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState ScryptHooks ScrypticM where
    get = ScrypticM get
    put = ScrypticM . put
    state = ScrypticM . state

getScryptHooks :: ScrypticM a -> IO (a,ScryptHooks)
getScryptHooks m = runStateT (runSM m) mempty

-- convenience function to build a 'ScryptHooks' and join it
-- to an existing engine.
joinToEngine :: ScryptEngine -> ScrypticM a -> IO a
joinToEngine engine m = do
    (a,scryptic) <- getScryptHooks m
    joinScryptEngine scryptic engine
    return a

scryptInput :: (Typeable a, Read a) => String -> (a -> IO ()) -> ScrypticM ()
scryptInput key akt = modify (<> Pure.scryptInput key akt)

scryptOutput :: (Typeable a, Read a, Show a) => String -> ScrypticM (a->IO())
scryptOutput key = do
    (akt,scryptic) <- liftIO $ Pure.scryptOutput key
    modify (<> scryptic)
    return akt
