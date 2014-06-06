{-# OPTIONS -Wall #-}
module Scryptic.Runtime (
  ScryptEngine,
  startScryptEngine,
  joinScryptEngine,
  runScrypt,
  runInteractive,
) where

import Scryptic.Runtime.Engine
import Scryptic.Scrypt
import Data.Text (Text)

-- | Run a scrypt in the current thread.
--
-- You may want to wrap this in `async` or `forkIO`
runScrypt :: Text -> Scrypt -> ScryptEngine -> IO ()
runScrypt lbl scrypt sEngine = do
    stc <- defaultOneshotContext sEngine scrypt
    runThread lbl stc

-- | Read from stdin and run scrypts in the current thread
--
-- You may want to wrap this in `async` or `forkIO`
runInteractive :: Text -> ScryptEngine -> IO ()
runInteractive lbl sEngine = do
    stc <- defaultInteractiveContext sEngine
    runThread lbl stc
