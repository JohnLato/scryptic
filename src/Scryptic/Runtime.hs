{-# OPTIONS -Wall #-}
module Scryptic.Runtime (
  ScryptEngine,
  startScryptEngine,
  joinScryptEngine,
  runScrypt,
) where

import Scryptic.Runtime.Engine
import Scryptic.Scrypt

runScrypt :: String -> Scrypt -> ScryptEngine -> IO ()
runScrypt lbl scrypt sEngine = do
    stc <- defaultOneshotContext sEngine scrypt
    runThread lbl stc
