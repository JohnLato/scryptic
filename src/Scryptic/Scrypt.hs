{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Scryptic.Scrypt
where

import Scryptic.Types
import Scryptic.RuntimeOptions

type Scrypt = [ScryptBlock]

data ScryptBlock
  = ScryptBlock [ScryptStatement]
  | TitledBlock String [ScryptStatement]
  deriving (Show)

data ScryptStatement
  = Trigger Key
  | TriggerSync Key Key{-sync-}
  | Wait Key
  | Write Key String
  | Watch Key
  | Unwatch Key
  | Sleep Double
  | SetOpt ScryptOptAdj String

deriving instance Show ScryptStatement
