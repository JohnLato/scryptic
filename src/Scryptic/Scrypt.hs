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
  = Wait Key
  | Write Key String
  | WriteSync Key String Key{-sync-}
  | Watch Key
  | Unwatch Key
  | Sleep Double
  | SetOpt ScryptOptAdj String

deriving instance Show ScryptStatement
