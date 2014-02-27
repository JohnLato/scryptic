{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Scryptic.Scrypt
where

import Scryptic.Types
import Scryptic.RuntimeOptions

type Scrypt = [ScryptStatement]

data ScryptStatement
  = Trigger Key
  | TriggerSync Key Key{-sync-}
  | Wait Key
  | Write Key String
  | Watch Key
  | Unwatch Key
  | Sleep Double
  | SetOpt ScryptOptAdj String
  | Title String

deriving instance Show ScryptStatement

-- check if a line alters control flow
isFlowLine :: ScryptStatement -> Bool
isFlowLine (Title{}) = True
isFlowLine _ = False
