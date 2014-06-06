{-# OPTIONS -Wall #-}

module Scryptic (
  module M,
  parseScript,
  parseFile,
  defaultScryptOpts,
) where

import Scryptic.Monad as M
import Scryptic.Types as M
import Scryptic.Runtime as M
import Scryptic.Runtime.Options
import Scryptic.Language.AbsScrypt
import Scryptic.Language.ErrM
import Scryptic.Language.LayoutScrypt
import Scryptic.Language.ParScrypt

import Control.Applicative
import Data.Text as Text
import Data.Text.IO as Text

parseScript :: Text -> Either String Scrypt
parseScript inp = case pScrypt . rLex $ Text.unpack inp of
    Bad s -> Left s
    Ok tree -> Right tree
  where
    rLex = resolveLayout True . myLexer

parseFile :: FilePath -> IO (Either String Scrypt)
parseFile file = parseScript <$> Text.readFile file
