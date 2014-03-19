{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -Wall #-}
module Scryptic.Scrypt (
  getBlocks,
  sKey,
  sOptVal,
  sNum,
  identS,
  titleOpt,
  nameQual,
  mkConfig,
  module Scryptic.Language.AbsScrypt,
) where

import Scryptic.Types
import Scryptic.RuntimeOptions
import Scryptic.Language.AbsScrypt
import Control.Lens
import Data.List (intercalate)
import Data.Monoid

$(makeIso ''Ident)
$(makeIso ''BlockOpt)
$(makeIso ''NameQual)

{-
 - bnfc is currently broken with bytestrings+layout, so
 - using String parser until that's fixed. (probably by me :o )

-- alex assumes UTF8, so we do too.
identS :: Iso' Ident String
identS = from $ packed . iso Text.encodeUtf8 Text.decodeUtf8 . ident
-}

identS :: Iso' Ident String
identS = from ident

getBlocks :: Scrypt -> [Block]
getBlocks (OneBlock block) = [block]
getBlocks (MultiBlock blocks) = blocks

sKey :: SKey -> Key
sKey (SKey quals nm) = Key $ intercalate "."
    $ quals^.mapping (from nameQual . identS) ++ [nm^.identS]

sOptVal :: Read a => SOptVal -> Maybe a
sOptVal sov = case sov of
    SOptNum num -> mRead $ show $ sNum num
    SOptStr str -> mRead $ "\\\"" ++ str ++ "\\\""
    SOptNone -> mRead "()"

sNum :: SNum -> Double
sNum n = case n of
    IntNum int -> fromIntegral int
    DubNum d   -> d

mkConfig :: BlockOpt -> BlockConfig
mkConfig (TitleOpt idnt) = bcTitle.unwrapped._Just.from identS .~ idnt $ mempty
