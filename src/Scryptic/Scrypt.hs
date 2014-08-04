{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS -Wall #-}
module Scryptic.Scrypt (
  getBlocks,
  sKey,
  sOptVal,
  sVal,
  sNum,
  sNumD,
  ident,
  titleOpt,
  nameQual,
  mkConfig,

  CompiledExpr(..),
  evalExpr,
  module Scryptic.Language.AbsScrypt,
) where

import Scryptic.Types
import Scryptic.Language.AbsScrypt
import Control.Applicative
import Control.Lens
import Data.Monoid
import qualified Data.Text as Text
import Data.Typeable

#ifndef MIN_VERSION_lens
#define MIN_VERSION_lens(x,y,z) 1
#endif

#if MIN_VERSION_lens(4,3,0)
#define MKISO makePrisms
#else
#define MKISO makeIso
#endif


$(MKISO ''Ident)
$(MKISO ''BlockOpt)
$(MKISO ''NameQual)

#if MIN_VERSION_lens(4,3,0)
ident :: Iso' Ident String
ident = _Ident

nameQual :: Iso' NameQual Ident
nameQual = _NameQual

titleOpt :: Iso' BlockOpt Ident
titleOpt = _TitleOpt
#endif

{-
 - bnfc is currently broken with bytestrings+layout, so
 - using String parser until that's fixed. (probably by me :o )

-- alex assumes UTF8, so we do too.
ident :: Iso' Ident String
ident = from $ packed . iso Text.encodeUtf8 Text.decodeUtf8 . ident
-}

getBlocks :: Scrypt -> [Block]
getBlocks (OneBlock block) = [block]
getBlocks (MultiBlock blocks) = blocks

sKey :: SKey -> Key
#if MIN_VERSION_lens(4,3,0)
sKey (SKey quals nm) = Key $ Text.intercalate "."
    $ map (^. nameQual . ident . to Text.pack) quals
      ++ [nm ^. ident . to Text.pack]
#else
sKey (SKey quals nm) = Key $ Text.intercalate "."
    $ map (^. from nameQual . from ident . to Text.pack) quals
      ++ [nm ^. from ident . to Text.pack]
#endif

sOptVal :: Typeable a => SOptVal -> Maybe a
sOptVal sov = case sov of
    SOptVal sval -> sVal sval
    SOptNone -> cast ()

sVal :: Typeable a => SVal -> Maybe a
sVal sv = case sv of
    SValNum num -> sNum num
    SValStr str -> cast str

sNum :: (Typeable a) => SNum -> Maybe a
sNum n = case n of
    IntNum int -> cast int
                  <|> cast (fromIntegral int::Int)
                  <|> cast (fromIntegral int::Double)
    DubNum d   -> cast d

sNumD :: SNum -> Double
sNumD n = case n of
    IntNum int -> fromIntegral int
    DubNum d   -> d


mkConfig :: BlockOpt -> BlockConfig
#if MIN_VERSION_lens(4,3,0)
mkConfig (TitleOpt idnt) = bcTitle._Wrapped._Just.from ident .~ idnt $ mempty
#elif MIN_VERSION_lens(4,0,0)
mkConfig (TitleOpt idnt) = bcTitle._Wrapped._Just.ident .~ idnt $ mempty
#else
mkConfig (TitleOpt idnt) = bcTitle.unwrapped._Just.ident .~ idnt $ mempty
#endif

data CompiledExpr = CompiledExpr
    { ceKey :: Key
    , cePred :: forall a. (Typeable a, Eq a, Ord a) => a -> Maybe Bool
    }
  | CmpAndExpr CompiledExpr CompiledExpr
  | CmpOrExpr CompiledExpr CompiledExpr

getCmp :: (Eq a, Ord a) => CmpOp -> a -> a -> Bool
getCmp EqOp  = (==)
getCmp NEqOp = (/=)
getCmp LtOp  = (<)
getCmp GtOp  = (>)
getCmp LEqOp = (<=)
getCmp GEqOp = (>=)

evalExpr :: Expr -> Either String CompiledExpr
evalExpr (KeyExpr key) = Right $ CompiledExpr (sKey key) (const $ Just True)

evalExpr (CmpExpr (KeyExpr key) cmpOp (RConstExpr val)) = Right $
    CompiledExpr (sKey key) $ \a -> getCmp cmpOp a <$> sVal val
evalExpr (CmpExpr (RConstExpr val) cmpOp (KeyExpr key)) = Right $
    CompiledExpr (sKey key) $ \a -> getCmp cmpOp <$> sVal val <*> pure a
evalExpr e@(CmpExpr _ _ _) = Left $ "mal-formed expression, expecting comparison of key to value: " ++ show e

evalExpr (RCmpExpr l cmpL (sKey -> key) cmpR r) = Right $
    CompiledExpr key $ \a ->
        (&&) <$> (getCmp cmpL <$> sVal l <*> pure a) <*> (getCmp cmpR a <$> sVal r)

evalExpr (AndExpr l r)  = CmpAndExpr <$> evalExpr l <*> evalExpr r
evalExpr (OrExpr l r)   = CmpOrExpr <$> evalExpr l <*> evalExpr r

evalExpr (RConstExpr{}) = Left $ "mal-formed expression, constant not allowed"
