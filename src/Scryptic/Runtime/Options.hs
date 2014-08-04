{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -Wall #-}
module Scryptic.Runtime.Options (
    ScryptOpt(..),
    unSOA,
    defaultScryptOpts,
    debugScryptOpts,

    -- ** lenses
    soTrace,
    soDieOnErr,
    soDebug,
    soDumpScrypts,

    getValuedOptionSetter,
    -- ** really not for external use...
    ScryptOptAdj(..),
    scryptOptAdj,
    mRead,
) where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable

#ifndef MIN_VERSION_lens
#define MIN_VERSION_lens(x,y,z) 1
#endif

#if MIN_VERSION_lens(4,3,0)
#define MKISO makeLenses
#define SCRYPTOPTADJ _scryptOptAdj
#else
#define MKISO makeIso
#define SCRYPTOPTADJ unSOA
#endif

data ScryptOpt = ScryptOpt
    { _soTrace :: Bool
    , _soDieOnErr :: Bool
    , _soDebug :: Bool
    , _soDumpScrypts :: Bool
    } deriving (Eq, Show)

$(makeLenses ''ScryptOpt)

defaultScryptOpts :: ScryptOpt
defaultScryptOpts = ScryptOpt False True False False

debugScryptOpts :: ScryptOpt
debugScryptOpts = ScryptOpt True False True True

newtype ScryptOptAdj = ScryptOptAdj { SCRYPTOPTADJ :: ScryptOpt -> ScryptOpt}
$(MKISO ''ScryptOptAdj)

#if MIN_VERSION_lens(4,3,0)
unSOA :: ScryptOptAdj -> ScryptOpt -> ScryptOpt
unSOA = SCRYPTOPTADJ
#endif


instance Show (ScryptOptAdj) where
    show _ = "<<scryptOptAdj>>"

boolOptionMap :: Map Text (ReifiedLens' ScryptOpt Bool)
boolOptionMap = Map.fromList
    [ ("trace", Lens soTrace)
    , ("dieOnError", Lens soDieOnErr)
    , ("debug", Lens soDebug)
    , ("dumpScrypts", Lens soDumpScrypts) ]

getValuedOptionSetter :: Text -> String -> Either Text ScryptOptAdj
getValuedOptionSetter key val = 
     getValuedOptionSetter' boolOptionMap key val 
-- for now there are only boolean options, but if I add more
-- (e.g. verbosity) it can be handled here (e.g. with `mplus`)

getValuedOptionSetter' :: forall a. (Typeable a, Read a)
                       => Map Text (ReifiedLens' ScryptOpt a)
                       -> Text -> String
                       -> Either Text ScryptOptAdj
getValuedOptionSetter' optMap key val = case Map.lookup key optMap of
    Nothing -> Left $ "scryptic: no option named " <> key
    Just theLens -> case mRead val of
        Just b  -> return . ScryptOptAdj $ (runLens theLens) .~ b
        Nothing -> failKeyVal key val (show $ typeOf (undefined :: a))

failKeyVal :: Text -> String -> String -> Either Text a
failKeyVal key val msg = Left $ Text.concat
    [ "value <", Text.pack val, "> for key <",key
    ,"> doesn't appear to be a ", Text.pack msg ]

mRead :: Read a => String -> Maybe a
mRead str = case reads str of
        ((b,_):_) -> Just b
        [] -> Nothing
