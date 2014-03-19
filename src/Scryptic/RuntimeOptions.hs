{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -Wall #-}
module Scryptic.RuntimeOptions (
    ScryptOpt(..),
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
import Data.Typeable

#ifndef MIN_VERSION_lens
#define MIN_VERSION_lens(x,y,z) 0
#endif

#if MIN_VERSION_lens(4,0,0)
#define LENS Lens
#define UNLENS runLens
#else
#define LENS ReifyLens
#define UNLENS reflectLens
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

newtype ScryptOptAdj = ScryptOptAdj { unSOA :: ScryptOpt -> ScryptOpt}
$(makeIso ''ScryptOptAdj)

instance Show (ScryptOptAdj) where
    show _ = "<<scryptOptAdj>>"

boolOptionMap :: Map String (ReifiedLens' ScryptOpt Bool)
boolOptionMap = Map.fromList
    [ ("trace", LENS soTrace)
    , ("dieOnError", LENS soDieOnErr)
    , ("debug", LENS soDebug)
    , ("dumpScrypts", LENS soDumpScrypts) ]

getValuedOptionSetter :: String -> String -> Either String ScryptOptAdj
getValuedOptionSetter key val = 
     getValuedOptionSetter' boolOptionMap key val 
-- for now there are only boolean options, but if I add more
-- (e.g. verbosity) it can be handled here (e.g. with `mplus`)

getValuedOptionSetter' :: forall a. (Typeable a, Read a)
                       => Map String (ReifiedLens' ScryptOpt a)
                       -> String -> String
                       -> Either String ScryptOptAdj
getValuedOptionSetter' optMap key val = case Map.lookup key optMap of
    Nothing -> Left $ "scryptic: no option named " ++ key
    Just theLens -> case mRead val of
        Just b  -> return . ScryptOptAdj $ (UNLENS theLens) .~ b
        Nothing -> failKeyVal key val (show $ typeOf (undefined :: a))

failKeyVal :: String -> String -> String -> Either String a
failKeyVal key val msg = Left $ concat
    [ "value <", val, "> for key <",key,"> doesn't appear to be a ", msg ]

mRead :: Read a => String -> Maybe a
mRead str = case reads str of
        ((b,_):_) -> Just b
        [] -> Nothing
