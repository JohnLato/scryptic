{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Functor.Identity
import Data.Typeable
import Text.Parsec (Parsec, choice, Stream)

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

getValuedOptionSetter :: (Stream s Identity t)
                      => String -> String
                      -> Parsec s u ScryptOptAdj
getValuedOptionSetter key val = choice
    [ getValuedOptionSetter' boolOptionMap key val ]
-- for now there are only boolean options, but if I add more
-- (e.g. verbosity) this will handle it easily

getValuedOptionSetter' :: forall s a u t. (Typeable a, Read a
                           , Stream s Identity t)
                       => Map String (ReifiedLens' ScryptOpt a)
                       -> String -> String
                       -> Parsec s u ScryptOptAdj
getValuedOptionSetter' optMap key val = case Map.lookup key optMap of
    Nothing -> fail $ "scryptic: no option named " ++ key
    Just theLens -> case mRead val of
        Just b  -> return . ScryptOptAdj $ (UNLENS theLens) .~ b
        Nothing -> failKeyVal key val (show $ typeOf (undefined :: a))

failKeyVal :: Monad m => String -> String -> String -> m a
failKeyVal key val msg = fail $ concat
    [ "value <", val, "> for key <",key,"> doesn't appear to be a ", msg ]

mRead :: Read a => String -> Maybe a
mRead str = case reads str of
        ((b,_):_) -> Just b
        [] -> Nothing
