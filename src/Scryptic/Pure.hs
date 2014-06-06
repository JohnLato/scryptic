{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -Wall #-}
module Scryptic.Pure (
  scryptInput,
  scryptOutput,
) where

import Scryptic.Types

import Control.Concurrent.STM
import Control.Lens
import Control.Monad

import Data.Char (isSpace)
import Data.Monoid
import Data.Map as Map
import Data.Typeable

-- | Script an input to an application
--
-- this function returns a new IO action that can be run to de-register
-- the input.
scryptInput :: (Typeable a, Read a)
            => String -> (a -> IO ()) -> IO (IO(), ScryptHooks)
scryptInput (mkKey -> key) akt = do
    let typeHint = case splitTyConApp (typeOf akt) of
            (_,aTyp:_) -> aTyp
            _ -> error $ "scryptInput: couldn't determine input type from " ++ show (typeOf akt)

    cleanRef <- newTVarIO (return ())
    let derefAkt = join (readTVarIO cleanRef)
        registerDeref = writeTVar cleanRef
    return $ (derefAkt, mempty & outMap .~ Map.singleton key (Output typeHint akt registerDeref))

-- | Scrypt an output from an application
scryptOutput :: (Typeable a, Read a, Show a, Ord a)
             => String -> IO (a->IO(),ScryptHooks)
scryptOutput (mkKey -> key) = do
    ref <- newTVarIO (const $ return ())
    let scryptic = mempty & inpMap .~ Map.singleton key (Input ref)
        akt a = readTVarIO ref >>= ($ a)
    return (akt, scryptic)

-- current rules are that we convert spaces to underscores
mkKey :: String -> Key
mkKey keyStr = Key $ Prelude.filter (not . isSpace) keyStr
