module Scryptic.Pure (
  scryptInput,
  scryptOutput,
) where

import Scryptic.Types

import Control.Concurrent.STM
import Control.Lens

import Data.Monoid
import Data.Map as Map
import Data.Typeable

-- | Script an input to an application
scryptInput :: (Typeable a, Read a)
            => String -> (a -> IO ()) -> ScryptHooks
scryptInput key akt =
    let typeHint = case splitTyConApp (typeOf akt) of
            (_,aTyp:_) -> aTyp
            _ -> error $ "scryptInput: couldn't determine input type from " ++ show (typeOf akt)
    in mempty & outMap .~ Map.singleton key (Output typeHint akt Nothing)

-- | Scrypt an output from an application
scryptOutput :: (Typeable a, Read a, Show a)
             => String -> IO (a->IO(),ScryptHooks)
scryptOutput key = do
    ref <- newTVarIO (const $ return ())
    let scryptic = mempty & inpMap .~ Map.singleton key (Input ref)
        akt a = readTVarIO ref >>= ($ a)
    return (akt, scryptic)
