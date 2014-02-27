{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Scryptic.Types where

import Control.Concurrent.STM
import Control.Lens
import Data.Map (Map)
import Data.Monoid
import qualified Data.Map as Map
import Data.Typeable

type MkFinalizer = Maybe (IO () -> IO ())

-- inputs/outputs from the Scryptic point of view; an Input corresponds
-- to output from an application.
data Input  = forall a. (Typeable a, Read a, Show a)
            => Input (TVar (a->IO ())) MkFinalizer
    deriving (Typeable)
data Output = forall a. (Typeable a, Read a)
            => Output TypeRep (a -> IO ()) MkFinalizer
    deriving (Typeable)

type Key = String

data ScryptHooks = ScryptHooks
    { _inpMap :: Map Key Input
    , _outMap :: Map Key Output
    }

$(makeLenses ''ScryptHooks)

inputFinalizer :: IndexPreservingLens' Input MkFinalizer
inputFinalizer = iplens (\(Input _ f) -> f) (\(Input a _) f -> Input a f)

outputFinalizer :: IndexPreservingLens' Output MkFinalizer
outputFinalizer =
  iplens (\(Output _ _ f) -> f) (\(Output rep a _) f -> Output rep a f)


-- Lens ix sucks for map lookups.
atKey :: (Ord key, Conjoined p, Gettable f)
      => key -> p (Maybe a) (f (Maybe a))
      -> p (Map key a) (f (Map key a))
atKey key = to (Map.lookup key)

instance Monoid ScryptHooks where
    mempty = ScryptHooks Map.empty Map.empty
    (ScryptHooks in1 out1) `mappend` (ScryptHooks in2 out2) =
        -- input/output labels are flipped so it makes sense to the
        -- user.
        ScryptHooks (merge "output" in1 in2) (merge "input" out1 out2)

merge :: String -> Map Key a -> Map Key a -> Map Key a
merge lbl = Map.unionWithKey (\k _ _ -> error $ concat
    [ "Scryptic: duplicate " , lbl
    , " key `", k, "'" ] )
