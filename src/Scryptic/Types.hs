{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -Wall #-}
module Scryptic.Types where

import Control.Concurrent.STM
import Control.Exception as E
import Control.Lens
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Monoid
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import Data.Data

type MkFinalizer = IO () -> STM ()

data InputMap a = InputMap
      { _imWatching :: IntMap (a->IO ())
      , _imWaiting  :: IntMap (a->IO ())
      } deriving (Typeable)

$(makeLenses ''InputMap)

emptyInputMap :: InputMap a
emptyInputMap = InputMap mempty mempty

mapBoth :: (IntMap (a->IO()) -> IntMap (a->IO()))
        -> InputMap a -> InputMap a
mapBoth f im = im & imWatching %~ f & imWaiting %~ f

-- inputs/outputs from the Scryptic point of view; an Input corresponds
-- to output from an application.
data Input  = forall a. (Typeable a, Read a, Show a, Ord a)
            => Input (TVar (InputMap a))
    deriving (Typeable)
data Output = forall a. (Typeable a, Read a)
            => Output TypeRep (a -> IO ()) MkFinalizer
    deriving (Typeable)

newtype Key = Key {unKey :: Text}
    deriving (Eq, Show, Ord, Read)

-- kind of jenky, but 'key' is too useful an identifier to make it an iso
$(makeLensesWith (isoRules & lensIso .~ (const $ Just "keyy")) ''Key)

data ScryptHooks = ScryptHooks
    { _inpMap :: Map Key Input
    , _outMap :: Map Key Output
    }

$(makeLenses ''ScryptHooks)

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
    , " key `", k^.from keyy.to Text.unpack, "'" ] )

data BlockConfig = BlockConfig
    { _bcTitle :: Last String }
    deriving (Eq, Show)

instance Monoid BlockConfig where
    mempty = BlockConfig mempty
    BlockConfig tL `mappend` BlockConfig tR = BlockConfig (tL<>tR)

data BadCast = BadCast Text deriving (Eq, Show, Data, Typeable)
instance E.Exception BadCast

data BadKey = BadKey Text deriving (Eq, Show, Data, Typeable)
instance E.Exception BadKey

$(makeLenses ''BlockConfig)
