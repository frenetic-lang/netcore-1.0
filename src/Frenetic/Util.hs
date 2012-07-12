module Frenetic.Util
  ( newLift
  , newLift2
  , Set
  , Map
  , module Control.Concurrent.Chan
  ) where

import Control.Concurrent.Chan
import Data.Set (Set)
import Data.Map (Map)
import Control.Newtype

newLift :: (Newtype n o) => (o -> o) -> n -> n
newLift f = pack . f . unpack

newLift2 :: (Newtype n o) => (o -> o -> o) -> n -> n -> n
newLift2 f n1 n2 = pack (f (unpack n1) (unpack n2))
