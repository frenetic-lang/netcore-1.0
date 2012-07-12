module Frenetic.Slices.Slice
  ( Slice (..)
  , Loc (..)
  ) where

import Data.Word
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Frenetic.NetCore.API as NC

type Switch = Word64
type Port = Word16

-- |Fully qualified port locations
data Loc = Loc Switch Port deriving (Show, Eq)

data Slice = Slice {
  -- |Ports internal to the slice
  internalPorts :: Set.Set Loc
  -- |External ports, and restrictions on inbound packets
, ingress :: Map.Map Loc NC.Predicate
  -- |External ports, and restrictions on outbound packets
, egress  :: Map.Map Loc NC.Predicate
}
