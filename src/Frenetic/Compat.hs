module Frenetic.Compat
  ( Transmission (..)
  )  where

import Frenetic.Switches.OpenFlow
import Frenetic.Common
import Frenetic.NetCore.Types
import qualified Data.List          as List
import           Data.Bits
import           Data.Word
import qualified Data.Set           as Set
import           Frenetic.Pattern


{-| Data that was sent. -}
data Transmission ptrn pkt = Transmission {
      trPattern :: ptrn,
      trSwitch :: Switch,
      trPkt :: pkt
    } deriving (Eq)

