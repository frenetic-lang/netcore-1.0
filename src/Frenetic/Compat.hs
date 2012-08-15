module Frenetic.Compat
  ( Packet (..)
  , Transmission (..)
  -- * Implementation
  , FreneticImpl (..)
  )  where

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

-- |'FreneticImpl a' is a family of related abstract types that define a
-- back-end for Frenetic.
class (Show (PatternImpl a),
       Show (ActionImpl a),
       Matchable (PatternImpl a),
       Eq (PacketImpl a),
       Eq (ActionImpl a),
       Eq (PatternImpl a))
       => FreneticImpl a where

  data PacketImpl a
  -- |'PatternImpl a' represents switch-level patterns, which may not be
  -- as expressive as Frenetic's pattern language.
  data PatternImpl a
  -- |'ActionImpl a' represents switch-level actions. All Frenetic actions
  -- (@Action@) may not be realizable on switches.
  data ActionImpl a

  -- |'ptrnMatchPkt pkt pat' is 'True' if 'pat' matches 'pkt'.
  ptrnMatchPkt :: PacketImpl a -> PatternImpl a -> Bool
  toPacket :: PacketImpl a -> Maybe Packet

  fromPattern :: Pattern -> PatternImpl a
  toPattern :: PatternImpl a -> Pattern

  actnDefault :: ActionImpl a
  actnController :: ActionImpl a
  actnTranslate :: Action -> ActionImpl a

  actnControllerPart :: ActionImpl a -> Switch -> PacketImpl a -> IO ()

