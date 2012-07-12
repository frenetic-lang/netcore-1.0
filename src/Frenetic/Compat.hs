--------------------------------------------------------------------------------
-- The Frenetic Project                                                       --
-- frenetic@frenetic-lang.org                                                 --
--------------------------------------------------------------------------------
-- Licensed to the Frenetic Project by one or more contributors. See the      --
-- NOTICE file distributed with this work for additional information          --
-- regarding copyright and ownership. The Frenetic Project licenses this      --
-- file to you under the following license.                                   --
--                                                                            --
-- Redistribution and use in source and binary forms, with or without         --
-- modification, are permitted provided the following conditions are met:     --
-- - Redistributions of source code must retain the above copyright           --
--   notice, this list of conditions and the following disclaimer.            --
-- - Redistributions of binaries must reproduce the above copyright           --
--   notice, this list of conditions and the following disclaimer in          --
--   the documentation or other materials provided with the distribution.     --
-- - The names of the copyright holds and contributors may not be used to     --
--   endorse or promote products derived from this work without specific      --
--   prior written permission.                                                --
--                                                                            --
-- Unless required by applicable law or agreed to in writing, software        --
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  --
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   --
-- LICENSE file distributed with this work for specific language governing    --
-- permissions and limitations under the License.                             --
--------------------------------------------------------------------------------
-- /src/Frenetic/Compat.hs                                                    --
--------------------------------------------------------------------------------

module Frenetic.Compat
  ( Packet (..)
  , Transmission (..)
  -- * Implementation
  , FreneticImpl (..)
  )  where

import Frenetic.Util
import Frenetic.NetCore.API
import qualified Data.List          as List
import           Data.Bits
import           Data.Word
import qualified Data.Set           as Set
import           Frenetic.Pattern

-- |Frenetic packets
data Packet = Packet {
    pktDlSrc :: Word48
  , pktDlDst :: Word48
  , pktDlTyp :: Word16
  , pktDlVlan :: Word16
  , pktDlVlanPcp :: Word8
  , pktNwSrc :: Word32
  , pktNwDst :: Word32
  , pktNwProto :: Word8
  , pktNwTos :: Word8
  , pktTpSrc :: Word16
  , pktTpDst :: Word16
  , pktInPort :: Port
  } deriving (Show, Eq, Ord)

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
  --
  --  @patOverapprox@ and @patUnderapprox@ must follow the laws in the
  -- Approx class. If the pattern is not a real underapproximation,
  -- @patUnderapprox@ must return Nothing.
  data PatternImpl a
  -- |'ActionImpl a' represents switch-level actions. All Frenetic actions
  -- (@Action@) may not be realizable on switches.
  data ActionImpl a

  -- |'ptrnMatchPkt pkt pat' is 'True' if 'pat' matches 'pkt'.
  ptrnMatchPkt :: PacketImpl a -> PatternImpl a -> Bool
  toPacket :: PacketImpl a -> Packet

  updatePacket :: PacketImpl a -> Packet -> PacketImpl a

  fromPatternOverapprox :: Pattern -> PatternImpl a
  fromPatternUnderapprox :: Packet -> Pattern -> Maybe (PatternImpl a)
  toPattern :: PatternImpl a -> Pattern

  actnDefault :: ActionImpl a
  actnController :: ActionImpl a
  actnTranslate :: Action -> ActionImpl a

