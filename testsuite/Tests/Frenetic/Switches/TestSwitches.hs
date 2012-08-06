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
-- * Redistributions of source code must retain the above copyright           --
--   notice, this list of conditions and the following disclaimer.            --
-- * Redistributions of binaries must reproduce the above copyright           --
--   notice, this list of conditions and the following disclaimer in          --
--   the documentation or other materials provided with the distribution.     --
-- * The names of the copyright holds and contributors may not be used to     --
--   endorse or promote products derived from this work without specific      --
--   prior written permission.                                                --
--                                                                            --
-- Unless required by applicable law or agreed to in writing, software        --
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  --
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   --
-- LICENSE file distributed with this work for specific language governing    --
-- permissions and limitations under the License.                             --
--------------------------------------------------------------------------------
-- /testsuite/Frenetic/TestSwitches                                           --
--                                                                            --
-- $Id$ --
--------------------------------------------------------------------------------

module Tests.Frenetic.Switches.TestSwitches where

import qualified Data.Set as Set
import Data.Word
import Data.Bits
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Property (Property, morallyDubiousIOProperty)
import Test.QuickCheck.Text
import Frenetic.Compat
import Tests.Frenetic.ArbitraryCompat
import Frenetic.Pattern
import Frenetic.NetCore.Compiler
import Frenetic.Switches.OpenFlow
import Tests.Frenetic.Switches.ArbitraryOpenFlow
import Nettle.OpenFlow hiding (match)
import qualified Nettle.IPv4.IPPacket as IP
import Frenetic.NetCore.Types hiding (ethernetAddress)

switchTests = $(testGroupGenerator)

prop_fromPattern_toPattern :: PatternImpl OpenFlow -> Bool
prop_fromPattern_toPattern sptrn =
  sptrn == (fromPattern $ toPattern sptrn)

prop_fromPattern_toPattern_match :: PatternImpl OpenFlow -> Bool
prop_fromPattern_toPattern_match sptrn =
  p1 `match` p2
    where
      p1 = toPattern sptrn
      p2 = toPattern p2'
      p2' :: PatternImpl OpenFlow
      p2' = fromPattern $ toPattern sptrn

prop_ipAddressPrefix :: Word32 -> Word8 -> Bool
prop_ipAddressPrefix ip len_in = prefix == idPrefix
  where
    prefix = (IPAddress ip, len)
    len = len_in `mod` 32
    idPrefix = prefixToIPAddressPrefix $ ipAddressPrefixToPrefix prefix


-- The following tests pinpoint values that have failed at some
-- point in the past.
case_fromPattern_toPattern_regression_1 = do
  let p = toOFPat $ Match {
      inPort = Just 27
    , srcEthAddress = Nothing
    , dstEthAddress = Just (ethernetAddress 7 43 42 48 39 35)
    , vLANID = Just 0
    , vLANPriority = Just 40
    , ethFrameType = Nothing
    , ipTypeOfService = Nothing
    , matchIPProtocol = Nothing
    , srcIPAddress = (IPAddress 41,7)
    , dstIPAddress = (IPAddress 11,32)
    , srcTransportPort = Just 28
    , dstTransportPort = Nothing
    }
  p @=? (fromPattern $ toPattern p)

case_exact_regression_1 = p @=? (toPattern p')
  where
    p :: Pattern
    p' :: PatternImpl OpenFlow
    p = top{ptrnNwSrc = Prefix 0x5000001 0}
    p' = fromPattern p

-- case_OFMatch_fail_1 = (matches (0, ethFrame) match) @=? True
--   where
--     match = Match {
--         inPort = Nothing
--       , srcEthAddress = Nothing
--       , dstEthAddress = Nothing
--       , vLANID = Nothing
--       , vLANPriority = Nothing
--       , ethFrameType = Nothing
--       , ipTypeOfService = Nothing
--       -- , matchIPProtocol = Nothing
--       , srcIPAddress = (IPAddress 0, 0)
--       , dstIPAddress = (IPAddress 0, 0)
--       , srcTransportPort = Nothing
--       , dstTransportPort = Nothing
--       }
--     ethFrame :: EthernetHeader :*: EthernetBody :*: HNil
--     ethFrame = ethHeader .*. ethBody .*. HNil
--     ethHeader = EthernetHeader {
--         destMACAddress = 0
--       , sourceMACAddress = 0
--       , typeCode = 0
--       }
--     ethBody = IPInEthernet [ipHeader, ipBody]
--     ipHeader = IP.IPHeader {
--         IP.ipSrcAddress = IPAddress 1
--       , IP.ipDstAddress = IPAddress 2
--       , IP.ipProtocol = 0
--       , IP.headerLength = 32
--       , IP.totalLength = 32
--       , IP.dscp = 0
--       }
--     ipBody = IP.UninterpretedIPBody 0

case_prefix_1 = (Prefix 0 0) @=? p1
  where
    p1 = ipAddressPrefixToPrefix (IPAddress 0, 0)

case_prefix_2 = (Prefix 0xFFFF0000 16) @=? p1
  where
    p1 = ipAddressPrefixToPrefix (IPAddress 0xFFFF0000, 16)

case_prefix_3 = (IPAddress 0xFFFF0000, 16) @=? p1
  where
    p1 = prefixToIPAddressPrefix $ Prefix 0xFFFF0000 16

case_ipAddressPrefix_1 = prefix @=? idPrefix
  where
    prefix = (IPAddress 0, 0)
    idPrefix = prefixToIPAddressPrefix $ ipAddressPrefixToPrefix prefix

