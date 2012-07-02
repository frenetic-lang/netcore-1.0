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
-- /testsuite/Frenetic/ArbitraryAPI                                           --
--                                                                            --
-- $Id$ --
--------------------------------------------------------------------------------

module Tests.Frenetic.Switches.ArbitraryOpenFlow where
import Test.QuickCheck
import Nettle.OpenFlow
import Frenetic.NetCore.Compiler
import Frenetic.Switches.OpenFlow
import Frenetic.Compat hiding (Action)

instance Arbitrary Match where
  arbitrary = do
    inPort_v <- arbitrary
    srcEthAddress_v <- arbitrary
    dstEthAddress_v <- arbitrary
    vLANID_v <- arbitrary
    vLANPriority_v <- arbitrary
    ethFrameType_v <- arbitrary
    ipTypeOfService_v <- arbitrary
    ipProtocol_v <- arbitrary
    srcIPAddress_v <- srcIP
    dstIPAddress_v <- dstIP
    srcTransportPort_v <- arbitrary
    dstTransportPort_v <- arbitrary
    return $ Match {
        inPort = inPort_v
      , srcEthAddress = srcEthAddress_v
      , dstEthAddress = dstEthAddress_v
      , vLANID = vLANID_v
      , vLANPriority = vLANPriority_v
      , ethFrameType = ethFrameType_v
      , ipTypeOfService = ipTypeOfService_v
      , matchIPProtocol = ipProtocol_v
      , srcIPAddress = srcIPAddress_v
      , dstIPAddress = dstIPAddress_v
      , srcTransportPort = srcTransportPort_v
      , dstTransportPort = dstTransportPort_v
      }
      where srcIP = suchThat arbitrary f
            dstIP = suchThat arbitrary f
            f (addr, prefixLength) = prefixLength <= 32

instance Arbitrary EthernetAddress where
  arbitrary = do
    w1 <- arbitrary
    w2 <- arbitrary
    w3 <- arbitrary
    w4 <- arbitrary
    w5 <- arbitrary
    w6 <- arbitrary
    return $ ethernetAddress w1 w2 w3 w4 w5 w6

instance Arbitrary IPAddress where
  arbitrary = do
    addr <- arbitrary
    return $ IPAddress addr

instance Arbitrary Action where
-- TODO: generate modification actions
  arbitrary = do
    p <- arbitrary
    return $ SendOutPort p

instance Arbitrary PseudoPort where
  arbitrary = do 
    let maxLenToSendController = 65535
    p <- arbitrary
    oneof [ return $ PhysicalPort p
          , return Flood
-- TODO: add other kinds of forwarding
--           , return InPort
--           , return AllPhysicalPorts
--           , return $ ToController maxLenToSendController
--           , return NormalSwitching
--           , return WithTable
          ]

instance (Arbitrary ptrn, Arbitrary actn) => Arbitrary (Classifier ptrn actn) where
  arbitrary = do 
    l <- listOf arbitrary
    return $ Classifier l

instance Arbitrary (PatternImpl OpenFlow) where
  arbitrary = do
    v <- arbitrary
    return (toOFPat v)

instance Arbitrary (ActionImpl OpenFlow) where
  arbitrary = do
    v <- arbitrary
    return (toOFAct v)