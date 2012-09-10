module Tests.Frenetic.Switches.ArbitraryOpenFlow where

import Nettle.IPv4.IPAddress
import Test.QuickCheck
import Nettle.OpenFlow
import Frenetic.NetCore.Compiler
import Frenetic.Switches.OpenFlow
import Frenetic.Compat
import Frenetic.NetCore.Types hiding (Action, PseudoPort)

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
    srcIPAddress_v <- arbitrary
    dstIPAddress_v <- arbitrary
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

instance Arbitrary IPAddressPrefix where
  arbitrary = do
    addr <- arbitrary
    len <- oneof $ map return [0 .. 32 ]
    return (IPAddressPrefix addr len)

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
