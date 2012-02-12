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
-- /src/Network.hs                                                            --
-- OpenFlow classifiers                                                       --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE 
    NoMonomorphismRestriction,
    GADTs,
    TypeSynonymInstances,
    FlexibleInstances,
    MultiParamTypeClasses,
    StandaloneDeriving,
    DeriveDataTypeable
 #-}

module Frenetic.Switches.OpenFlow where

import           Control.Newtype

import           Data.Bits
import           Data.LargeWord
import qualified Data.Set                        as Set
import           Data.Typeable
import           Data.Word


import           Nettle.OpenFlow.Match           as OFMatch
import           Nettle.OpenFlow.Action          as OFAction
import qualified Nettle.IPv4.IPAddress           as IPAddress
import           Nettle.Ethernet.EthernetAddress
    
import           Frenetic.Pattern
import           Frenetic.Language
import           Frenetic.Compiler

--
--
--
    
data Rule = Rule OFMatch.Match [OFAction.Action]

instance Show Rule where
  show (Rule p a) = "Rule(" ++ show p ++ ", " ++ show a ++ ")"

classifierToRules :: Classifier OFMatch.Match OFAction.ActionSequence -> [Rule]
classifierToRules = map (\(p, a) -> Rule p a) . unpack
                    
ethToWord48 (EthernetAddress a b c d e f) =
    LargeKey a (LargeKey b (LargeKey c (LargeKey d (LargeKey e f))))
             
word48ToEth (LargeKey a (LargeKey b (LargeKey c (LargeKey d (LargeKey e f))))) =
    EthernetAddress a b c d e f

prefixToIPAddressPrefix :: Prefix Word32 -> IPAddress.IPAddressPrefix
prefixToIPAddressPrefix (Prefix (Wildcard x m)) =
    (IPAddress.IPAddress x, fromIntegral $ sum [1 | i <- [0 .. 31], not $ testBit m i])

ipAddressPrefixToPrefix :: IPAddress.IPAddressPrefix -> Prefix Word32
ipAddressPrefixToPrefix = undefined


instance Matchable IPAddress.IPAddressPrefix where
  top = IPAddress.defaultIPPrefix
  intersect = IPAddress.intersect

instance GAction OFAction.ActionSequence where
    actnController = sendToController 0
    actnDefault = sendToController 0
    actnTranslate s = map (SendOutPort . PhysicalPort) $ Set.toList s

-- Gonna need this for embedded patterns
deriving instance Typeable OFMatch.Match

instance Matchable OFMatch.Match where
  top = Match { 
          inPort = top,
          srcEthAddress = top,
          dstEthAddress = top,
          vLANID = top,
          vLANPriority = top,
          ethFrameType = top,
          ipTypeOfService = top,
          ipProtocol = top,
          srcIPAddress = top,
          dstIPAddress = top,
          srcTransportPort = top,
          dstTransportPort = top }
        
  intersect ofm1 ofm2 = 
      do inport <- intersect (inPort ofm1) (inPort ofm2)
         srcethaddress <- intersect (srcEthAddress ofm1) (srcEthAddress ofm2)
         dstethaddress <- intersect (dstEthAddress ofm1) (dstEthAddress ofm2)
         vlanid <- intersect (vLANID ofm1) (vLANID ofm2)
         vlanpriority <- intersect (vLANPriority ofm1) (vLANPriority ofm2)
         ethframetype <- intersect (ethFrameType ofm1) (ethFrameType ofm2)
         iptypeofservice <- intersect (ipTypeOfService ofm1) (ipTypeOfService ofm2)
         ipprotocol <- intersect (ipProtocol ofm1) (ipProtocol ofm2)
         srcipaddress <- intersect (srcIPAddress ofm1) (srcIPAddress ofm2)
         dstipaddress <- intersect (dstIPAddress ofm1) (dstIPAddress ofm2)
         srctransportport <- intersect (srcTransportPort ofm1) (srcTransportPort ofm2)
         dsttransportport <- intersect (dstTransportPort ofm1) (dstTransportPort ofm2)
         return $ Match { 
           inPort = inport,
           srcEthAddress = srcethaddress,
           dstEthAddress = dstethaddress,
           vLANID = vlanid,
           vLANPriority = vlanpriority,
           ethFrameType = ethframetype,
           ipTypeOfService = iptypeofservice,
           ipProtocol = ipprotocol,
           srcIPAddress = srcipaddress,
           dstIPAddress = dstipaddress,
           srcTransportPort = srctransportport,
           dstTransportPort = dsttransportport }


instance GPattern OFMatch.Match where
  ptrnOverapprox ptrn = top {
    srcEthAddress = fmap word48ToEth $ overapprox $ ptrnDlSrc ptrn,
    dstEthAddress = fmap word48ToEth $ overapprox $ ptrnDlDst ptrn,
    ethFrameType = overapprox $ ptrnDlTyp ptrn,
    vLANID = overapprox $ ptrnDlVlan ptrn,
    vLANPriority = overapprox $ ptrnDlVlanPcp ptrn,
    srcIPAddress = prefixToIPAddressPrefix $ overapprox $ ptrnNwSrc ptrn ,
    dstIPAddress = prefixToIPAddressPrefix $ overapprox $ ptrnNwDst ptrn ,
    ipProtocol = overapprox $ ptrnNwProto ptrn,
    ipTypeOfService = overapprox $ ptrnNwTos ptrn,
    srcTransportPort = overapprox $ ptrnTpSrc ptrn,
    dstTransportPort = overapprox $ ptrnTpDst ptrn, 
    inPort = ptrnInPort ptrn
    }
    
  ptrnUnderapprox pkt ptrn = do 
    ptrnDlSrc' <- underapprox (ptrnDlSrc ptrn) (pktDlSrc pkt)
    ptrnDlDst' <- underapprox (ptrnDlDst ptrn) (pktDlDst pkt)
    ptrnDlTyp' <- underapprox (ptrnDlTyp ptrn) (pktDlTyp pkt)
    ptrnDlVlan' <- underapprox (ptrnDlVlan ptrn) (pktDlVlan pkt)
    ptrnDlVlanPcp' <- underapprox (ptrnDlVlanPcp ptrn) (pktDlVlanPcp pkt)
    ptrnNwSrc' <- underapprox (ptrnNwSrc ptrn) (pktNwSrc pkt)
    ptrnNwDst' <- underapprox (ptrnNwDst ptrn) (pktNwDst pkt)
    ptrnNwProto' <- underapprox (ptrnNwProto ptrn) (pktNwProto pkt)
    ptrnNwTos' <- underapprox (ptrnNwTos ptrn) (pktNwTos pkt)
    ptrnTpSrc' <- underapprox (ptrnTpSrc ptrn) (pktTpSrc pkt)
    ptrnTpDst' <- underapprox (ptrnTpDst ptrn) (pktTpDst pkt)
    return $ top {
      srcEthAddress = fmap word48ToEth $ ptrnDlSrc',
      dstEthAddress = fmap word48ToEth $ ptrnDlDst',
      ethFrameType = ptrnDlTyp',
      vLANID = ptrnDlVlan',
      vLANPriority = ptrnDlVlanPcp',
      srcIPAddress = prefixToIPAddressPrefix $ ptrnNwSrc' ,
      dstIPAddress = prefixToIPAddressPrefix $ ptrnNwDst' ,
      ipProtocol = ptrnNwProto',
      ipTypeOfService = ptrnNwTos',
      srcTransportPort = ptrnTpSrc',
      dstTransportPort = ptrnTpDst',   
      inPort = ptrnInPort ptrn
      }

  ptrnInverse ptrn = Pattern {
    ptrnDlSrc     = inverseapprox $ fmap ethToWord48 $ srcEthAddress ptrn,
    ptrnDlDst     = inverseapprox $ fmap ethToWord48 $ dstEthAddress ptrn,
    ptrnDlTyp     = inverseapprox $ ethFrameType ptrn,
    ptrnDlVlan    = inverseapprox $ vLANID ptrn,
    ptrnDlVlanPcp = inverseapprox $ vLANPriority ptrn,
    ptrnNwSrc     = inverseapprox $ ipAddressPrefixToPrefix $ srcIPAddress ptrn,
    ptrnNwDst     = inverseapprox $ ipAddressPrefixToPrefix $ dstIPAddress ptrn,
    ptrnNwProto   = inverseapprox $ ipProtocol ptrn,
    ptrnNwTos     = inverseapprox $ ipTypeOfService ptrn,
    ptrnTpSrc     = inverseapprox $ srcTransportPort ptrn,
    ptrnTpDst     = inverseapprox $ dstTransportPort ptrn,
    ptrnInPort    = inPort ptrn
    }