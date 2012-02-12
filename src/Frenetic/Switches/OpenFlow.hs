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

--
--
--

classifierToRules :: Classifier OFMatch.Match OFAction.ActionSequence -> [Rule]
classifierToRules = map (\(p, a) -> Rule p a) . unpack
                    
ethToWord48 (EthernetAddress a b c d e f) =
    LargeKey a (LargeKey b (LargeKey c (LargeKey d (LargeKey e f))))
             
word48ToEth (LargeKey a (LargeKey b (LargeKey c (LargeKey d (LargeKey e f))))) =
    EthernetAddress a b c d e f

prefixToIPAddressPrefix :: Prefix Word32 -> IPAddress.IPAddressPrefix
prefixToIPAddressPrefix (Prefix (Wildcard x m)) =
    (IPAddress.IPAddress x, fromIntegral $ sum [1 | i <- [0 .. 31], not $ testBit m i])

instance Pattern IPAddress.IPAddressPrefix where
  top = IPAddress.defaultIPPrefix
  intersect = IPAddress.intersect

instance Actionable OFAction.ActionSequence where
    actController = sendToController 0
    actDefault = sendToController 0
    actTranslate s = map (SendOutPort . PhysicalPort) $ Set.toList s

-- Gonna need this for embedded patterns
deriving instance Typeable OFMatch.Match

instance Pattern OFMatch.Match where
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

instance Patternable OFMatch.Match where
    patOverapprox Dl_src w = top { srcEthAddress = fmap word48ToEth $ overapprox w }
    patOverapprox Dl_dst w = top { dstEthAddress = fmap word48ToEth $ overapprox w }
    patOverapprox Dl_typ w = top { ethFrameType = overapprox w }
    patOverapprox Dl_vlan w = top { vLANID = overapprox w }
    patOverapprox Dl_vlan_pcp w = top { vLANPriority = overapprox w }
    patOverapprox Nw_src w = top { srcIPAddress = prefixToIPAddressPrefix $ overapprox w  }
    patOverapprox Nw_dst w = top { dstIPAddress = prefixToIPAddressPrefix $ overapprox w  }
    patOverapprox Nw_proto w = top { ipProtocol = overapprox w }
    patOverapprox Nw_tos w = top { ipTypeOfService = overapprox w }
    patOverapprox Tp_src w = top { srcTransportPort = overapprox w }
    patOverapprox Tp_dst w = top { dstTransportPort = overapprox w }

    patInport p = top { inPort = Just p }

    patUnderapprox Dl_src w v = do p <- underapprox w v
                                   return $ top { srcEthAddress = fmap word48ToEth $ p }
    patUnderapprox Dl_dst w v = do p <- underapprox w v
                                   return $ top { dstEthAddress = fmap word48ToEth $ p }
    patUnderapprox Dl_typ w v = do p <- underapprox w v
                                   return $ top { ethFrameType = p }
    patUnderapprox Dl_vlan w v = do p <- underapprox w v
                                    return $ top { vLANID = p }
    patUnderapprox Dl_vlan_pcp w v = do p <- underapprox w v
                                        return $ top { vLANPriority = p }
    patUnderapprox Nw_src w v = do p <- underapprox w v
                                   return $ top { srcIPAddress = prefixToIPAddressPrefix p }
    patUnderapprox Nw_dst w v = do p <- underapprox w v
                                   return $ top { dstIPAddress = prefixToIPAddressPrefix p }
    patUnderapprox Nw_proto w v = do p <- underapprox w v
                                     return $ top { ipProtocol = p }
    patUnderapprox Nw_tos w v = do p <- underapprox w v
                                   return $ top { ipTypeOfService = p }
    patUnderapprox Tp_src w v = do p <- underapprox w v
                                   return $ top { srcTransportPort = p }
    patUnderapprox Tp_dst w v = do p <- underapprox w v 
                                   return $ top { dstTransportPort = p }
