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

import Data.Map
import Data.Bits
import Nettle.OpenFlow.Match
import Nettle.OpenFlow.Action
import qualified Nettle.OpenFlow.Packet as Pack
import Nettle.OpenFlow.Match as OFMatch
import Nettle.OpenFlow.Action as OFAction
import Nettle.IPv4.IPAddress as IPAddress
import Nettle.Ethernet.EthernetAddress
import Data.Set as Set
import Data.List as List
import Data.LargeWord
import Data.Typeable
    
import qualified Frenetic.Pattern as P
import Frenetic.Language
import Frenetic.Compiler
    
type Pattern = Match

type Actions = [Action]

data Rule = Rule Pattern Frenetic.Switches.OpenFlow.Actions

instance Show Rule where
  show (Rule p a) = "Rule(" ++ show p ++ ", " ++ show a ++ ")"

data Classifier = Classifier [Rule]

data Configuration = Configuration (Map Switch Classifier)

instance (Eq a) => P.Pattern (Maybe a) where
  top = Nothing
  intersect x Nothing = Just x
  intersect Nothing x = Just x
  intersect m1 m2 | m1 == m2 = Just m1
                  | otherwise = Nothing
                   
instance P.Pattern IPAddressPrefix where
  top = defaultIPPrefix
  intersect = IPAddress.intersect

instance Actionable Frenetic.Switches.OpenFlow.Actions where
    actController = undefined
    actDefault = undefined
    actTranslate s = List.map (SendOutPort . PhysicalPort) $ Set.toList s

skelToRules :: Skeleton Pattern Frenetic.Switches.OpenFlow.Actions -> [Rule]
skelToRules (Skeleton bones) = List.map (\(Bone p a) -> Rule p a) bones 

instance P.Pattern OFMatch.Match where
  top = Match { 
    inPort = P.top,
    srcEthAddress = P.top,
    dstEthAddress = P.top,
    vLANID = P.top,
    vLANPriority = P.top,
    ethFrameType = P.top,
    ipTypeOfService = P.top,
    ipProtocol = P.top,
    srcIPAddress = P.top,
    dstIPAddress = P.top,
    srcTransportPort = P.top,
    dstTransportPort = P.top }
  intersect ofm1 ofm2 = 
    do { inport <- P.intersect (inPort ofm1) (inPort ofm2)
       ; srcethaddress <- P.intersect (srcEthAddress ofm1) (srcEthAddress ofm2)
       ; dstethaddress <- P.intersect (dstEthAddress ofm1) (dstEthAddress ofm2)
       ; vlanid <- P.intersect (vLANID ofm1) (vLANID ofm2)
       ; vlanpriority <- P.intersect (vLANPriority ofm1) (vLANPriority ofm2)
       ; ethframetype <- P.intersect (ethFrameType ofm1) (ethFrameType ofm2)
       ; iptypeofservice <- P.intersect (ipTypeOfService ofm1) (ipTypeOfService ofm2)
       ; ipprotocol <- P.intersect (ipProtocol ofm1) (ipProtocol ofm2)
       ; srcipaddress <- P.intersect (srcIPAddress ofm1) (srcIPAddress ofm2)
       ; dstipaddress <- P.intersect (dstIPAddress ofm1) (dstIPAddress ofm2)
       ; srctransportport <- P.intersect (srcTransportPort ofm1) (srcTransportPort ofm2)
       ; dsttransportport <- P.intersect (dstTransportPort ofm1) (dstTransportPort ofm2)
       ; Just $ Match { 
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
           dstTransportPort = dsttransportport } }

word48ToEth (LargeKey a (LargeKey b (LargeKey c (LargeKey d (LargeKey e f))))) = EthernetAddress a b c d e f

countBits :: (Bits a) => a -> Int
countBits x = sum [1 | i <- [0 .. bitSize x], not $ testBit x i]
                                                                                 
instance Patternable Match where
    patOverapprox Dl_src w = P.top { srcEthAddress = fmap word48ToEth $ P.overapprox w }
    patOverapprox Dl_dst w = P.top { dstEthAddress = fmap word48ToEth $ P.overapprox w }
    patOverapprox Dl_typ w = P.top { ethFrameType = P.overapprox w }
    patOverapprox Dl_vlan w = P.top { vLANID = P.overapprox w }
    patOverapprox Dl_vlan_pcp w = P.top { vLANPriority = P.overapprox w }
    patOverapprox Nw_src w = P.top { srcIPAddress = (IPAddress x, fromIntegral $ countBits m) }
        where
          P.Prefix (P.Wildcard x m) = P.overapprox w
    patOverapprox Nw_dst w = P.top { dstIPAddress = (IPAddress x, fromIntegral $ countBits m) }
        where
          P.Prefix (P.Wildcard x m) = P.overapprox w
    patOverapprox Nw_proto w = P.top { ipProtocol = P.overapprox w }
    patOverapprox Nw_tos w = P.top { ipTypeOfService = P.overapprox w }
    patOverapprox Tp_src w = P.top { srcTransportPort = P.overapprox w }
    patOverapprox Tp_dst w = P.top { dstTransportPort = P.overapprox w }

    patInport p = P.top { inPort = Just p }

    patUnderapprox Dl_src w v = do p <- P.underapprox w v
                                   return $ P.top { srcEthAddress = fmap word48ToEth $ p }
    patUnderapprox Dl_dst w v = do p <- P.underapprox w v
                                   return $ P.top { dstEthAddress = fmap word48ToEth $ p }
    patUnderapprox Dl_typ w v = do p <- P.underapprox w v
                                   return $ P.top { ethFrameType = p }
    patUnderapprox Dl_vlan w v = do p <- P.underapprox w v
                                    return $ P.top { vLANID = p }
    patUnderapprox Dl_vlan_pcp w v = do p <- P.underapprox w v
                                        return $ P.top { vLANPriority = p }
    patUnderapprox Nw_src w v = do P.Prefix (P.Wildcard x m) <- P.underapprox w v
                                   return $ P.top { srcIPAddress = (IPAddress x, fromIntegral $ countBits m) }
    patUnderapprox Nw_dst w v = do P.Prefix (P.Wildcard x m) <- P.underapprox w v
                                   return $ P.top { dstIPAddress = (IPAddress x, fromIntegral $ countBits m) }
    patUnderapprox Nw_proto w v = do p <- P.underapprox w v
                                     return $ P.top { ipProtocol = p }
    patUnderapprox Nw_tos w v = do p <- P.underapprox w v
                                   return $ P.top { ipTypeOfService = p }
    patUnderapprox Tp_src w v = do p <- P.underapprox w v
                                   return $ P.top { srcTransportPort = p }
    patUnderapprox Tp_dst w v = do p <- P.underapprox w v 
                                   return $ P.top { dstTransportPort = p }

deriving instance Typeable OFMatch.Match