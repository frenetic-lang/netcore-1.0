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
    FlexibleInstances
 #-}

module Frenetic.Switches.OpenFlow where

import Data.Map
import Data.Bits
import Nettle.OpenFlow.Match
import Nettle.OpenFlow.Action
import Nettle.OpenFlow.Match as OFMatch
import Nettle.OpenFlow.Action as OFAction
import Nettle.IPv4.IPAddress as IPAddress
import Data.Set as Set
import Data.List as List
    
import Frenetic.Network
import qualified Frenetic.Pattern as P
import qualified Frenetic.Compiler as C

type Pattern = Match

type Actions = [Action]

data Rule = Rule Pattern Actions

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

instance C.SwitchAction Actions where
    actController = undefined
    actDefault = undefined
    actTranslate s = List.map (SendOutPort . PhysicalPort) $ Set.toList s

skelToRules :: C.Skeleton Pattern Actions -> [Rule]
skelToRules (C.Skeleton bones) = List.map (\(C.Bone p a) -> Rule p a) bones 

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

headerExactMatch :: (Bits b) => Header b -> Maybe b -> OFMatch.Match
headerExactMatch Dl_src (Just (HardwareAddress v)) = P.top { srcEthAddress = Just v }
headerExactMatch Dl_src Nothing = P.top
headerExactMatch Dl_dst (Just (HardwareAddress v)) = P.top { dstEthAddress = Just v }
headerExactMatch Dl_dst Nothing = P.top
headerExactMatch Dl_typ v = P.top { ethFrameType = v }
headerExactMatch Dl_vlan v = P.top { vLANID = v }
headerExactMatch Dl_vlan_pcp v = P.top { vLANPriority = v }
headerExactMatch Nw_src v = P.top { srcIPAddress = case v of { Just a -> (IPAddress a, maxPrefixLen); Nothing -> defaultIPPrefix } }
headerExactMatch Nw_dst v = P.top { dstIPAddress = case v of { Just a -> (IPAddress a, maxPrefixLen); Nothing -> defaultIPPrefix } }
headerExactMatch Nw_proto v = P.top { ipProtocol = v }
headerExactMatch Nw_tos v = P.top { ipTypeOfService = v }
headerExactMatch Tp_src v = P.top { srcTransportPort = v }
headerExactMatch Tp_dst v = P.top { dstTransportPort = v }

inportExactMatch :: Port -> OFMatch.Match
inportExactMatch n = P.top { inPort = Just n }
