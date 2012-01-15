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
    GADTs
 #-}

module Frenetic.Switches.OpenFlow where

import Data.Map
import Data.Bits
import Nettle.OpenFlow.Match
import Nettle.OpenFlow.Action
import Nettle.OpenFlow.Match as OFMatch
import Nettle.OpenFlow.Action as OFAction
import Nettle.IPv4.IPAddress as IPAddress
    
import Frenetic.Network
import Frenetic.Patterns

type Pattern = Match

type Actions = [Action]

data Rule = Rule Pattern Actions

instance Show Rule where
  show (Rule p a) = "Rule(" ++ show p ++ ", " ++ show a ++ ")"

data Classifier = Classifier [Rule]

data Configuration = Configuration (Map Switch Classifier)

instance MatchElement OFMatch.Match where
  mTop = Match { 
    inPort = mTop,
    srcEthAddress = mTop,
    dstEthAddress = mTop,
    vLANID = mTop,
    vLANPriority = mTop,
    ethFrameType = mTop,
    ipTypeOfService = mTop,
    ipProtocol = mTop,
    srcIPAddress = mTop,
    dstIPAddress = mTop,
    srcTransportPort = mTop,
    dstTransportPort = mTop }
  mMeet ofm1 ofm2 = 
    do { inport <- mMeet (inPort ofm1) (inPort ofm2)
       ; srcethaddress <- mMeet (srcEthAddress ofm1) (srcEthAddress ofm2)
       ; dstethaddress <- mMeet (dstEthAddress ofm1) (dstEthAddress ofm2)
       ; vlanid <- mMeet (vLANID ofm1) (vLANID ofm2)
       ; vlanpriority <- mMeet (vLANPriority ofm1) (vLANPriority ofm2)
       ; ethframetype <- mMeet (ethFrameType ofm1) (ethFrameType ofm2)
       ; iptypeofservice <- mMeet (ipTypeOfService ofm1) (ipTypeOfService ofm2)
       ; ipprotocol <- mMeet (ipProtocol ofm1) (ipProtocol ofm2)
       ; srcipaddress <- mMeet (srcIPAddress ofm1) (srcIPAddress ofm2)
       ; dstipaddress <- mMeet (dstIPAddress ofm1) (dstIPAddress ofm2)
       ; srctransportport <- mMeet (srcTransportPort ofm1) (srcTransportPort ofm2)
       ; dsttransportport <- mMeet (dstTransportPort ofm1) (dstTransportPort ofm2)
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
headerExactMatch Dl_src (Just (HardwareAddress v)) = mTop { srcEthAddress = Just v }
headerExactMatch Dl_src Nothing = mTop
headerExactMatch Dl_dst (Just (HardwareAddress v)) = mTop { dstEthAddress = Just v }
headerExactMatch Dl_dst Nothing = mTop
headerExactMatch Dl_typ v = mTop { ethFrameType = v }
headerExactMatch Dl_vlan v = mTop { vLANID = v }
headerExactMatch Dl_vlan_pcp v = mTop { vLANPriority = v }
headerExactMatch Nw_src v = mTop { srcIPAddress = case v of { Just a -> (IPAddress a, maxPrefixLen); Nothing -> defaultIPPrefix } }
headerExactMatch Nw_dst v = mTop { dstIPAddress = case v of { Just a -> (IPAddress a, maxPrefixLen); Nothing -> defaultIPPrefix } }
headerExactMatch Nw_proto v = mTop { ipProtocol = v }
headerExactMatch Nw_tos v = mTop { ipTypeOfService = v }
headerExactMatch Tp_src v = mTop { srcTransportPort = v }
headerExactMatch Tp_dst v = mTop { dstTransportPort = v }

inportExactMatch :: Port -> OFMatch.Match
inportExactMatch n = mTop { inPort = Just n }
