
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
-- src/Frenetic/Hosts/Nettle.hs                                                            --
-- Nettle Host                                                   --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE
    NoMonomorphismRestriction,
    StandaloneDeriving,
    FlexibleInstances,
    Rank2Types,
    GADTs,
    ExistentialQuantification
 #-}

module Frenetic.Hosts.Nettle where

import Data.List
import Data.Either
import Data.Set as Set
import Data.Map as Map
import Data.Word
import System.IO
import System.IO.Unsafe
import Numeric
import Data.Char
import Network.Socket.Internal
import Control.Exception.Base
import Control.Concurrent
import Control.Monad.State

import Nettle.OpenFlow.FlowTable as FlowTable hiding (FlowRemoved)
import Nettle.OpenFlow.Packet
import Nettle.Ethernet.EthernetFrame
import Nettle.Ethernet.EthernetAddress
import Nettle.IPv4.IPPacket
import Nettle.IPv4.IPAddress
import Nettle.OpenFlow.MessagesBinary
import Nettle.OpenFlow.Messages as Messages
import Nettle.OpenFlow.Port
import Nettle.OpenFlow.Switch
import Nettle.Servers.TCPServer
import Nettle.Servers.MultiplexedTCPServer

import Frenetic.Language
import Frenetic.Network
import Frenetic.Switches.OpenFlow
import Frenetic.Compiler

data ControllerState = 
       ControllerState { addrMap :: Map SockAddr Switch,
                         policy :: Policy PacketInfo }

type ControllerOp  = StateT ControllerState IO

type FSCMessage = (TransactionID,SCMessage)
type FCSMessage = (TransactionID,CSMessage)

type OFProcess = 
  Process (TCPMessage FSCMessage) (SockAddr, FCSMessage) IOException

-- Packet instances    

nettleEthernetFrame pkt = 
  case runGetE getEthernetFrame $ packetData pkt of 
    Left err -> error ("Expected an Ethernet frame: " ++ err)
    Right ef -> ef

nettleEthernetHeaders pkt = 
  case nettleEthernetFrame pkt of
    EthernetFrame hdr _ -> hdr

nettleEthernetBody pkt = 
  case nettleEthernetFrame pkt of
    EthernetFrame _ bdy -> bdy

instance Packet Nettle.OpenFlow.Packet.PacketInfo where 
  getHeader pi h = 
    case h of 
      Dl_src -> 
        HardwareAddress $ sourceMACAddress $ nettleEthernetHeaders pi
      Dl_dst -> 
        HardwareAddress $ destMACAddress $ nettleEthernetHeaders pi
      Dl_typ -> 
        typeCode $ nettleEthernetHeaders pi
      Dl_vlan -> 
        case nettleEthernetHeaders pi of
          EthernetHeader _ _ _ -> 0xfffff 
          Ethernet8021Q _ _ _ _ _ vlan -> vlan
      Dl_vlan_pcp -> 
        case nettleEthernetHeaders pi of
          EthernetHeader _ _ _ -> 0 
          Ethernet8021Q _ _ _ pri _ _ -> pri
      Nw_src -> 
        case addr of IPAddress a -> a
        where addr = case nettleEthernetBody pi of
                       IPInEthernet (IPPacket hdr _) -> ipSrcAddress hdr
                       ARPInEthernet arp -> senderIPAddress arp
                       _ -> ipAddress 0 0 0 0
      Nw_dst -> 
        case addr of IPAddress a -> a
        where addr = case nettleEthernetBody pi of
                       IPInEthernet (IPPacket hdr _) -> ipDstAddress hdr
                       ARPInEthernet arp -> targetIPAddress arp
                       _ -> ipAddress 0 0 0 0
      Nw_proto -> 
        case nettleEthernetBody pi of
          IPInEthernet (IPPacket hdr _) -> ipProtocol hdr
          ARPInEthernet arp -> 
            case arpOpCode arp of 
              ARPRequest -> 1
              ARPReply -> 2
          _ -> 0
      Nw_tos -> 
          case nettleEthernetBody pi of
            IPInEthernet (IPPacket hdr _) -> dscp hdr
            _ -> 0 
      Tp_src -> 
        case nettleEthernetBody pi of 
          IPInEthernet (IPPacket _ (TCPInIP (src,_))) -> src
          IPInEthernet (IPPacket _ (UDPInIP (src,_))) -> src
          IPInEthernet (IPPacket _ (ICMPInIP (typ,_))) -> fromIntegral typ
          _ -> 0
      Tp_dst ->
        case nettleEthernetBody pi of 
          IPInEthernet (IPPacket _ (TCPInIP (_,dst))) -> dst
          IPInEthernet (IPPacket _ (UDPInIP (_,dst))) -> dst
          IPInEthernet (IPPacket _ (ICMPInIP (_,cod))) -> fromIntegral cod
          _ -> 0

-- Nettle server


installRules :: SockAddr -> [Rule]  -> OFProcess -> ControllerOp ()
installRules addr rules proc = 
  liftIO $ foldM_ (\pri rule -> tellP proc (addr,(1, mk_flow rule pri)) >> return (pri - 1)) 65535 rules
  where mk_flow (Rule pat acts) pri = 
          FlowMod $ AddFlow { match=pat, 
                              priority=pri, 
                              FlowTable.actions=acts, 
                              cookie=0, 
                              notifyWhenRemoved=False, 
                              idleTimeOut=Permanent,
                              hardTimeOut=Permanent,
                              applyToPacket=Nothing,
                              overlapAllowed=True } 

sendBufferedPacket :: SockAddr -> BufferID -> PortID -> Transmission PacketInfo -> OFProcess -> ControllerOp ()
sendBufferedPacket addr mbid inport t proc = 
  do state <- get
     let pol = policy state 
     let ofacts = actTranslate (interpretPolicy pol t)
     let msg = Messages.PacketOut $ 
                   Nettle.OpenFlow.Packet.PacketOut 
                     { bufferIDData = Left mbid, 
                       inPort = Just inport, 
                       Nettle.OpenFlow.Packet.actions = ofacts }
     liftIO $ tellP proc (addr,(1, msg)) 

--
-- main handlers
--

frenetic_port :: ServerPortNumber
frenetic_port = 6633 

packetIn :: SockAddr -> PacketInfo -> OFProcess -> ControllerOp ()
packetIn addr pkt proc = 
  case runGetE getEthernetFrame (packetData pkt) of
       Left err -> 
           -- skip non-ethernet frames
           liftIO $ hPutStrLn stderr ("Skipping packet due to " ++ err)
       Right (EthernetFrame header body) -> 
           do state <- get
              let addrs = addrMap state
              let pol = policy state
              case Map.lookup addr addrs of
                Just switch -> 
                    do let inport = receivedOnPort pkt 
                       let t = Transmission switch inport pkt
                       installRules addr (skelToRules $ specialize pol switch t) proc
                       case bufferID pkt of 
                         Nothing -> return () 
                         Just bid -> sendBufferedPacket addr bid inport t proc 
                Nothing -> return () 

keepalive :: SockAddr -> OFProcess -> IO () 
keepalive addr proc =  
  do threadDelay 15000000
     hPutStrLn stderr ("Echo request")
     tellP proc (addr, (1, CSEchoRequest []))
     keepalive addr proc
         
switchJoin :: SockAddr -> OFProcess -> ControllerOp ()
switchJoin addr proc = 
  do liftIO $ tellP proc (addr, (1, FeaturesRequest))
     liftIO $ forkIO (keepalive addr proc)
     return ()
  
of_dispatch :: SockAddr -> FSCMessage -> OFProcess -> ControllerOp ()
of_dispatch addr (xid, scmsg) proc =   
  case scmsg of 
    SCHello -> 
        switchJoin addr proc
    SCEchoRequest n -> 
        do liftIO $ hPutStrLn stderr "Echo reply"
           liftIO $ tellP proc (addr, (1, CSEchoReply n))
    SCEchoReply _ -> do return ()
    Features sf -> 
        do let switch = switchID sf 
           liftIO $ hPutStrLn stderr ("Features from " ++ show switch)
           state <- get
           let pol = policy state 
           let addrs = addrMap state
           put (state { addrMap = Map.insert addr switch addrs })
           installRules addr (skelToRules $ compile switch pol) proc 
    PacketIn pkt -> 
        let src = show $ getHeader pkt Dl_src in 
        let dst = show $ getHeader pkt Dl_dst in 
        do liftIO $ hPutStrLn stderr ("PacketIn: " ++ src ++ " => " ++ dst)
           packetIn addr pkt proc
    PortStatus status -> do return ()
    FlowRemoved flow -> do return ()
    StatsReply reply -> do return ()
    BarrierReply -> do return ()
    Error e -> 
        do liftIO $ hPutStrLn stderr ("Error: " ++ show e)      

loop :: OFProcess -> ControllerOp ()
loop proc = do
  msg <- liftIO $ readP proc
  case msg of 
    ConnectionEstablished addr -> 
        do liftIO $ hPutStrLn stderr ("Connection to " ++ show addr ++ " established")
           liftIO $ tellP proc (addr, (1, CSHello))
    ConnectionTerminated addr ioex ->        
        do liftIO $ hPutStrLn stderr ("Connection to " ++ show addr ++ " terminated: " ++ show ioex)
    PeerMessage addr msg -> 
        do of_dispatch addr msg proc
  loop proc 

nettleServer :: Policy PacketInfo -> IO ()
nettleServer init_policy = do let init_state = ControllerState { addrMap = Map.empty,
                                                            policy = init_policy }
                              proc <- openFlowServer frenetic_port
                              hPutStrLn stderr "--- Welcome to Frenetic ---"
                              evalStateT (loop proc) init_state 