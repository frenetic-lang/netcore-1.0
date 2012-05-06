
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
-- src/Frenetic/Hosts/Nettle.hs                                               --
-- Nettle Host                                                                --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE
    NoMonomorphismRestriction,
    FlexibleInstances,
    Rank2Types,
    GADTs,
    MultiParamTypeClasses
 #-}

module Frenetic.Hosts.Nettle where

import Data.Set                            as Set
import Data.Map                            as Map
import Frenetic.LargeWord

import Control.Exception.Base
import Control.Concurrent
import Control.Monad.State
import Control.Newtype

import System.IO

import Nettle.Ethernet.EthernetFrame
import Nettle.Ethernet.EthernetAddress
import Nettle.IPv4.IPPacket
import Nettle.IPv4.IPAddress
import Nettle.OpenFlow.FlowTable           as FlowTable hiding (FlowRemoved)
import Nettle.OpenFlow.Match               as OFMatch
import Nettle.OpenFlow.MessagesBinary
import Nettle.OpenFlow.Messages            as Messages
import Nettle.OpenFlow.Packet
import Nettle.OpenFlow.Port
import Nettle.OpenFlow.Switch
import Nettle.OpenFlow.Action as OFAction
import Nettle.Servers.TCPServer
import Nettle.Servers.MultiplexedTCPServer

import Frenetic.NetCore.API
import Frenetic.NetCore.Compiler
import Frenetic.Switches.OpenFlow
import Frenetic.Compat
--import Frenetic.Util

--
-- Data types
--

data ControllerState = 
       ControllerState { addrMap :: Map SockAddr Switch,
                         policy :: Policy }

type ControllerOp = StateT ControllerState IO

type FSCMessage = (TransactionID, SCMessage)
type FCSMessage = (TransactionID, CSMessage)

type OFProcess = 
  Process (TCPMessage FSCMessage) (SockAddr, FCSMessage) IOException

-- Packet instances

               
-- Nettle server


installRules :: SockAddr -> [(OFMatch.Match, OFAction.ActionSequence)]  -> OFProcess -> ControllerOp ()
installRules addr rules proc = 
  liftIO $ foldM_ (\pri rule -> tellP proc (addr,(1, mk_flow rule pri)) >> return (pri - 1)) 65535 rules
  where mk_flow (pat, acts) pri = 
          FlowMod AddFlow { match=pat, 
                              priority=pri, 
                              FlowTable.actions=acts, 
                              cookie=0, 
                              notifyWhenRemoved=False, 
                              idleTimeOut=Permanent,
                              hardTimeOut=Permanent,
                              applyToPacket=Nothing,
                              overlapAllowed=True } 

sendBufferedPacket :: SockAddr -> BufferID -> PortID -> Transmission OFMatch.Match PacketInfo -> OFProcess -> ControllerOp ()
sendBufferedPacket addr mbid inport t proc = 
  do state <- get
     let pkts = undefined --Set.toList $ interpretPolicy (policy state) t 
     let ofacts = Prelude.map ((SendOutPort . PhysicalPort) . receivedOnPort) pkts
     let msg = Messages.PacketOut 
                   Nettle.OpenFlow.Packet.PacketOut 
                     { bufferIDData = Left mbid, 
                       Nettle.OpenFlow.Packet.inPort = Just inport, 
                       Nettle.OpenFlow.Packet.actions = ofacts }
     liftIO $ tellP proc (addr,(1, msg)) 

--
-- main handlers
--

freneticPort :: ServerPortNumber
freneticPort = 6633 

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
                       let t = Transmission (undefined :: OFMatch.Match) switch pkt
                       installRules addr (unpack $ specialize t pol) proc
                       case bufferID pkt of 
                         Nothing -> return () 
                         Just bid -> sendBufferedPacket addr bid inport t proc 
                Nothing -> return () 

keepalive :: SockAddr -> OFProcess -> IO () 
keepalive addr proc =  
  do threadDelay 15000000
     hPutStrLn stderr "Echo request"
     tellP proc (addr, (1, CSEchoRequest []))
     keepalive addr proc
         
switchJoin :: SockAddr -> OFProcess -> ControllerOp ()
switchJoin addr proc = 
  do liftIO $ tellP proc (addr, (1, FeaturesRequest))
     liftIO $ forkIO (keepalive addr proc)
     return ()
  
ofDispatch :: SockAddr -> FSCMessage -> OFProcess -> ControllerOp ()
ofDispatch addr (xid, scmsg) proc =   
  case scmsg of 
    SCHello ->
        switchJoin addr proc
    SCEchoRequest n -> 
        do liftIO $ hPutStrLn stderr "Echo reply"
           liftIO $ tellP proc (addr, (1, CSEchoReply n))
    SCEchoReply _ ->  return ()
    Features sf -> 
        do let switch = switchID sf 
           liftIO $ hPutStrLn stderr ("Features from " ++ show switch)
           state <- get
           let pol = policy state 
           let addrs = addrMap state
           put (state { addrMap = Map.insert addr switch addrs })
           installRules addr (unpack $ compile switch pol) proc 
    PacketIn pkt -> 
        let src = show $ pktDlSrc $ toPacket pkt in 
        let dst = show $ pktDlDst $ toPacket pkt  in 
        do liftIO $ hPutStrLn stderr ("PacketIn: " ++ src ++ " => " ++ dst)
           packetIn addr pkt proc
    PortStatus status ->  return ()
    FlowRemoved flow ->  return ()
    StatsReply reply ->  return ()
    BarrierReply ->  return ()
    Error e -> 
        liftIO $ hPutStrLn stderr ("Error: " ++ show e)      

loop :: OFProcess -> ControllerOp ()
loop proc = do
  msg <- liftIO $ readP proc
  case msg of 
    ConnectionEstablished addr -> 
        do liftIO $ hPutStrLn stderr ("Connection to " ++ show addr ++ " established")
           liftIO $ tellP proc (addr, (1, CSHello))
    ConnectionTerminated addr ioex ->        
        liftIO $ hPutStrLn stderr ("Connection to " ++ show addr ++ " terminated: " ++ show ioex)
    PeerMessage addr msg -> 
         ofDispatch addr msg proc
  loop proc 

nettleServer :: Policy -> IO ()
nettleServer init_policy = do let init_state = ControllerState { addrMap = Map.empty,
                                                                 policy = init_policy }
                              proc <- openFlowServer freneticPort
                              hPutStrLn stderr "--- Welcome to Frenetic ---"
                              evalStateT (loop proc) init_state 
