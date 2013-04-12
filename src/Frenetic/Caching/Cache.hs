{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Provides a simple, basic, and efficient server which provides methods
-- to listen for new switches to connect, and to receive and send OpenFlow
-- messages to switches. This server handles the initialization procedure with switches
-- and handles echo requests from switches.
module Frenetic.Caching.Cache
    (     
      -- * OpenFlow Server
      OpenFlowServer
      , ServerPortNumber 
      , HostName
      , startOpenFlowServer
      , acceptSwitch 
      , closeServer
        -- * Switch connection
      , SwitchHandle
      , handle2SwitchID
      , switchSockAddr
      , receiveFromSwitch
      , sendToSwitch
      , sendToCache
      , sendToSwitchWithID
      , closeSwitchHandle
        -- * Utility
      , untilNothing
    ) where

import Frenetic.Common
import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Exception.Base
import Control.Monad.State
import System.IO
import Frenetic.Topo (Switch,Port,Loc)
import Control.Exception
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Nettle.OpenFlow
-- import Nettle.Servers.Server (ServerPortNumber, OpenFlowServer, SwitchHandle)
import Nettle.Servers.Server (ServerPortNumber)
import qualified Nettle.Servers.Server as Server 
import Data.Word
import Foreign
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf
import System.Log.Logger
import Debug.Trace (traceShow)
import Frenetic.Caching.CacheUtil
import Frenetic.NetCore.Pretty
import Frenetic.NetCore.Types
import Frenetic.NetCore.Util
import Frenetic.NetCore.Short (synthRestrict, (<+>))
import Data.Binary (decode)
import Frenetic.NetCore.Semantics
import Frenetic.NetCore.Compiler
import Frenetic.Update1
import Frenetic.Switches.OpenFlow
import qualified Frenetic.Topo as Topo
import qualified Frenetic.NetCore.Types as NetCore
import qualified Text.JSON.Generic as JSON
import Prelude hiding (catch)
import Control.Exception
import Data.Graph (Graph)
import Data.List
import Data.IORef as R
import Debug.Trace (traceShow)
--import Frenetic.Hosts.Nettle
newr = R.newIORef
getr = R.readIORef
r != n = R.writeIORef r n



type Channels = (Chan (Maybe (TransactionID, SCMessage)),Chan (TransactionID, CSMessage))
data SwitchHandle = SwitchHandle !(Server.SwitchHandle, Channels)
newtype OpenFlowServer = OpenFlowServer (Server.OpenFlowServer, IORef (Map SwitchID SwitchHandle))


{-
type ChanState = (Chan (TransactionID, SCMessage),Chan (TransactionID, SCMessage))
type Chans a = State ChanState a


createChans :: Chan (TransactionID, SCMessage) -> Chan (TransactionID, SCMessage) -> IO (Chan (TransactionID, SCMessage), Chan (TransactionID, SCMessage))
createChans chan1 chan2 = do 
    put (chan1, chan2)
    return (chan1, chan2)


getChans :: Chans (Chan (TransactionID, SCMessage), Chan (TransactionID, SCMessage)) 
getChans chs = do
    (chan1,chan2) <- get
    return (chan1, chan2)
-}


convertSH :: SwitchHandle -> Server.SwitchHandle
convertSH sh@(SwitchHandle (sh1, _)) = sh1

convertOFS :: OpenFlowServer -> Server.OpenFlowServer
convertOFS (OpenFlowServer (ofs1, _)) = ofs1


-- | Starts an OpenFlow server. 
-- The server socket will be bound to a wildcard IP address if the first argument is 'Nothing' and will be bound to a particular 
-- address if the first argument is 'Just' something. The 'HostName' value can either be an IP address in dotted quad notation, 
-- like 10.1.30.127, or a host name, whose IP address will be looked up. The server port must be specified.
startOpenFlowServer :: Maybe HostName -> ServerPortNumber -> IO OpenFlowServer
startOpenFlowServer mHostName portNumber = do 
    ofs1 <- Server.startOpenFlowServer mHostName portNumber 
    shmr <- newIORef Map.empty
    return (OpenFlowServer (ofs1, shmr))


-- | Closes the OpenFlow server.
closeServer :: OpenFlowServer -> IO ()
-- closeServer (OpenFlowServer (s,_)) = sClose s
closeServer ofs =
    Server.closeServer (convertOFS ofs)



-- | Blocks until a switch connects to the server and returns the 
-- switch handle.
acceptSwitch :: OpenFlowServer -> IO (SwitchHandle, SwitchFeatures)
acceptSwitch ofs@(OpenFlowServer (ofs1, shmr)) = do 
    (sh1, sf) <- Server.acceptSwitch ofs1
    msgChan <- newChan
    queryChan <- newChan
    let sh = SwitchHandle (sh1, (msgChan, queryChan))
    switchHandleMap <- readIORef shmr
    writeIORef shmr (Map.insert (handle2SwitchID sh) sh switchHandleMap)
    return (sh, sf)


-- | Returns the socket address of the switch connection. 
switchSockAddr :: SwitchHandle -> SockAddr
switchSockAddr sh = Server.switchSockAddr (convertSH sh)
            
-- | Blocks until a message is received from the switch or the connection is closed.
-- Returns `Nothing` only if the connection is closed.
receiveFromSwitch :: SwitchHandle -> IO (Maybe (TransactionID, SCMessage))
receiveFromSwitch sh@(SwitchHandle (sh1, (msgChan, _))) = do
    -- (msgChan, _) <- getChans
    msg <- readChan msgChan
    return msg
    -- Server.receiveFromSwitch sw

-- | Send a message to the switch.
sendToSwitch :: SwitchHandle -> (TransactionID, CSMessage) -> IO ()       
sendToSwitch sh@(SwitchHandle (sh1, (_, queryChan))) query = do
    writeChan queryChan query
    -- Server.sendToSwitch sw query

sendToSwitchWithID :: OpenFlowServer -> SwitchID -> (TransactionID, CSMessage) -> IO ()                                             
sendToSwitchWithID (OpenFlowServer (_,shmr)) sid msg
   = do switchHandleMap <- readIORef shmr
        case Map.lookup sid switchHandleMap of
          Nothing -> printf "Tried to send message to switch: %d, but it is no longer connected.\nMessage was %s.\n" sid (show msg)
          Just sh -> sendToSwitch sh msg --this could fail.


-- | Close a switch connection.     
closeSwitchHandle :: SwitchHandle -> IO ()    
closeSwitchHandle sh = 
    Server.closeSwitchHandle (convertSH sh)


handle2SwitchID :: SwitchHandle -> SwitchID
handle2SwitchID sh = 
    Server.handle2SwitchID (convertSH sh)

-- | Repeatedly perform the first action, passing its result to the second action, until
-- the result of the first action is 'Nothing', at which point the computation returns.
untilNothing :: IO (Maybe a) -> (a -> IO ()) -> IO ()
untilNothing sense act = 
    Server.untilNothing sense act


runCache :: SwitchHandle -> [(Match, [Act])] -> MVar() -> IO ()
runCache sw@(SwitchHandle (sw1, (msgChan, queryChan))) classifier kill = do
    flowTbl <- toFlowTable classifier
    -- Map each flow to its PacketCounter and ByteCounter
    let insertFlow statsMap flowID =  Map.insert flowID ((!!) flowTbl flowID, 0, 0) statsMap
    statsTbl <- newIORef $ foldl insertFlow Map.empty [0 .. (length flowTbl)-1] 
    
    kill' <- runFlowStats sw1 classifier
    debugM "controller" $ "flow table is " ++ show flowTbl
    --  killMVar' <- runQueryOnSwitch server switch classifier counters callbacks
    -- Priority 65535 is for microflow rules from reactive-specialization 
    let flowMods = deleteAllFlows : (zipWith mkAddFlow flowTbl  [65534, 65533 ..])
    -- mapM_ (Server.sendToSwitch sw1) (zip [0,0..] flowMods)
    msgChan1 <- newChan
    let loop = do
        m <- Server.receiveFromSwitch sw1
	debugM "controller" $ "Received message1 at cache layer - " ++ show m
        writeChan msgChan1 m
        case m of
          Just _ -> loop
          Nothing -> return ()
    tid <- forkIO $ loop
    forkIO $ do
       takeMVar kill
       killThread tid
       putMVar kill' ()
       takeMVar kill'
       putMVar kill ()
    queryOrMsg <- select queryChan msgChan1
    forever $ do
        v <- readChan queryOrMsg
	case v of
	    Left query@(xid, StatsRequest (FlowStatsRequest {statsRequestMatch = match, ..})) -> do 
	      -- Need to write the function Matches :: Match -> Match-> Bool (Look at Semantics.hs L625) 
	      statsMap <- readIORef statsTbl
	      let find_match stat@(xid, ((ptrn,acts),pktcnt, bytecnt)) = 
		        match == ptrn 
	      case (Data.List.find find_match (Map.toList statsMap)) of
	        Just (id,((match1,acts),pktcnt,bytecnt)) -> do
	          let scmsg = StatsReply (FlowStatsReply False [FlowStats {flowStatsMatch = match1, 
	          					  flowStatsActions = acts,
							  --flowStatsPriority = xid,
							  flowStatsPacketCount = pktcnt,
							  flowStatsByteCount = bytecnt}
					  ])
	          writeChan msgChan (Just (xid,scmsg)) 
	          debugM "CacheLayer" $ "flowStats at the Cache layer : PktCount = " ++ show pktcnt
		Nothing -> do
	      -- do a longest prefix match lookup in the classifer and give the statistics.      
	      -- Create a flowstats packet and send it to the controller.   
	          debugM "CacheLayer" $ "Received a query from the controller and sent to switchi - " ++ show query
	          Server.sendToSwitch sw1 query
	    Left msg -> do
	      debugM "controller" $ "Packet out from the controller " ++ show msg
	      Server.sendToSwitch sw1 msg
	    Right Nothing -> do
	      debugM "controller" $ "The switch seems to have disconnected"
	    Right scmsg@(Just (xID, msg)) ->  case msg of
	      StatsReply (FlowStatsReply _ stats) -> do
	          -- xid is the ID of the counter
		  let xid = fromIntegral xID
		  statsMap <- readIORef statsTbl
	          let counterId = fromIntegral xid 
                  let getStats (FlowStats{..}) = (flowStatsPacketCount, flowStatsByteCount)
                  let update (x0,y0) stat = 
			let (x,y) = getStats stat in
			       (x0+x,y0+y)      -- Fix this, you should do incremental sum
		  let (pktcnt,bytecnt) = foldl update (0,0) stats	    
		  case (Map.lookup xid statsMap) of
	            Just (flow,x, y) -> do
	              writeIORef statsTbl (Map.insert xid (flow,pktcnt,bytecnt) statsMap)
		    Nothing -> do
		      debugM "controller" $ "There is no flow entry corresponding to the reply in the Cache"  
	      PacketIn (packet@(PacketInfo {receivedOnPort=inPort, 
	                                reasonSent=reason,
	                                bufferID=bufferID,
					packetLength=len,
					packetData=pkt,
	                                enclosedFrame=Right frame})) -> do
		 case reason of
		   ExplicitSend -> writeChan msgChan scmsg
		   _ -> do
			 let match_packet stat@(xid, ((ptrn,acts),pktcnt, bytecnt)) = 
			        ptrnMatchPkt packet ptrn 
		         debugM "CacheLayer" $ "Saw this packet at the cache layer - " ++ show (packet)
			 statsMap <- readIORef statsTbl
			 case Data.List.find match_packet (Map.toList statsMap) of
			   Just (xid, ((ptrn, acts), pktcnt, bytecnt)) -> do 
			     let csmsg = PacketOut (PacketOutRecord (Right pkt) Nothing
			                      acts)
			     let pktbytes = fromIntegral len
		             debugM "CacheLayer" $ "Actions to apply to the packet at the cache layer - " ++ show (acts)
		             writeIORef statsTbl (Map.insert xid ((ptrn, acts), pktcnt+1, bytecnt+pktbytes) statsMap)	      
			    {-- case hasController acts of
			       True -> do
		                 debugM "CacheLayer" $ "Sent a packet to the controller from the cache layer - " ++ show (toPacket packet)
				 writeChan msgChan scmsg
			       False -> do
			         Server.sendToSwitch sw1 (0, csmsg)
		             --}
			     evalOFActions sw acts xID msg
			   Nothing -> do
			     debugM "Controller" $ "No rules match this packet and hence sent to controller"
		             writeChan msgChan scmsg
			    
	      _ -> do
	         debugM "controller" $ "received a message from the switch - " ++ show msg
	         writeChan msgChan scmsg
            


-- TODO: THis will not work if you have modifications and then ToController
hasController :: ActionSequence -> Bool
hasController acts = 
  case acts of
    (SendOutPort (ToController _)):acts1 -> True
    act:acts1 -> hasController acts1
    [] -> False



evalOFActions :: SwitchHandle -> ActionSequence -> TransactionID -> SCMessage -> IO()
evalOFActions sw acts xid msg = 
  case acts of
    act:acts1 -> do
      msgOut <- (evalOFAction sw act xid msg)
      evalOFActions sw acts1 xid msgOut
    [] -> do 
      debugM "controller" $ "Empty actions!!" 

evalOFAction :: SwitchHandle -> Nettle.OpenFlow.Action -> TransactionID -> SCMessage -> IO (SCMessage)
evalOFAction sw@(SwitchHandle (sw1,(msgChan,_))) act xid msg@(PacketIn pkt@(PacketInfo {..})) =
  case act of
    SendOutPort port ->
      case port of
        ToController _ -> do
	  debugM "CacheLayer" $ "Sent a packet to the controller from the cache layer - " ++ show (toPacket pkt)
	  let msg1 = PacketIn (PacketInfo {bufferID, packetLength, receivedOnPort, reasonSent = ExplicitSend, packetData, enclosedFrame})
	  writeChan msgChan (Just (xid, msg1))
	  return msg
	_ -> do  
	  let msgOut = PacketOut (PacketOutRecord (Right packetData) Nothing
	                        [act])
	  Server.sendToSwitch sw1 (0, msgOut)
          return msg
    _ -> do
      -- TODO: include modification actions
      return msg    


sendToCache :: SwitchHandle -> [(Match, [Act])] -> IO (MVar ())
sendToCache sh@(SwitchHandle (_, (msgChan, queryChan))) classifier = do
    kill' <- newEmptyMVar
    tid <- forkIO $ runCache sh classifier kill'
    kill <- newEmptyMVar
    -- Standard : Always kill your child and return
    forkIO $ do
      takeMVar kill
      putMVar kill' ()
      takeMVar kill'
      killThread tid
      putMVar kill ()
    return kill



runFlowStats :: Server.SwitchHandle -> [(Match, [Act])] -> IO (MVar ()) 
runFlowStats switch classifier = do
  dummyChan <- newChan
  let matches = zip [0..] (map fst classifier)
  let mkCb callbacks (x,m) = Map.insert x (CallbackPktCounter 100 (writeChan dummyChan)) callbacks
  let callbacks = foldl mkCb Map.empty matches
  let getMatches (Right delay) = Right delay 
      getMatches (Left (x, _)) = Left (x, fst ((!!) classifier x))   
  let queryStream = map getMatches (callbackDelayStream callbacks)
  let body (Right delay) = do
        threadDelay (delay * 1000)
      body (Left (x, match)) = do
        let mkReq m = (fromIntegral x, -- Word32
                        StatsRequest (FlowStatsRequest m AllTables Nothing))
        Server.sendToSwitch switch (mkReq match)
  tid <- forkIO $ mapM_ body queryStream
  kill <- newEmptyMVar
  forkIO $ do
    takeMVar kill
    killThread tid 
    putMVar kill ()
  return kill



