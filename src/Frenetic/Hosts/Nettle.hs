module Frenetic.Hosts.Nettle where

import Frenetic.Common
import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Exception.Base
import Control.Monad.State
import System.IO
import Frenetic.Topo (Switch,Port,Loc)
import Frenetic.NetCore.Pretty
import Frenetic.NetCore.Types
import Frenetic.NetCore.Util
import Frenetic.NetCore.Short (synthRestrict, (<+>))
import Data.Binary (decode)
import Frenetic.NetCore.Semantics
import Frenetic.NetCore.Compiler
import Frenetic.Update1
import Frenetic.Switches.OpenFlow
import Data.List (nub, find, intersperse)
import Frenetic.NettleEx hiding (Id)
import qualified Frenetic.Topo as Topo
import qualified Frenetic.NetCore.Types as NetCore
import qualified Text.JSON.Generic as JSON
import Prelude hiding (catch)
import Control.Exception

type Counters = Map Id (IORef (Integer, Map (Switch,Predicate) Integer))

initCounters :: Callbacks -> IO Counters
initCounters callbacks = do
  let mk counters x = do
        ref <- newIORef (0, Map.empty)
        return (Map.insert x ref counters)
  foldM mk Map.empty (Map.keys callbacks)

deleteAllFlows = FlowMod (DeleteFlows matchAny Nothing)

mkAddFlow :: (Match, ActionSequence)
          -> Priority
          -> CSMessage
mkAddFlow (pat, acts) pri = FlowMod AddFlow {
  match=pat,
  priority=pri,
  actions=acts,
  cookie=0,
  notifyWhenRemoved=False,
  idleTimeOut=Permanent,
  hardTimeOut=Permanent,
  applyToPacket=Nothing,
  overlapAllowed=True
}

isNotToController (SendOutPort (ToController _)) = False
isNotToController _ = True

policyOutputs :: Policy -> [(Chan (Loc, ByteString), Predicate)]
policyOutputs PoBottom = []
policyOutputs (PoBasic _ _) = []
policyOutputs (PoUnion pol1 pol2) = policyOutputs pol1 ++ policyOutputs pol2
policyOutputs (Sequence pol1 pol2) = policyOutputs pol1 ++ policyOutputs pol2
policyOutputs (Restrict (SendPackets chan) pred) = [(chan, pred)]
policyOutputs (Restrict pol pred) = policyOutputs (synthRestrict pol pred)
policyOutputs (SendPackets chan) = [(chan, Any)]

parsePkt :: Topo.Port -> ByteString -> Maybe Packet
parsePkt pt rawPkt = 
  let len = fromIntegral (BS.length rawPkt) -- TODO(arjun): overflow
      body = Right (decode rawPkt)
    in toPacket (PacketInfo Nothing len pt ExplicitSend rawPkt body)

processOut :: OpenFlowServer -> Callbacks -> Counters -> Out -> IO ()
processOut server _ _ (OutPkt sw pt hdrs pkt) = do
  let msg = PacketOut (PacketOutRecord pkt Nothing 
                                       [physicalPortOfPseudoPort pt])
  sendToSwitchWithID server sw (0, msg)
processOut _ _ _ OutNothing = do
  return ()
-- The errors below should *never* occur; if they do, we have a bug.
processOut _ callbacks _ (OutGetPkt x loc hdrs) = case Map.lookup x callbacks of
  Nothing -> do
    errorM "controller" ("unknown callback " ++ show x)
  Just cb -> case cb of
    CallbackGetPkt procM -> procM (loc,hdrs)
    otherwise -> errorM "controller" ("expected GetPkt callback for " ++ show x)
processOut _ _ counters (OutUpdPktCounter x sw pred numPkts) = 
  case Map.lookup x counters of
    Nothing -> errorM "controller" "counter not found"
    Just mapRef -> atomicModifyIORef mapRef 
                     (\(n,m) -> 
                       let last = Map.findWithDefault 0 (sw,pred) m
                           d = numPkts - last in
                       ((n + d, Map.insert (sw,pred) numPkts m), ()))
processOut _ _ counters (OutUpdByteCounter x sw pred numBytes) = 
  case Map.lookup x counters of
    Nothing -> errorM "controller" "counter not found"
    Just mapRef -> atomicModifyIORef mapRef 
                     (\(n,m) ->
                          let last = Map.findWithDefault 0 (sw,pred) m  
                              d = numBytes - last in
                          ((n + d, Map.insert (sw,pred) numBytes m), ()))
processOut _ callbacks _ (OutSwitchEvt x evt) = case Map.lookup x callbacks of
  Nothing -> do
    errorM "controller" ("unknown callback " ++ show x)
  Just cb -> case cb of
    CallbackMonSwitch procM -> procM evt
    otherwise -> errorM "controller" ("unexpected callback for " ++ show x)
processOut _ _ counters (OutIncrPktCounter x) =
  case Map.lookup x counters of
    Nothing -> errorM "controller" "counter not found"
    Just mapRef -> atomicModifyIORef mapRef 
                     (\(n,m) -> ((n+1, m), ()))
processOut _ _ counters (OutIncrByteCounter x pktSize) =
  case Map.lookup x counters of
    Nothing -> errorM "controller" "counter not found"
    Just mapRef -> atomicModifyIORef mapRef 
                     (\(n,m) -> ((n+pktSize, m), ()))

invokeCallbacksOnTimers :: Counters
                        -> Callbacks
                        -> IO (MVar ())
invokeCallbacksOnTimers counters callbacks = do
  killMVar <- newEmptyMVar
  let body (Right delay) = do
        threadDelay (delay * 1000)
      body (Left (x, cb)) = case Map.lookup x counters of
        Nothing -> errorM "controller" "invokeCallbacksOnTimers: counter?"
        Just ref -> do
          (localCount, countersPerSwitch) <- readIORef ref
          let v = localCount 
          case cb of
            CallbackByteCounter _ procM -> procM (0, v)
            CallbackPktCounter _ procM -> procM (0, v)
            CallbackMonSwitch _ -> 
              errorM "controller" "invokeCallbacksOnTimer: wrong callback type"
            CallbackGetPkt _ ->
              errorM "controller" "invokeCallbacksOnTimer: wrong callback type"
  tid <- forkIO $ mapM_ body (callbackDelayStream callbacks)
  forkIO $ do
    takeMVar killMVar
    killThread tid
    putMVar killMVar ()
  return killMVar


handlePolicyOutputs :: OpenFlowServer 
                    -> Callbacks
                    -> Counters
                    -> Pol 
                    -> [(Int, Chan (Loc, ByteString))]
                    -> MVar () 
                    -> NettleServerOpts
                    -> IO ()
handlePolicyOutputs server callbacks counters pol gens kill opts = do
  mergedChan <- newChan
  let mkOutput (x, chan) = forkIO $ forever $ do
        v <- readChan chan
        writeChan mergedChan (x, v)
  readThreads <- mapM mkOutput gens
  writeThread <- forkIO $ forever $ do
    (x, (Loc sw pt, rawPkt)) <- readChan mergedChan
    case parsePkt pt rawPkt of
      Nothing -> warningM "controller" "dropping unparsable packet"
      Just pkt -> do
        let inGenPkt = InGenPkt x sw (Physical pt) pkt rawPkt
        tryLogIO opts inGenPkt
        let outs = evalPol pol inGenPkt
        mapM_ (processOut server callbacks counters) outs
  takeMVar kill
  mapM_ killThread (writeThread:readThreads)
  putMVar kill ()

-- |Installs the static portion of the policy, then react.
handleSwitch :: OpenFlowServer
             -> Callbacks
             -> Counters
             -> SwitchHandle
             -> Pol
             -> MVar ()
             -> NettleServerOpts
             -> IO ()
handleSwitch server callbacks counters switch pol kill opts = do
  -- 1. Clear the flow table
  -- 2. In parallel:
  --    a. Receive a message from the switch
  --    b. Receive a policy
  -- 3. Keep the last policy in an accumulator
  -- 4. On new message, evaluate it with the last policy (not consistent
  --    update!)
  -- 5. On new policy, update the switch and accumulator
  
  -- Maybe BUG: Arjun worried might not kill
  let switchID = handle2SwitchID switch
  -- create switch message channel
  msgChan <- newChan
  let loop = do
        m <- receiveFromSwitch switch
        writeChan msgChan m 
        case m of
          Just _ -> loop 
          Nothing -> return ()
  forkIO $ loop
  -- create kill message channel
  killChan <- newChan
  killThreadId <- forkIO $ do
    v <- takeMVar kill
    writeChan killChan v
  -- main function
  debugM "controller" $ "policy is " ++ show pol
  let classifier = compile (handle2SwitchID switch) pol
  debugM "controller" $ "classifier is " ++ show classifier
  flowTbl <- toFlowTable classifier
  debugM "controller" $ "flow table is " ++ show flowTbl
  killMVar' <- runQueryOnSwitch server switch classifier counters callbacks
  -- Priority 65535 is for microflow rules from reactive-specialization
  let flowMods = [deleteAllFlows]
  mapM_ (sendToSwitch switch) (zip [0,0..] flowMods)
  killOrMsg <- select killChan msgChan
  forever $ do
    v <- readChan killOrMsg
    case v of
      Left () -> do
        putMVar killMVar' ()
        takeMVar killMVar'
        putMVar kill ()
        tid <- myThreadId
        killThread tid
      Right Nothing -> do
        let evt = InSwitchEvt (SwitchDisconnected switchID)
        tryLogIO opts evt
        let outs = evalPol pol evt
        mapM_ (processOut server callbacks counters) outs
        writeChan killChan ()
      Right (Just (xid, msg)) ->  case msg of
        PortStatus (reason, ofPort@Port{..}) -> do
          let evt = InSwitchEvt (PortEvent switchID portID reason ofPort)
          tryLogIO opts evt
          let outs = evalPol pol evt
          mapM_ (processOut server callbacks counters) outs      
        StatsReply (FlowStatsReply _ stats) -> do
          -- xid is the ID of the counter
          let counterId = fromIntegral xid
          let mkIn (FlowStats{..}) = 
                InCounters counterId
                           switchID
                           (And (Switch switchID)
                                (predicateOfMatch flowStatsMatch))
                           flowStatsPacketCount 
                           flowStatsByteCount
          let ins = map mkIn stats
          units <- mapM (tryLogIO opts) ins
          let outs = concatMap (evalPol pol) ins
          debugM "controller" $ show (stats, ins, outs)
          mapM_ (processOut server callbacks counters) outs
        PacketIn (pk@(PacketInfo {receivedOnPort=inPort, 
                                  reasonSent=reason,
                                  bufferID=bufferID,    
                                  enclosedFrame=Right frame})) -> do
          let loc = Loc switchID inPort
          case toPacket pk of
            Nothing -> return ()
            Just pkt -> do
              let buf = case reason of
                          ExplicitSend -> Nothing -- TODO(arjun): what?
                          -- TODO(arjun): this is totally broken
                          -- we are ignoring the bufferID if it is present! <<- wtf is this
                          NotMatched -> bufferID
              let inp = InPkt loc pkt buf
              tryLogIO opts inp
              let outputs = evalPol pol inp
              -- Why does this use side-effects? WHy do I even care?
              actions <- actnTranslate (mapMaybe outToMicroflowAction outputs)
              let flowMod = mkAddFlow (frameToExactMatch inPort frame, actions) 65535
              sendToSwitch switch (0, flowMod)
              mapM_ (processOut server callbacks counters) outputs

        otherwise -> debugM "controller" $ show otherwise -- ignore all other messages 

deleteQueue :: OpenFlowServer -> Queue -> IO ()
deleteQueue server (Queue switch port queue _) = do
  sendToSwitchWithID server switch
    (0, ExtQueueDelete port [QueueConfig queue []])

createQueue :: OpenFlowServer -> Queue -> IO ()
createQueue server (Queue switch port queue minRate) = do
  let cfg = MinRateQueue (Enabled minRate)
  sendToSwitchWithID server switch
    (0, ExtQueueModify port [QueueConfig queue [cfg]])

createQueuesForSwitch :: OpenFlowServer 
                      -> Map Switch [Queue]
                      -> Switch
                      -> IO ()
createQueuesForSwitch server qMap switch =
  mapM_ (createQueue server) (Map.findWithDefault [] switch qMap)

deleteQueuesForSwitch :: OpenFlowServer 
                      -> Map Switch [Queue]
                      -> Switch
                      -> IO ()
deleteQueuesForSwitch server qMap switch =
  mapM_ (deleteQueue server) (Map.findWithDefault [] switch qMap)

-- |Configuration options that tweak the operation of the Nettle server.
data NettleServerOpts = NettleServerOpts
  { logIO :: Maybe (Chan String) 
    -- ^Output JSON representations of policies and packets to deploy
    -- and incoming packets and stats.
    , host :: Maybe String
  }

defaultNettleServerOpts :: NettleServerOpts
defaultNettleServerOpts = NettleServerOpts { logIO = Nothing, host = Nothing }

tryLogIO :: JSON.Data a => NettleServerOpts -> a -> IO ()
tryLogIO NettleServerOpts{..} val =
  case logIO of
    Just chan -> writeChan chan $ JSON.encodeJSON val
    _ -> return ()

makeSwitchChan :: OpenFlowServer
               -> IO (Chan (SwitchHandle, SwitchFeatures))
makeSwitchChan server = do
  chan <- newChan
  forkIO $ forever $ do
    v <- acceptSwitchRetry server
    writeChan chan v
  return chan

acceptSwitchRetry :: OpenFlowServer
                  -> IO (SwitchHandle, SwitchFeatures)
acceptSwitchRetry server = do
  let exnHandler (e :: SomeException) = do
        infoM "nettle" $ "could not accept switch " ++ show e
        accept
      accept = do
        (acceptSwitch server) `catches`
          [ Handler (\(e :: AsyncException) -> throw e), -- handles ^C and such
            Handler exnHandler ]
  (switch, switchFeatures) <- accept
  return (switch, switchFeatures)

programToSemanticPol :: Program ->
                        (Map Switch [Queue],
                         Callbacks, 
                         [(Id, Chan (Loc, ByteString))],
                         Pol)
programToSemanticPol program = 
  let (queues', sugaredPolicy) = evalProgram program
      (callbacks, generators, pol) = desugarPolicy sugaredPolicy in
  (queues', callbacks, generators, pol)

nettleServer' :: OpenFlowServer 
              -> NettleServerOpts 
              -> Chan (Either (SwitchHandle, SwitchFeatures) 
                              (Map Switch [Queue],
                               Callbacks, 
                               [(Id, Chan (Loc, ByteString))],
                               Pol))
              -> IO ()
nettleServer' server opts switchPolicyChan = do
  let loop :: [(SwitchHandle, MVar ())]
           -> Callbacks
           -> Counters
           -> Pol
           -> Map Switch [Queue]
           -> [MVar ()]
           -> Either (SwitchHandle, SwitchFeatures) 
                     (Map Switch [Queue],
                      Callbacks, 
                      [(Id, Chan (Loc, ByteString))],
                      Pol)
           -> IO ()
      loop switches callbacks counters pol queues outputs 
           (Left (switch,features)) = do
        let switchId = handle2SwitchID switch
        noticeM "controller" $ "switch " ++ show switchId ++ " connected"
        kill <- newEmptyMVar
        forkIO (handleSwitch server callbacks counters switch pol kill opts)
        createQueuesForSwitch server queues switchId -- TODO: del existing qs?
        let inSwitchEvt = (InSwitchEvt $ SwitchConnected switchId features)
        tryLogIO opts inSwitchEvt
        let outs = evalPol pol inSwitchEvt
        mapM_ (processOut server callbacks counters) outs
        debugM "controller" $ "waiting for program/switch after new switch."
        next <- readChan switchPolicyChan
        loop ((switch,kill):switches) callbacks counters pol
             queues outputs next
      loop switches _ _ _ queues killMVars 
           (Right (queues', callbacks, generators, pol)) = do
        debugM "controller" $ "recv new NetCore program"
        tryLogIO opts pol
        let killVars = killMVars ++ (map (\(_,k) -> k) switches)
        mapM_ (\k -> putMVar k ()) killVars
        mapM_ takeMVar killVars -- wait for termination
        debugM "controller" $ "killed helper threads"
        let switchIds = map (\(h, _) -> handle2SwitchID h) switches
        mapM_ (deleteQueuesForSwitch server queues) switchIds
        mapM_ (createQueuesForSwitch server queues') switchIds
        counters <- initCounters callbacks
        let handle (switch, _) = do
              kill <- newEmptyMVar
              forkIO (handleSwitch server callbacks counters switch pol kill opts)
              return (switch, kill)
        switches' <- mapM handle switches
        killOutputs' <- newEmptyMVar
        forkIO (handlePolicyOutputs server callbacks counters pol generators
                                    killOutputs' opts)
        killCallbackInvokers' <- invokeCallbacksOnTimers counters callbacks
        debugM "controller" $ "waiting for program/switch after new program."
        next <- readChan switchPolicyChan
        loop switches' callbacks counters pol queues'
             [killOutputs', killCallbackInvokers'] next
  v <- readChan switchPolicyChan
  dummyOutput <- newEmptyMVar
  forkIO $ do
    takeMVar dummyOutput
    putMVar dummyOutput ()
    debugM "controller" $ "dummy thead terminated"
  loop [] Map.empty Map.empty PolEmpty Map.empty [dummyOutput] v
  closeServer server

nettleServer :: NettleServerOpts -> Chan Program -> IO ()
nettleServer opts progChan = do
  server <- startOpenFlowServer Nothing 6633
  switchChan <- makeSwitchChan server
  polChan <- mapChan progChan (\prog -> return $ programToSemanticPol prog)
  switchPolChan <- select switchChan polChan
  nettleServer' server opts switchPolChan

nettleRemoteServer :: NettleServerOpts
                   -> Chan (Map Switch [Queue],
                            Callbacks, 
                            [(Id, Chan (Loc, ByteString))],
                            Pol)
                   -> IO ()
nettleRemoteServer opts chan = do
  server <- startOpenFlowServer Nothing 6633
  switchChan <- makeSwitchChan server
  switchPolChan <- select switchChan chan
  nettleServer' server opts switchPolChan

consistentNettleServer :: NettleServerOpts 
                       -> Chan (Policy, SwitchID -> [Word16]) 
                       -> IO ()
consistentNettleServer opts policyChan = do
  server <- startOpenFlowServer Nothing 6633
  switchChan <- makeSwitchChan server
  switchPolicyChan <- select switchChan policyChan
  childChan <- newChan
  let wrapper :: Either (SwitchHandle, SwitchFeatures) Program
              -> IO (Either (SwitchHandle, SwitchFeatures) 
                        (Map Switch [Queue],
                         Callbacks, 
                         [(Id, Chan (Loc, ByteString))],
                         Pol))
      wrapper (Right p) = return (Right $ programToSemanticPol p)
      wrapper (Left x)  = return (Left x)
  liftedChildChan <- mapChan childChan wrapper
  forkIO $ nettleServer' server opts liftedChildChan
  let loop :: [SwitchID]
           -> Word16
           -> Policy
           -> (SwitchID -> [Word16])
           -> Either (SwitchHandle, SwitchFeatures) (Policy, SwitchID -> [Word16])
           -> IO ()
      loop switches ver policy extPorts (Left (switch,features)) = do
        let switchId = handle2SwitchID switch
        let (internal,external) = gen_update_pols policy ver switches extPorts
        debugM "controller" $ "internal policy" ++ (show internal)
        debugM "controller" $ "external policy" ++ (show external)        
        writeChan childChan (Right $ Policy internal)
        writeChan childChan (Right $ Policy (internal <+> external))
        writeChan childChan (Left (switch,features))
        next <- readChan switchPolicyChan
        loop (switchId:switches) ver policy extPorts next
      loop switches ver _ _ (Right (policy, extPorts)) = do
        let (internal,external) = gen_update_pols policy ver switches extPorts
        debugM "controller" $ "internal policy" ++ (show internal)
        debugM "controller" $ "external policy" ++ (show external)        
        writeChan childChan (Right $ Policy internal)
        writeChan childChan (Right $ Policy (internal <+> external))
        next <- readChan switchPolicyChan
        loop switches (ver + 1) policy extPorts next
  v <- readChan switchPolicyChan
  loop [] 1 PoBottom (\x -> []) v
  closeServer server

selectMatches :: [(Match, [Act])] -> Int -> a -> [Match]
selectMatches classifier x _ = map fst (filter hasQuery classifier)
  where hasQuery (_, acts) = any matchingId acts
        matchingId (ActFwd _ _) = False
        matchingId (ActQueryPktCounter y _) = x == y
        matchingId (ActQueryByteCounter y _) = x == y
        matchingId (ActGetPkt _) = False
        matchingId (ActMonSwitch _) = False

runQueryOnSwitch :: OpenFlowServer
                 -> SwitchHandle
                 -> [(Match, [Act])]
                 -> Counters
                 -> Callbacks
                 -> IO (MVar ())
runQueryOnSwitch server switch classifier counters callbacks = do
  let callbackMatches = Map.mapWithKey (selectMatches classifier) 
                          (Map.filter (isJust.callbackInterval) callbacks)
  let getMatches (Right delay) = Right delay
      getMatches (Left (x, _)) = case Map.lookup x callbackMatches of
        Nothing -> error "runQueryOnSwitch: callback not found"
        Just matches -> Left (x, matches)
  let queryStream = map getMatches (callbackDelayStream callbacks)
  let body (Right delay) = do
        threadDelay (delay * 1000)
      body (Left (x, matches)) = do
        let mkReq m = (fromIntegral x, -- Word32
                       StatsRequest (FlowStatsRequest m AllTables Nothing))
        mapM_ (sendToSwitch switch) (map mkReq matches)
  tid <- forkIO $ mapM_ body queryStream
  killMVar <- newEmptyMVar
  forkIO $ do
    takeMVar killMVar
    killThread tid
    putMVar killMVar ()
  return killMVar
