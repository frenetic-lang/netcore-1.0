module Frenetic.Hosts.Nettle where

import Frenetic.Common
import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Exception.Base
import Control.Monad.State
import System.IO
import Frenetic.NetCore.Pretty
import Frenetic.NetCore.Types
import Frenetic.NetCore.Util
import Frenetic.NetCore.Short (synthRestrict)
import Data.Binary (decode)
import Frenetic.NetCore.Semantics
import Frenetic.NetCore.Compiler
import Frenetic.Switches.OpenFlow
import Data.List (nub, find, intersperse)
import Frenetic.NettleEx
import qualified Frenetic.NetCore.Types as NetCore

data Counters = Counters {
  totalVal :: IORef Integer,
  lastVal :: IORef Integer
}

data Counter = PktCounter | ByteCounter

newCounters :: IO Counters
newCounters = do
  total <- newIORef 0
  last <- newIORef 0
  return (Counters total last)

mkFlowMod :: (Match, ActionSequence)
          -> Priority
          -> CSMessage
mkFlowMod (pat, acts) pri = FlowMod AddFlow {
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

prettyClassifier :: (Match, ActionSequence) -> String
prettyClassifier (match, as) = "(" ++ show match ++ ", " ++ show as ++ ")"

isNotToController (SendOutPort (ToController _)) = False
isNotToController _ = True


policyOutputs :: Policy -> [(Chan (Loc, ByteString), Predicate)]
policyOutputs PoBottom = []
policyOutputs (PoBasic _ _) = []
policyOutputs (PoUnion pol1 pol2) = policyOutputs pol1 ++ policyOutputs pol2
policyOutputs (Restrict (SendPackets chan) pred) = [(chan, pred)]
policyOutputs (Restrict pol pred) = policyOutputs (synthRestrict pol pred)
policyOutputs (SendPackets chan) = [(chan, Any)]

mkOutputThread :: Nettle -> (Chan (Loc, ByteString), Predicate) -> IO ThreadId
mkOutputThread nettle (chan, pred) = forkIO $ forever $ do
  (loc@(Loc switch port), pktBytes) <- readChan chan
  let len = fromIntegral (BS.length pktBytes) -- TODO(arjun): overflow
  let body = Right (decode pktBytes)
  case toPacket (PacketInfo Nothing len port ExplicitSend pktBytes body) of
    Nothing -> warningM "nettle" $ "dropping unparsable packet being sent to "
                 ++  show loc
    Just pkt -> case interpretPredicate pred (loc, pkt) of
      True -> do
        let msg = PacketOut $ 
                    PacketOutRecord (Right pktBytes) Nothing (sendOnPort port)
        sendToSwitchWithID nettle switch (0, msg)
      False -> infoM "nettle" $ "dropping packet due to policy restriction"

handlePolicyOutputs :: Nettle -> Policy -> MVar () -> IO ()
handlePolicyOutputs nettle pol kill = do
  let outputs = policyOutputs pol
  childTids <- mapM (mkOutputThread nettle) outputs
  readMVar kill
  mapM_ killThread childTids
  putMVar kill ()

-- |Installs the static portion of the policy, then react.
handleSwitch :: Nettle
             -> SwitchHandle
             -> Policy
             -> MVar ()
             -> Chan (TransactionID, SCMessage)
             -> IO ()
handleSwitch nettle switch policy kill msgChan = do
  -- 1. Clear the flow table
  -- 2. In parallel:
  --    a. Receive a message from the switch
  --    b. Receive a policy
  -- 3. Keep the last policy in an accumulator
  -- 4. On new message, evaluate it with the last policy (not consistent
  --    update!)
  -- 5. On new policy, update the switch and accumulator
  let switchID = handle2SwitchID switch
  killChan <- newChan
  killThreadId <- forkIO $ do
    v <- readMVar kill
    writeChan killChan v
  let classifier = compile (handle2SwitchID switch) policy
  let flowTbl = toFlowTable classifier
  debugM "nettle" $ "flow table is " ++ show flowTbl
  killChildThreads <- runQueryOnSwitch nettle switch classifier
  -- Priority 65535 is for microflow rules from reactive-specialization
  let flowMods = zipWith mkFlowMod flowTbl  [65534, 65533 ..]
  mapM_ (sendToSwitch switch) (zip [0,0..] flowMods)
  killOrMsg <- select killChan msgChan
  forever $ do
    v <- readChan killOrMsg
    case v of
      Left () -> do
        killChildThreads
        putMVar kill ()
        tid <- myThreadId
        killThread tid
      Right (xid, msg) ->  case msg of
        PacketIn (pkt@(PacketInfo {receivedOnPort=inPort, 
                                   reasonSent=reason,
                                   enclosedFrame=Right frame})) -> do
          let loc = Loc switchID inPort
          case toPacket pkt of
            Nothing -> return ()
            Just pk -> do
              let actions = interpretPolicy policy (loc, pk)
              let ofActions = actnTranslate actions
              actnControllerPart ofActions loc pkt
              case reason of
                ExplicitSend -> return ()
                NotMatched -> case bufferID pkt of
                  Nothing -> return () -- odd case
                  Just buf -> do
                    let msg = PacketOutRecord (Left buf) (Just inPort)
                                (filter isNotToController (fromOFAct ofActions))
                    sendToSwitch switch (2, PacketOut msg)

deleteQueue :: Nettle -> Queue -> IO ()
deleteQueue nettle (Queue switch port queue _) = do
  sendToSwitchWithID nettle switch
    (0, ExtQueueDelete port [QueueConfig queue []])

createQueue :: Nettle -> Queue -> IO ()
createQueue nettle (Queue switch port queue minRate) = do
  let cfg = MinRateQueue (Enabled minRate)
  sendToSwitchWithID nettle switch
    (0, ExtQueueModify port [QueueConfig queue [cfg]])

nettleServer :: Chan Program -> IO ()
nettleServer policyChan = do
  server <- startOpenFlowServerEx Nothing 6633
  switchChan <- makeSwitchChan server
  switchPolicyChan <- select switchChan policyChan
  let loop :: [(SwitchHandle, Chan (TransactionID, SCMessage), MVar ())]
           -> Policy
           -> [Queue]
           -> MVar ()
           -> Either (SwitchHandle, 
                     SwitchFeatures, Chan (TransactionID, SCMessage))
                     Program
           -> IO ()
      loop switches policy queues outputs (Left (switch,features,msgChan)) = do
        noticeM "controller" $ "switch " ++ show (handle2SwitchID switch) ++
                " connected"
        kill <- newEmptyMVar
        forkIO (handleSwitch server switch policy kill msgChan)
        next <- readChan switchPolicyChan
        loop ((switch,msgChan,kill):switches) policy queues outputs next
      loop switches _ queues killOutputs (Right program) = do
        let (queues', policy) = evalProgram program
        let killVars = map (\(_,_,k) -> k) switches
        mapM_ (\k -> putMVar k ()) (killOutputs:killVars) -- kill all handlers
        mapM_ readMVar (killOutputs:killVars) -- wait for termination
        mapM_ (deleteQueue server) queues
        mapM_ (createQueue server) queues'
        let handle (switch, msgChan, _) = do
              kill <- newEmptyMVar
              forkIO (handleSwitch server switch policy kill msgChan)
              return (switch, msgChan, kill)
        switches' <- mapM handle switches
        killOutputs' <- newEmptyMVar
        forkIO (handlePolicyOutputs server policy killOutputs)
        next <- readChan switchPolicyChan
        loop switches' policy queues' killOutputs' next
  v <- readChan switchPolicyChan
  dummyOutput <- newEmptyMVar
  forkIO $ do
    readMVar dummyOutput
    putMVar dummyOutput ()
  loop [] PoBottom [] dummyOutput v
  closeServer server

-- The classifier (1st arg.) pairs Nettle patterns with Frenetic actions.
-- The actions include queries that are not realizable on switches.
-- We assume the classifier does not have any fully-shadowed patterns.
classifierQueries :: [(Match, ActionImpl)]
                  -> [(NetCore.Action, [Match])]
classifierQueries classifier = map sel queries where
  queries = nub (concatMap (actQueries.snd) classifier)
  sel query = (query, map fst (filter (hasQuery query) classifier))
  hasQuery query (_, action) = query `elem` actQueries action

runCounterQuery killFlag nettle switch ctrlAction msDelay pats counter = do
  (Counters totalRef lastRef) <- newCounters
  let mkReq m = StatsRequest (FlowStatsRequest m AllTables Nothing)
  let statReqs = map mkReq pats
  let switchID = handle2SwitchID switch
  forkIO $ forever $ do
    tid <- myThreadId
    threadDelay (msDelay * 1000)
    sendTransaction nettle switch statReqs $ \replies -> do
      count <- readIORef lastRef
      let count' = sum (map (getCount counter) replies)
      total <- readIORef totalRef
      let total' = total + (count' - count)
      kill <- readIORef killFlag
      if kill then do
        writeIORef lastRef 0 -- assumes all rules evicted
        killThread tid
      else do
        writeIORef lastRef count'
        writeIORef totalRef total'
        ctrlAction (switchID, total')
  return ()

runQueryOnSwitch :: Nettle
                 -> SwitchHandle
                 -> [(Match, ActionImpl)]
                 -> IO (IO ())
runQueryOnSwitch nettle switch classifier = do
  killFlag <- newIORef False
  let runQuery (GetPacket _ _, pats) = do
        -- nothing to do on the controller until a PacketIn
        return ()
      runQuery (CountPackets _ msDelay ctrlAction, pats) =
        runCounterQuery killFlag nettle switch ctrlAction msDelay pats
                        PktCounter
      runQuery (CountBytes _ msDelay ctrlAction, pats) =
        runCounterQuery killFlag nettle switch ctrlAction msDelay pats 
                        ByteCounter

  mapM_ runQuery (classifierQueries classifier)
  return $ do
    writeIORef killFlag True


getCount :: Counter -> SCMessage -> Integer
getCount counter msg = case msg of
  StatsReply (FlowStatsReply _ stats) -> case counter of
    PktCounter -> sum (map flowStatsPacketCount stats)
    ByteCounter -> sum (map flowStatsByteCount stats)
  otherwise -> 0

