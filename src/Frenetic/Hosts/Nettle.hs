module Frenetic.Hosts.Nettle where

import Frenetic.Common
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Exception.Base
import Control.Monad.State
import System.IO
import Frenetic.NetCore.Pretty
import Frenetic.NetCore.Types
import Frenetic.NetCore.Semantics
import Frenetic.NetCore.Compiler
import Frenetic.Switches.OpenFlow
import Frenetic.Compat
import Data.List (nub, find, intersperse)
import Frenetic.NettleEx
import qualified Frenetic.NetCore.Types as NetCore

useInPort (Just pt) (SendOutPort (PhysicalPort pt'))
  | pt == pt' = SendOutPort InPort
  | otherwise = SendOutPort (PhysicalPort pt')
useInPort _ act = act

mkFlowMod :: (Match, ActionSequence)
          -> Priority
          -> CSMessage
mkFlowMod (pat, acts) pri = FlowMod AddFlow {
  match=pat,
  priority=pri,
  actions=map (useInPort (inPort pat)) acts,
  cookie=0,
  notifyWhenRemoved=False,
  idleTimeOut=Permanent,
  hardTimeOut=Permanent,
  applyToPacket=Nothing,
  overlapAllowed=True
}

rawClassifier :: [(Match, ActionImpl)]
              -> [(Match, ActionSequence)]
rawClassifier classifier =
  map (\(p, a) -> (p, fromOFAct a)) classifier

prettyClassifier :: (Match, ActionSequence) -> String
prettyClassifier (match, as) = "(" ++ show match ++ ", " ++ show as ++ ")"

-- |Installs the static portion of the policy, then react.
handleSwitch :: Nettle
             -> SwitchHandle
             -> Policy
             -> Chan Policy
             -> Chan (TransactionID, SCMessage)
             -> IO ()
handleSwitch nettle switch initPolicy policyChan msgChan = do
  -- 1. Clear the flow table
  -- 2. In parallel:
  --    a. Receive a message from the switch
  --    b. Receive a policy
  -- 3. Keep the last policy in an accumulator
  -- 4. On new message, evaluate it with the last policy (not consistent
  --    update!)
  -- 5. On new policy, update the switch and accumulator
  let switchID = handle2SwitchID switch
  policiesAndMessages <- select policyChan msgChan
  let loop oldPolicy oldThreads (Left policy) = do
        sendToSwitch switch (0, FlowMod (DeleteFlows matchAny Nothing))
        let classifier = compile (handle2SwitchID switch) policy
        let flowTbl = rawClassifier classifier
        debugM "nettle" $ "policy is " ++ toString policy ++
                          " and flow table is " ++
                          concat (intersperse ", "
                                    (map prettyClassifier flowTbl))
        oldThreads
        newThreads <- runQueryOnSwitch nettle switch classifier
        -- Priority 65535 is for microflow rules from reactive-specialization
        let flowMods = zipWith mkFlowMod flowTbl  [65534, 65533 ..]
        mapM_ (sendToSwitch switch) (zip [0,0..] flowMods)
        debugM "nettle" $ "finished reconfiguring switch " ++
                          show (handle2SwitchID switch)
        nextMsg <- readChan policiesAndMessages

        loop policy newThreads nextMsg
      loop policy threads (Right (xid, msg)) = case msg of
        PacketIn (pkt@(PacketInfo {receivedOnPort=inPort,
                                   reasonSent=ExplicitSend,
                                   enclosedFrame=Right frame})) -> do
          case toPacket pkt of
            Nothing -> return ()
            Just pk -> do
              let t = Transmission (frameToExactMatch inPort frame) switchID pk
              let actions = interpretPolicy policy t
              actnControllerPart (actnTranslate actions) switchID (pkt)
          nextMsg <- readChan policiesAndMessages
          loop policy threads nextMsg
        PacketIn (pkt@(PacketInfo {receivedOnPort=inPort,
                                   reasonSent=NotMatched,
                                   enclosedFrame=Right frame})) -> do
          case toPacket pkt of
            Nothing -> return ()
            Just pk -> do
              let t = Transmission (frameToExactMatch inPort frame) switchID pk
              let actions = interpretPolicy policy t
              actnControllerPart (actnTranslate actions) switchID (pkt)
              case bufferID pkt of
                Nothing -> return ()
                Just buf -> do
                  let unCtrl (SendOutPort (ToController _)) = False
                      unCtrl _ = True
                  let msg = PacketOut $
                              PacketOutRecord (Left buf) (Just inPort)
                                (filter unCtrl (fromOFAct $ actnTranslate actions))
                  sendToSwitch switch (2, msg)
          nextMsg <- readChan policiesAndMessages
          loop policy threads nextMsg
        otherwise -> do
          nextMsg <- readChan policiesAndMessages
          loop policy threads nextMsg
  loop PoBottom (return ()) (Left initPolicy)

nettleServer :: Chan Policy -> Chan (Loc, ByteString) -> IO ()
nettleServer policyChan pktChan = do
  server <- startOpenFlowServerEx Nothing 6633
  currentPolicy <- newIORef PoBottom
  forkIO $ forever $ do
    pol <- readChan policyChan
    writeIORef currentPolicy pol
  forkIO $ forever $ do
    (Loc swID pt, pkt) <- readChan pktChan
    let msg = PacketOut $ PacketOutRecord (Right pkt)
                                          Nothing (sendOnPort pt)
    sendToSwitchWithID server swID (0,msg)
  forever $ do
    -- Nettle does the OpenFlow handshake
    (switch, switchFeatures, msgChan) <- acceptSwitch server
    noticeM "controller" $ "switch " ++ show (handle2SwitchID switch) ++
                           " connected"
    -- reading from a channel removes the messages. We need to dupChan for
    -- each switch.
    switchPolicyChan <- dupChan policyChan
    initPolicy <- readIORef currentPolicy
    forkIO (handleSwitch server switch initPolicy switchPolicyChan msgChan)
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

runQueryOnSwitch :: Nettle
                 -> SwitchHandle
                 -> [(Match, ActionImpl)]
                 -> IO (IO ())
runQueryOnSwitch nettle switch classifier = do
  killFlag <- newIORef False
  let mkReq m = StatsRequest (FlowStatsRequest m AllTables Nothing)
      switchID = handle2SwitchID switch
      runQuery (PktQuery _ _, pats) = do
        -- nothing to do on the controller until a PacketIn
        return ()
      runQuery (NumPktQuery qid outChan msDelay counter totalRef lastRef,
                pats) = do
        let statReqs = map mkReq pats
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
              writeChan outChan (switchID, total')
        return ()
  mapM_ runQuery (classifierQueries classifier)
  return $ do
    writeIORef killFlag True


getCount :: Counter -> SCMessage -> Integer
getCount counter msg = case msg of
  StatsReply (FlowStatsReply _ stats) -> case counter of
    CountPackets -> sum (map flowStatsPacketCount stats)
    CountBytes -> sum (map flowStatsByteCount stats)
  otherwise -> 0

