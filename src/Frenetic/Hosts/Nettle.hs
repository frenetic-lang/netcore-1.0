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
-- - Redistributions of source code must retain the above copyright           --
--   notice, this list of conditions and the following disclaimer.            --
-- - Redistributions of binaries must reproduce the above copyright           --
--   notice, this list of conditions and the following disclaimer in          --
--   the documentation or other materials provided with the distribution.     --
-- - The names of the copyright holds and contributors may not be used to     --
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
--------------------------------------------------------------------------------
module Frenetic.Hosts.Nettle where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import Frenetic.LargeWord
import Control.Exception.Base
import Control.Concurrent
import Control.Monad.State
import System.IO
import Nettle.OpenFlow
import Nettle.Servers.Server
import Frenetic.NetCore.API
import Frenetic.NetCore.Semantics
import Frenetic.NetCore.Compiler
import Frenetic.Switches.OpenFlow
import Frenetic.Compat
import Data.List (nub, find)

sendToSwitch' sw msg = do
  sendToSwitch sw msg

-- |spin-lock until we acquire a 'TransactionID'
reserveTxId :: Nettle -> IO TransactionID
reserveTxId nettle@(Nettle _ _ nextTxId _) = do
  let getNoWrap n = case n == maxBound of
        False -> (n + 1, Just n)
        True -> (n, Nothing)
  r <- atomicModifyIORef nextTxId getNoWrap
  case r of
    Just n -> return n
    Nothing -> reserveTxId nettle

releaseTxId :: TransactionID -> Nettle -> IO ()
releaseTxId n (Nettle _ _ nextTxId _) = do
  let release m = case m == n of
        False -> (m, ())
        True -> (m - 1, ())
  atomicModifyIORef nextTxId release

csMsgWithResponse :: CSMessage -> Bool
csMsgWithResponse msg = case msg of
  CSHello -> True
  CSEchoRequest _ -> True
  FeaturesRequest -> True
  StatsRequest _ -> True
  BarrierRequest -> True
  GetQueueConfig _ -> True
  otherwise -> False

hasMoreReplies :: SCMessage -> Bool
hasMoreReplies msg = case msg of
  StatsReply (FlowStatsReply True _) -> True
  StatsReply (TableStatsReply True _) -> True
  StatsReply (PortStatsReply True _) -> True
  StatsReply (QueueStatsReply True _) -> True
  otherwise -> False

sendTransaction :: Nettle
                -> SwitchHandle -- ^target switch
                -> [CSMessage] -- ^related messages
                -> ([SCMessage] -> IO ()) -- ^callback
                -> IO ()
sendTransaction nettle@(Nettle _ _ _ txHandlers) sw reqs callback = do
  txId <- reserveTxId nettle
  resps <- newIORef ([] :: [SCMessage])
  remainingResps  <- newIORef (length (filter csMsgWithResponse reqs))
  let handler msg = do
        modifyIORef resps (msg:) -- Nettle client operates in one thread
        unless (hasMoreReplies msg) $ do
          modifyIORef remainingResps (\x -> x - 1)
          n <- readIORef remainingResps
          when (n == 0) $ do
            resps <- readIORef resps
            atomicModifyIORef txHandlers (\hs -> (Map.delete txId hs, ()))
            releaseTxId txId nettle
            callback resps
  atomicModifyIORef txHandlers (\hs -> (Map.insert txId handler hs, ()))
  mapM_ (sendToSwitch sw) (zip [txId ..] reqs)
  return ()

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

rawClassifier :: Classifier (PatternImpl OpenFlow) (ActionImpl OpenFlow)
              -> [(Match, ActionSequence)]
rawClassifier (Classifier rules) =
  map (\(p, a) -> (fromOFPat p, fromOFAct a)) rules

handleOFMsg :: Nettle
            -> SwitchHandle
            -> Policy
            -> (TransactionID, SCMessage)
            -> IO ()
handleOFMsg nettle switch policy (xid, msg) = case msg of
  PacketIn (pkt@(PacketInfo {receivedOnPort=inPort,
                             enclosedFrame=Right frame})) -> do
    let switchID = handle2SwitchID switch
    let t = Transmission (toOFPat (frameToExactMatch inPort frame))  switchID
                         (toOFPkt pkt)
    let actions = interpretPolicy policy t
    actnControllerPart (actnTranslate actions) switchID (toOFPkt pkt)
    let mod = mkFlowMod (frameToExactMatch inPort frame,
                         fromOFAct $ actnTranslate actions)
                        65535
    
    sendToSwitch' switch (1, mod)
    case bufferID pkt of
      Nothing -> return ()
      Just buf -> do
        let msg = PacketOut $ PacketOutRecord (Left buf) (Just inPort) $
                    (fromOFAct $ actnTranslate actions)
        sendToSwitch' switch (2, msg)
  otherwise -> do
    handlers <- readIORef (txHandlers nettle)
    case Map.lookup xid handlers of
      Just handler -> handler msg
      Nothing -> putStrLn $ "Unhandled message " ++ show msg

-- |Installs the static portion of the policy, then react.
handleSwitch :: Nettle -> SwitchHandle -> Policy -> IO ()
handleSwitch nettle switch policy = do
  -- Nettle handles keep alive
  let classifier@(Classifier cl) = compile (handle2SwitchID switch) policy
  let flowTbl = rawClassifier classifier
  runQueryOnSwitch nettle switch cl
  -- Priority 65535 is for microflow rules from reactive-specialization
  let flowMods = zipWith mkFlowMod flowTbl [65534, 65533 ..]
  mapM_ (sendToSwitch' switch) (zip [0..] flowMods)
  untilNothing (receiveFromSwitch switch) (handleOFMsg nettle switch policy)

nettleServer :: Policy -> IO ()
nettleServer policy = do
  hPutStrLn stderr "--- Welcome to Frenetic ---"
  server <- startOpenFlowServer Nothing -- bind to this address
                                6633    -- port to listen on
  switches <- newIORef Map.empty
  nextTxId <- newIORef 10
  txHandlers <- newIORef Map.empty
  let nettle = Nettle server switches nextTxId txHandlers
  forever $ do
    -- Nettle does the OpenFlow handshake
    (switch, switchFeatures) <- acceptSwitch server
    modifyIORef switches (Map.insert (handle2SwitchID switch) switch)
    forkIO (handleSwitch nettle switch policy)
  closeServer server

-- The classifier (1st arg.) pairs Nettle patterns with Frenetic actions.
-- The actions include queries that are not realizable on switches.
-- We assume the classifier does not have any fully-shadowed patterns.
classifierQueries :: [(PatternImpl OpenFlow, ActionImpl OpenFlow)]
                  -> [(Query, [Match])]
classifierQueries classifier = map sel queries where
  queries = nub (concatMap (actQueries.snd) classifier)
  sel query = (query, map (fromOFPat.fst) (filter (hasQuery query) classifier))
  hasQuery query (_, action) = query `elem` actQueries action

runQueryOnSwitch :: Nettle
                 -> SwitchHandle
                 -> [(PatternImpl OpenFlow, ActionImpl OpenFlow)]
                 -> IO ()
runQueryOnSwitch nettle switch classifier =
  mapM_ runQuery (classifierQueries classifier)
    where mkReq m = StatsRequest (FlowStatsRequest m AllTables Nothing)
          switchID = handle2SwitchID switch
          runQuery (NumPktQuery outChan millisecondDelay, pats) = do
            let statReqs = map mkReq pats
            forkIO $ forever $ do
              threadDelay (millisecondDelay * 1000)
              sendTransaction nettle switch statReqs $ \replies -> do
              writeChan outChan (switchID, sum (map getPktCount replies))
            return ()
          runQuery (PktQuery _, pats) = do
            -- nothing to do on the controller until a PacketIn
            return ()

getPktCount :: SCMessage -> Integer
getPktCount msg = case msg of
  StatsReply (FlowStatsReply _ stats) -> sum (map flowStatsPacketCount stats)
  otherwise -> 0
