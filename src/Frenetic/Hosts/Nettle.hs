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

import Frenetic.Common
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toChunks)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Frenetic.LargeWord
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

rawClassifier :: [(PatternImpl OpenFlow, ActionImpl OpenFlow)]
              -> [(Match, ActionSequence)]
rawClassifier classifier =
  map (\(p, a) -> (fromOFPat p, fromOFAct a)) classifier

showMatch Match {..} = "Match {" ++ list ++ "}" where
  list = concat . intersperse ", " . catMaybes $ fields
  fields = [ case inPort of
               Nothing -> Nothing
               Just v -> Just $ "inPort = \"" ++ show v ++ "\""
           , case srcEthAddress of
               Nothing -> Nothing
               Just v -> Just $ "srcEthAddress = \"" ++ show v ++ "\""
           , case dstEthAddress of
               Nothing -> Nothing
               Just v -> Just $ "dstEthAddress = \"" ++ show v ++ "\""
           , case vLANID of
               Nothing -> Nothing
               Just v -> Just $ "vLANID = \"" ++ show v ++ "\""
           , case vLANPriority of
               Nothing -> Nothing
               Just v -> Just $ "vLANPriority = \"" ++ show v ++ "\""
           , case ethFrameType of
               Nothing -> Nothing
               Just v -> Just $ "ethFrameType = \"" ++ show v ++ "\""
           , case ipTypeOfService of
               Nothing -> Nothing
               Just v -> Just $ "ipTypeOfService = \"" ++ show v ++ "\""
           , case matchIPProtocol of
               Nothing -> Nothing
               Just v -> Just $ "matchIPProtocol = \"" ++ show v ++ "\""
           , case srcIPAddress of
               v@(_, len) -> if len == 0 then Nothing
                             else Just $ "srcIPAddress = \"" ++ show v ++ "\""
           , case dstIPAddress of
               v@(_, len) -> if len == 0 then Nothing
                             else Just $ "dstIPAddress = \"" ++ show v ++ "\""
           , case srcTransportPort of
               Nothing -> Nothing
               Just v -> Just $ "srcTransportPort = \"" ++ show v ++ "\""
           , case dstTransportPort of
               Nothing -> Nothing
               Just v -> Just $ "dstTransportPort = \"" ++ show v ++ "\""
           ]

prettyClassifier :: (Match, ActionSequence) -> String
prettyClassifier (match, as) = "(" ++ showMatch match ++ ", " ++ show as ++ ")"

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
  policiesAndMessages <- mergeChan policyChan msgChan
  let loop oldPolicy oldThreads (Left policy) = do
        sendToSwitch switch (0, FlowMod (DeleteFlows matchAny Nothing))
        let classifier = compile (handle2SwitchID switch) policy
        let flowTbl = rawClassifier classifier
        infoM "nettle" $ "policy is \n" ++ toString policy ++ 
                         "\n and flow table is " ++ 
                         (concat $ intersperse "\n"
                                     (map prettyClassifier flowTbl))
        mapM_ killThread oldThreads
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
          let t = Transmission (toOFPat (frameToExactMatch inPort frame))
                               switchID
                               (toOFPkt pkt)
          let actions = interpretPolicy policy t
          actnControllerPart (actnTranslate actions) switchID (toOFPkt pkt)
          nextMsg <- readChan policiesAndMessages
          loop policy threads nextMsg
        PacketIn (pkt@(PacketInfo {receivedOnPort=inPort,
                                   reasonSent=NotMatched,
                                   enclosedFrame=Right frame})) -> do
          let t = Transmission (toOFPat (frameToExactMatch inPort frame))
                               switchID
                               (toOFPkt pkt)
          let actions = interpretPolicy policy t
          actnControllerPart (actnTranslate actions) switchID (toOFPkt pkt)
          case bufferID pkt of
            Nothing -> return ()
            Just buf -> do
              let unCtrl (SendOutPort (ToController _)) = False
                  unCtrl _ = True
              let msg = PacketOut $ PacketOutRecord (Left buf) (Just inPort) $
                          (filter unCtrl (fromOFAct $ actnTranslate actions))
              sendToSwitch switch (2, msg)
          nextMsg <- readChan policiesAndMessages
          loop policy threads nextMsg
        otherwise -> do
          nextMsg <- readChan policiesAndMessages
          loop policy threads nextMsg
  loop PoBottom [] (Left initPolicy)

nettleServer :: Chan Policy -> Chan (Loc, ByteString) -> IO ()
nettleServer policyChan pktChan = do
  server <- startOpenFlowServerEx Nothing 6633
  currentPolicy <- newIORef PoBottom
  forkIO $ forever $ do
    pol <- readChan policyChan
    writeIORef currentPolicy pol
  forkIO $ forever $ do
    (Loc swID pt, pkt) <- readChan pktChan
    let msg = PacketOut $ PacketOutRecord (Right (BS.concat . toChunks $ pkt))
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
classifierQueries :: [(PatternImpl OpenFlow, ActionImpl OpenFlow)]
                  -> [(Query, [Match])]
classifierQueries classifier = map sel queries where
  queries = nub (concatMap (actQueries.snd) classifier)
  sel query = (query, map (fromOFPat.fst) (filter (hasQuery query) classifier))
  hasQuery query (_, action) = query `elem` actQueries action

runQueryOnSwitch :: Nettle
                 -> SwitchHandle
                 -> [(PatternImpl OpenFlow, ActionImpl OpenFlow)]
                 -> IO [ThreadId]
runQueryOnSwitch nettle switch classifier = do
  let mkReq m = StatsRequest (FlowStatsRequest m AllTables Nothing)
      switchID = handle2SwitchID switch
      runQuery (NumPktQuery _ outChan millisecondDelay, pats) = do
        let statReqs = map mkReq pats
        tId <- forkIO $ forever $ do
          threadDelay (millisecondDelay * 1000)
          sendTransaction nettle switch statReqs $ \replies -> do
            writeChan outChan (switchID, sum (map getPktCount replies))
        return (Just tId)
      runQuery (PktQuery _ _, pats) = do
        -- nothing to do on the controller until a PacketIn
        return Nothing
  threads <- mapM runQuery (classifierQueries classifier)
  return (catMaybes threads)


getPktCount :: SCMessage -> Integer
getPktCount msg = case msg of
  StatsReply (FlowStatsReply _ stats) -> sum (map flowStatsPacketCount stats)
  otherwise -> 0
