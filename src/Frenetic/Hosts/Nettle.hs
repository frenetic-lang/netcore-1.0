
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
import Frenetic.NetCore.Compiler
import Frenetic.Switches.OpenFlow
import Frenetic.Compat

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

handleOFMsg :: SwitchHandle -> Policy -> (TransactionID, SCMessage) -> IO ()
handleOFMsg switch policy (xid, msg) = case msg of
  PacketIn (pkt@(PacketInfo {receivedOnPort=inPort,
                             enclosedFrame=Right frame})) -> do
    let switchID = handle2SwitchID switch
    let t = Transmission undefined switchID (toOFPkt pkt)
    let t' = Transmission (toOFPat (frameToExactMatch inPort frame)) 
                          switchID 
                          (toOFPkt pkt)
    let flowTbl = rawClassifier (specialize t policy)
    let flowMods = zipWith mkFlowMod flowTbl [65535, 65534 ..]
    mapM_ (sendToSwitch switch) (zip [1 ..] flowMods)
    case bufferID pkt of
      Nothing -> return ()
      Just buf -> do
        let (actions, _ {- spurious -}) = interpretPolicy policy t'
        let msg = PacketOut $ PacketOutRecord (Left buf) (Just inPort) $
                    (fromOFAct $ actnTranslate actions)
        sendToSwitch switch (2, msg)
  otherwise -> do
    putStrLn $ "Unhandled message " ++ show msg
      
-- |Installs the static portion of the policy, then react.
handleSwitch :: SwitchHandle -> Policy -> IO ()
handleSwitch switch policy = do
  -- Nettle handles keep alive
  let flowTbl = rawClassifier (compile (handle2SwitchID switch) policy)
  let flowMods = zipWith mkFlowMod flowTbl [65535 ..]
  mapM_ (sendToSwitch switch) (zip [0..] flowMods)
  untilNothing (receiveFromSwitch switch) (handleOFMsg switch policy)

nettleServer :: Policy -> IO ()
nettleServer policy = do 
  hPutStrLn stderr "--- Welcome to Frenetic ---"
  server <- startOpenFlowServer Nothing -- bind to this address
                                6633    -- port to listen on
  forever $ do
    -- Nettle does the OpenFlow handshake
    (switch, switchFeatures) <- acceptSwitch server
    forkIO (handleSwitch switch policy)
  closeServer server
