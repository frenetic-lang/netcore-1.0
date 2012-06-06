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
-- * The names of the copyright holds and contributors may not be used to     -- --   endorse or promote products derived from this work without specific      --
--   prior written permission.                                                --
--                                                                            --
-- Unless required by applicable law or agreed to in writing, software        --
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  --
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   --
-- LICENSE file distributed with this work for specific language governing    --
-- permissions and limitations under the License.                             --
--------------------------------------------------------------------------------
-- /testsuite/examples/repeater.hs                                            --
-- Single-switch repeater example                                             --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}

import Frenetic.Server
import Frenetic.NetCore.API
import Frenetic.Compat
import Frenetic.Pattern
import Data.Set

import Topologies
import qualified Policies.ShortestPath as SP

import System.IO (readFile)
import Debug.Trace (trace, traceShow)

main = freneticServer $ PoUnion policy3 arp_policy
--main = do
--  topoInput <- readFile "../topologies/repeater_topo.top"
--  let topo :: Topology
--      topo = parseTopo topoInput
--  let policy = SP.mkPolicy topo
--  traceShow policy $ freneticServer policy

policy2 = PoUnion 
           (PoBasic (PrPattern top{ptrnInPort = (Just 1)})
                    (singleton $ Forward 2))
           (PoBasic (PrPattern top{ptrnInPort = (Just 2)})
                    (singleton $ Forward 1))

policy3 = PoUnion 
           (PoBasic (PrPattern top{ptrnNwSrc = h1, ptrnNwDst = h2})
                    (singleton $ Forward 2))
           (PoBasic (PrPattern top{ptrnNwSrc = h2, ptrnNwDst = h1})
                    (singleton $ Forward 1))
    where h1 = Wildcard 0xA000001 0
          h2 = Wildcard 0xA000002 0

arp_policy :: Policy
arp_policy = PoBasic (PrPattern top{ptrnDlTyp = arpType}) $ singleton Flood
    where arpType = Wildcard 0x806 0

flood_policy :: Policy
flood_policy = PoBasic (PrPattern top) (singleton Flood)

