-------------------------------------------------------------------------------
-- The Frenetic Project                                                       -
-- frenetic@frenetic-lang.org                                                 -
-------------------------------------------------------------------------------
-- Licensed to the Frenetic Project by one or more contributors. See the      -
-- NOTICE file distributed with this work for additional information          -
-- regarding copyright and ownership. The Frenetic Project licenses this      -
-- file to you under the following license.                                   -
--                                                                            -
-- Redistribution and use in source and binary forms, with or without         -
-- modification, are permitted provided the following conditions are met:     -
-- - Redistributions of source code must retain the above copyright           -
--   notice, this list of conditions and the following disclaimer.            -
-- - Redistributions in binary form must reproduce the above copyright        -
--   notice, this list of conditions and the following disclaimer in          -
--   the documentation or other materials provided with the distribution.     -
-- - The names of the copyright holds and contributors may not be used to     -
--   endorse or promote products derived from this work without specific      -
--   prior written permission.                                                -
--                                                                            -
-- Unless required by applicable law or agreed to in writing, software        -
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  -
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   -
-- LICENSE file distributed with this work for specific language governing    -
-- permissions and limitations under the License.                             -
-------------------------------------------------------------------------------
-- /test-suite/examples/Topologies.hs                                         -
-------------------------------------------------------------------------------

{-# LANGUAGE
    TypeSynonymInstances
 #-}

module Topologies.Repeater where

import qualified Topologies as TS

import Data.Bits
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Frenetic.Compat (Port)

-- Topology for a single repeater switch connecting two hosts:
--      h1 -- switch -- h2
type Topology = (Gr TS.NodeLabel TS.EdgeLabel)
instance TS.Topology Topology where
    hosts g = map fst $ filter f $ labNodes g
        where f (_, ntype) = case ntype of
                                 TS.Host -> True
                                 _ -> False

    switches g = map fst $ filter f $ labNodes g
        where f (_, ntype) = case ntype of
                                 TS.Switch -> True
                                 _ -> False

    links g = edges g

    port g n1 n2 = foldl f Nothing $ lsuc' $ context g n1
        where f Nothing (n, port) | n == n2 = Just port
                                  | otherwise = Nothing
              f (Just p) _ = Just p

    -- ip g n = 10.0.0.0 .|. n
    ip g n = 167772160 .|. fromIntegral n

    mkTopo numHosts numSwitches = mkGraph lNodes lEdges
        where lNodes = map (\n -> (n, TS.Host)) hosts ++ map (\n -> (n, TS.Switch)) switches
              hosts = [1, 2]
              switches = [101]
              lEdges = [(1,101,2), (101,1,1), (2,101,1), (101,2,1)]


