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

module Topologies where

import Data.Graph.Inductive.Graph (Node, Edge)
import Frenetic.Compat (Port)
import Data.Word

data NodeLabel = Host | Switch
type EdgeLabel = Port

{- A Topology is a directed graph with Host/Switch labels on nodes and Port
labels on edges. -}
class Topology t where
    hosts :: t -> [Node]
    switches :: t -> [Node]
    port :: t -> Node -> Node -> Maybe Port
    links :: t -> [Edge]
    ip :: t -> Node -> Word32
    mkTopo :: Int -> Int -> t
    showT :: t -> String
    showT t = foldl (\s (e1,e2) -> s ++ "\n" ++ show e1 ++ " <-> " ++ show e2) "" $ links t

