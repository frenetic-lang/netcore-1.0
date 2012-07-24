module Frenetic.TopoGen
  ( linear
  , linearHosts
  , kComplete
  ) where

import Frenetic.Topo
import qualified Data.List as List

import Frenetic.NetCore.API
import Data.Graph.Inductive.Graph

-- | Produce a topology with n nodes in a row, port 1 pointing to the previous
-- and port 2 pointing to the next, starting at node 0.
linear :: (Integral n) => n -> Topo
linear n = graph where
  nodes = [0 .. ((fromIntegral n - 1))]
  pairs = zip nodes (tail nodes)
  graph = buildGraph . map (\(n1, n2) -> ((n1, 2), (n2, 1))) $ pairs

-- | Produce a topology with n nodes in a row, port 1 pointing to the previous
-- and port 2 pointing to the next, starting at node 0.  Hosts are numbered 100
-- + (10 * switch) and 101 + (10 * switch).  Does not support more than 10
-- switches.  Port 3 connects to host XX0, port 4 connects to host XX1.
linearHosts :: (Integral n) => n -> Topo
linearHosts n = graph where
  nodes = [0 .. (fromIntegral n - 1)]
  hostLinks = concatMap buildHosts nodes
  pairs = zip nodes (tail nodes)
  graph = buildGraph $ (map (\(n1, n2) -> ((n1, 2), (n2, 1))) pairs) ++ hostLinks
  buildHosts node = [((node, 3), (100 + 10 * node, 0)),
                     ((node, 4), (101 + 10 * node, 0))]

kComplete :: (Integral n) => n -> Topo
-- |Produce a topology on the n-complete graph, starting with node 1.  Each node
-- x has an edge to node y over port y.
kComplete n = buildGraph pairs where
  nodes :: [Node]
  nodes = [1 .. (fromIntegral n)]
  pairs = [((fromIntegral x, (fromIntegral y)),
            (fromIntegral y, (fromIntegral x)))
          | (x:xs) <- List.tails nodes, y <- xs]
