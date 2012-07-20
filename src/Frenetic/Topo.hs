module Frenetic.Topo
  ( Topo
  , buildGraph
  , getEdgeLabel
  , subgraph
  , switches
  , hosts
  , ports
  ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import qualified Data.Set as Set

import Frenetic.NetCore.API

type Topo = Gr () Port

-- | Build a graph from list of undirected edges labeled with their ports
-- Ensures that the resulting graph is undirected-equivalent, and labels each
-- "directed" edge with the appropriate port to send a packet over that edge
-- from the source switch.
--
-- By convention, hosts have a single port 0, and non-hosts have any number of
-- non-zero ports.  If 0 is in the ports of a node, it is considered to be a
-- host regardless of other ports that may be present.
buildGraph :: [((Node, Port), (Node, Port))] -> Topo
buildGraph links = mkGraph nodes edges where
  nodes = Set.toList .
          Set.unions $
          map (\ ((n1, _), (n2, _)) -> Set.fromList [(n1, ()), (n2, ())])
              links
  edges = Set.toList .
          Set.unions $
          map (\ ((n1, p1), (n2, p2)) ->
                 Set.fromList [(n1, n2, p1), (n2, n1, p2)])
              links

-- | Get the subgraph that only contains the nodes matched by the predicate
filterGr :: (LNode a -> Bool) -> Gr a b -> Gr a b
filterGr pred gr = delNodes badNodes gr where
  badNodes = map (\(n, _) -> n) . filter (not . pred) . labNodes $ gr

-- | Get the subgraph only containing nodes
subgraph :: Set.Set Node -> Gr a b -> Gr a b
subgraph nodes gr = filterGr (\(n, _) -> Set.member n nodes) gr

-- | Maybe get the label of the edge from n1 to n2
getEdgeLabel :: Gr a b -> Node -> Node -> Maybe b
getEdgeLabel gr n1 n2 = lookup n2 (lsuc gr n1)

-- | Get the switches of a topology.  A switch is a node with no port 0
switches :: Gr a Port -> [Node]
switches topo = filter (not . isHost topo) $ nodes topo

hostPort :: Port
hostPort = 0
isHost :: Gr a Port -> Node -> Bool
isHost topo node = elem hostPort (ports topo node)

-- | Get the hosts of a topology.  A host is a node with only one port, 0.
hosts :: Gr a Port -> [Node]
hosts topo = filter (isHost topo) $ nodes topo

-- |Get the ports of a switch in the topology.
ports :: Gr a Port -> Node -> [Port]
ports topo n = map snd . lsuc topo $ n
