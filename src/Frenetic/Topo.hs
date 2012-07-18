module Frenetic.Topo
  ( Topo
  , buildGraph
  , getEdgeLabel
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

-- | Maybe get the label of the edge from n1 to n2
getEdgeLabel :: Gr a b -> Node -> Node -> Maybe b
getEdgeLabel gr n1 n2 = lookup n2 (lsuc gr n1)
