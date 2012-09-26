module Frenetic.Topo
  ( Topo
  , buildGraph
  , getEdgeLabel
  , getEdge
  , reverseLoc
  , subgraph
  , isHost
  , switches
  , hosts
  , lPorts
  , ports
  ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Maybe
import qualified Data.List as List
import qualified Data.Set as Set

import Frenetic.NetCore.Types

type Topo = Gr Int Port

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
          map (\ ((n1, _), (n2, _)) -> Set.fromList [(n1,-2), (n2, -2)])
              links
  edges = Set.toList .
          Set.unions $
          map (\ ((n1, p1), (n2, p2)) ->
                 Set.fromList [(n1, n2, p1), (n2, n1, p2)])
              links

-- | Get the subgraph that only contains the nodes matched by the predicate
filterGr :: (Graph gr) => (LNode a -> Bool) -> gr a b -> gr a b
filterGr pred gr = delNodes badNodes gr where
  badNodes = map fst . filter (not . pred) . labNodes $ gr


-- |Get the l-1 nodes of the topology
-- By definition, these are the nodes incident to no cycle in the topology
lOne :: (Graph gr) => gr a b -> gr a b 
lOne gr = 
  let inCycle (v, label) = (foldl (\x -> \y -> if y == v then x+1 else x) 0 (dfs gr (v, label))) > 1 
  in filterGr inCycle gr 

lOneClose :: (Graph gr) => gr a b -> [Node] -> [Node]
lOneClose g nodes = nodes


dfs :: (Graph gr) => gr a b -> LNode a -> [Node]
dfs gr a = 
  let explore path visited (i,l) = 
        case () of  _ 
                      |  (isJust (List.find (\x -> i == x) path)) -> i : visited 
                      |  (isJust (List.find (\x -> i == x) visited)) -> i : visited 
                      |  otherwise -> 
                        let n_path = i : path in
                        let edges = labNodes (filterGr (\x -> isJust (getEdgeLabel gr i (fst x))) gr) in
                          foldl (\x -> \y ->  x  ++ explore n_path visited y) [] edges 
  in explore [] [] a

-- | Get the subgraph only containing nodes
subgraph :: (Graph gr) => Set.Set Node -> gr a b -> gr a b
subgraph nodes gr = filterGr (\(n, _) -> Set.member n nodes) gr

-- | Maybe get the label of the edge from n1 to n2
getEdgeLabel :: (Graph gr) => gr a b -> Node -> Node -> Maybe b
getEdgeLabel gr n1 n2 = lookup n2 (lsuc gr n1)

-- | Put an edge into a normal form (lowest location first)
normal :: (Loc, Loc) -> (Loc, Loc)
normal (l1, l2) = if l1 < l2 then (l1, l2) else (l2, l1)

-- | Get the normalized pair of locations that one location is on
getEdge :: Topo -> Loc -> (Loc, Loc)
getEdge topo loc = normal (loc, reverseLoc topo loc)

-- | Maybe get the reverse of a location
reverseLoc :: Topo -> Loc -> Loc
reverseLoc topo loc@(Loc switch port) =
  Loc (fromIntegral targetSwitch) targetPort
  where
  mTargetSwitch = List.find (\(_, port') -> port == port') $
                  lsuc topo (fromIntegral switch)
  (targetSwitch, _) = case mTargetSwitch of
                        Just s -> s
                        Nothing ->
                          error ("Location invalid: could not find dest for "
                                 ++ show loc ++ " among "
                                 ++ show (labEdges topo))
  mTargetPort = getEdgeLabel topo targetSwitch (fromIntegral switch)
  targetPort = case mTargetPort of
                 Just p -> p
                 Nothing -> error ("Graph not undirected, inverse missing of: "
                            ++ show loc)

-- | Get the switches of a topology.  A switch is a node with no port 0
switches :: (Graph gr) => gr a Port -> [Node]
switches topo = filter (not . isHost topo) $ nodes topo

hostPort :: Port
hostPort = 0
isHost :: (Graph gr) => gr a Port -> Node -> Bool
isHost topo node = elem hostPort (ports topo node)

-- | Get the hosts of a topology.  A host is a node with only one port, 0.
hosts :: (Graph gr) => gr a Port -> [Node]
hosts topo = filter (isHost topo) $ nodes topo

-- |Get the (dest, port) of a switch in the topology
lPorts :: (Graph gr) => gr a Port -> Node -> [(Node, Port)]
lPorts topo n = lsuc topo n

-- |Get the ports of a switch in the topology.
ports :: (Graph gr) => gr a Port -> Node -> [Port]
ports topo = map snd . lPorts topo

