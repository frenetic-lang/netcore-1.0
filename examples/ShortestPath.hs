module ShortestPath
       where

import qualified Debug.Trace as Trace
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe
import Frenetic.NetCore
import Frenetic.NetCore.Semantics
import Frenetic.Topo
import Frenetic.TopoParser

type Node = Int

adjacent :: Topo -> Node -> Node -> Bool
adjacent gr n1 n2 =
  Data.Maybe.isJust (getEdgeLabel gr n1 n2)

set_up_distances :: Topo -> Map.Map (Node, Node) (Maybe Int)
set_up_distances graph =
  let nodes = switches graph in
  foldl (\m n1 -> foldl (\m n2 ->
                          let val =
                                if n1 == n2 then Just 0 else
                                  if adjacent graph n1 n2 then Just 1 else
                                    Nothing in
                          Map.insert (n1, n2) val m) m nodes)
  Map.empty nodes
  
set_up_nexts :: Topo -> Map.Map (Node, Node) (Maybe Node)
set_up_nexts graph =
  let nodes = switches graph in
  foldl (\m n1 ->
          foldl (\m n2 ->
                  Map.insert (n1, n2) Nothing m) m nodes) Map.empty nodes

floydWarshall :: Topo -> (Map.Map (Node, Node) (Maybe Int), Map.Map (Node, Node) (Maybe Node))
floydWarshall graph =
  let innerLoop (dists, nexts, knode, inode) jnode =
        --Note that the default case below should never hapen
        let distIJ = Map.findWithDefault Nothing (inode, jnode) dists
            distIK = Map.findWithDefault Nothing (inode, knode) dists
            distKJ = Map.findWithDefault Nothing (knode, jnode) dists
            curNext = Map.findWithDefault Nothing (inode, jnode) nexts in
        let newval (Just ij) (Just ik) (Just kj) =
              if ij > ik + kj then (Just (ik + kj), Just knode)
              else (Just ij, curNext)
            newval Nothing (Just ik) (Just kj) = (Just (ik + kj), Just knode)
            newval (Just ij) _ _ = (Just (ij), curNext)
            newval _ _ _ = (Nothing, curNext) in
        let (newDist, newNext) = newval distIJ distIK distKJ in
        (Map.insert (inode, jnode) newDist dists,
         Map.insert (inode, jnode) newNext nexts, knode, inode)
      middleLoop (dists, nexts, knode) inode =
        let (dists2, nexts2, knode2, inode2) =
              foldl innerLoop (dists, nexts, knode, inode) nodes in
        (dists2, nexts2, knode2)
      outerLoop (dists, nexts) knode =
        let (dists2, nexts2, knode2) =
              foldl middleLoop (dists, nexts, knode) nodes in
        (dists2, nexts2)
      nodes = switches graph in
  foldl outerLoop (set_up_distances graph, set_up_nexts graph) nodes

{- Finds the shortest path from one node to another using the results
   of the floydWarshall methdod.  The path is a maybe list of
   the next node to go to and the port to get there.
-}
findPath :: Topo ->
                 (Map.Map (Node,Node) (Maybe Int),
                  Map.Map (Node,Node) (Maybe Node))
                 -> Node -> Node-> Maybe [(Node, Port)]
findPath graph (dists, nexts) fromNode toNode = 
  if Data.Maybe.isNothing (Map.findWithDefault Nothing (fromNode, toNode) dists)
  then Nothing else
    (Just (processPath graph fromNode (findPathHelper nexts fromNode toNode)))
                                                                                        
findPathHelper :: Map.Map (Node, Node) (Maybe Node) -> Node -> Node-> [Node]
findPathHelper nexts fromNode toNode = 
  case Map.findWithDefault Nothing (fromNode, toNode) nexts of
    Just n -> (findPathHelper nexts fromNode n) ++
              (findPathHelper nexts n toNode) 
    Nothing -> [toNode]
               
processPath :: Topo -> Node -> [Node] -> [(Node, Port)]
processPath graph start nodeList =
  let (newLst, _) = foldl (\(lst, prev) node ->
                            case getEdgeLabel graph prev node of
            Just b -> ((prev, b):lst, node)
            Nothing -> ([], node) --Shoudn't happen, all edges should exist
        ) ([], start) nodeList in
  reverse newLst

--Assumes that hosts have exactly one port, port 0, as definined in Topo.
makePolicy :: Topo -> Policy
makePolicy graph =
  let (dists, nexts) = floydWarshall graph
      hostList = hosts graph
      updateList lst host ((s, p):t) = 
        case getEdgeLabel graph s host of
          Just port -> (host,s,port):lst
          _ -> lst
      updateList lst host [] = lst
      hostSwitchList =
        foldl (\lst h -> updateList lst h (lPorts graph h)) [] hostList in
  let pol = foldl (\policy (h1, n1, p1) ->
          foldl (\policy (h2, n2, p2) ->
                  if n1 == n2 then policy else
                    let path = findPath graph (dists, nexts) n1 n2 in
                    case path of
                      Nothing -> policy
                      Just p->
                        foldl (\policy (ni, pi) ->
                                PoBasic
                                (DlSrc (EthernetAddress (fromIntegral h1)) `And`
                                 DlDst (EthernetAddress (fromIntegral h2)) `And`
                                 Switch (fromIntegral ni))
                                [Forward (Physical pi) unmodified]
                                `PoUnion` policy)
                        policy p) policy hostSwitchList)
            PoBottom hostSwitchList in
  foldl (\policy (host, node, port) ->
          PoBasic (DlDst (EthernetAddress (fromIntegral host))
                   `And` Switch (fromIntegral node))
          [Forward (Physical port) unmodified]
          `PoUnion` policy) pol hostSwitchList

--sample data from the net command of mininet
netOutput :: String
netOutput = "s5 <-> s6-eth3 s7-eth3\n" ++
            "s6 <-> h1-eth0 h2-eth0 s5-eth1\n" ++
            "s7 <-> h3-eth0 h4-eth0 s5-eth2"

--If the network could not be parsed, make an empty graph
buildTopo (Left(error)) = buildGraph []
buildTopo (Right(list)) = buildGraph (makeEdgeList list)
  
  
main = controller (makePolicy (buildTopo (parseTopo netOutput)))