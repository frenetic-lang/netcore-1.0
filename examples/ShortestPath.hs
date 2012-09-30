module ShortestPath
       where

import qualified Debug.Trace as Trace
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe
import Frenetic.NetCore
import Frenetic.NetCore.Semantics

data Edge = Edge {dest :: Switch,
                  fport :: Port,
                  bport :: Port} deriving Eq
            
instance Show Edge where
  show e = show (dest e, fport e, bport e)

instance Ord Edge where
  compare e1 e2 = let dest1 = dest e1
                      dest2 = dest e2 in
                  if dest1 < dest2 then LT else
                    if dest1 > dest2 then GT else
                      EQ

data Node = Node {edges :: Set.Set Edge,
                  ip :: Int,
                  mac :: Switch} deriving (Show, Eq)

type Host = EthernetAddress

instance Ord Node where
  compare n1 n2 = let mac1 = mac n1
                      mac2 = mac n2 in
                  if mac1 < mac2 then LT else
                    if mac1 > mac2 then GT else
                      EQ


type Graph = Set.Set Node

tree1 = addNode empty 5 5 
tree2 = addNode tree1 6 6 
tree3 = addNode tree2 7 7
tree4 = addEdgeMac tree3 5 6 1 3
tree = addEdgeMac tree4 5 7 2 3
Just n5 = findNode tree 5
Just n6 = findNode tree 6
Just n7 = findNode tree 7

treeHostList = [(EthernetAddress 1, n6, 1),(EthernetAddress 2, n6, 2),
                (EthernetAddress 3, n7, 1),(EthernetAddress 4, n7, 2)]

test1 = addNode empty 1 1 
test2 = addNode test1 2 2
test3 = addNode test2 3 3
test4 = addNode test3 4 4 
test5 = addNode test4 5 5 
test6 = addNode test5 6 6 
test7 = addNode test6 7 7
test8 = addEdgeMac test7 1 2 3 4
test9 = addEdgeMac test8 2 3 3 4
test10 = addEdgeMac test9 3 4 3 4
test11 = addEdgeMac test10 4 5 3 4
test12 = addEdgeMac test11 1 6 3 4
test13 = addEdgeMac test12 3 6 3 4
test14 = addEdgeMac test13 6 7 3 4
{-Just n1 = findNode test14 1
Just n2 = findNode test14 2
Just n3 = findNode test14 3
Just n4 = findNode test14 4
Just n5 = findNode test14 5
Just n6 = findNode test14 6
Just n7 = findNode test14 7-}

--An empty graph to start with
empty :: Graph
empty =  Set.empty

--Adds a new node to the graph with the given ip address and mac address
addNode :: Graph -> Int -> Switch -> Graph
addNode graph ipAddr macAddr =
  let new_node = Node {edges = Set.empty, ip = ipAddr, mac = macAddr} in
  Set.insert new_node graph

findNode :: Graph -> Switch -> Maybe Node
findNode graph macAddr =
  Set.foldl (\found node -> if mac node == macAddr then
                               Just node else found) Nothing graph

findEdge :: Node -> Node -> Maybe Edge
findEdge start end =
  Set.foldl (\found edge -> if dest edge == mac end then
                               Just edge else found) Nothing (edges start)
  
addEdgeMac :: Graph -> Switch -> Switch -> Port -> Port -> Graph
addEdgeMac graph mac1 mac2 portF portB =
  case (findNode graph mac1, findNode graph mac2) of
    (Just n1, Just n2) -> addEdge graph n1 n2 portF portB
    _ -> graph

addEdge :: Graph -> Node -> Node -> Port -> Port -> Graph
addEdge graph start end portForward portBackward =
  let  new_edge1 = Edge {dest= mac end, fport=portForward, bport=portBackward}
       new_edge2 = Edge {dest= mac start, fport=portBackward, bport=portForward}
       n1New = Node {edges=Set.insert new_edge1 (edges start),
                     ip=ip(start), mac=mac(start)}
       n2New = Node {edges=Set.insert new_edge2 (edges end), ip=ip(end),
                     mac=mac(end)} in
  Set.insert n2New (Set.insert n1New (Set.delete start (Set.delete end graph)))

adjacent :: Node -> Node -> Bool
adjacent n1 n2 =
  not (Set.null $ Set.filter (\edge -> dest edge == mac n2) (edges n1))

set_up_distances :: Graph -> Map.Map (Node, Node) (Maybe Int)
set_up_distances graph =
  Set.foldl (\m n1 -> Set.foldl (\m n2 -> let val = if n1 == n2 then Just 0 else if adjacent n1 n2 then Just 1 else Nothing in
                          Map.insert (n1, n2) val m) m graph) Map.empty graph
  
set_up_nexts :: Graph -> Map.Map (Node, Node) (Maybe Node)
set_up_nexts graph =
  Set.foldl (\m n1 -> Set.foldl (\m n2 -> Map.insert (n1, n2) Nothing m) m graph) Map.empty graph

floydWarshall :: Graph -> (Map.Map (Node, Node) (Maybe Int), Map.Map (Node, Node) (Maybe Node))
floydWarshall graph =
  let innerLoop (dists, nexts, knode, inode) jnode =
        let distIJ = Map.findWithDefault Nothing (inode, jnode) dists    --Note that the default case should never happen
            distIK = Map.findWithDefault Nothing (inode, knode) dists
            distKJ = Map.findWithDefault Nothing (knode, jnode) dists
            curNext = Map.findWithDefault Nothing (inode, jnode) nexts in
        let newval (Just ij) (Just ik) (Just kj) = if ij > ik + kj then (Just (ik + kj), Just knode) else (Just ij, curNext)
            newval Nothing (Just ik) (Just kj) = (Just (ik + kj), Just knode)
            newval (Just ij) _ _ = (Just (ij), curNext)
            newval _ _ _ = (Nothing, curNext) in
        let (newDist, newNext) = newval distIJ distIK distKJ in
        (Map.insert (inode, jnode) newDist dists, Map.insert (inode, jnode) newNext nexts, knode, inode)
      middleLoop (dists, nexts, knode) inode =
        let (dists2, nexts2, knode2, inode2) = Set.foldl innerLoop (dists, nexts, knode, inode) graph in
        (dists2, nexts2, knode2)
      outerLoop (dists, nexts) knode =
        let (dists2, nexts2, knode2) = Set.foldl middleLoop (dists, nexts, knode) graph in
        (dists2, nexts2) in
  Set.foldl outerLoop (set_up_distances graph, set_up_nexts graph) graph

findPath :: (Map.Map (Node, Node) (Maybe Int), Map.Map (Node, Node) (Maybe Node)) -> Node -> Node-> Maybe [(Node, Port)]
findPath (dists, nexts) fromNode toNode = 
  if Data.Maybe.isNothing (Map.findWithDefault Nothing (fromNode, toNode) dists) then Nothing else (Just (processPath fromNode (findPathHelper nexts fromNode toNode)))
                                                                                        
findPathHelper :: Map.Map (Node, Node) (Maybe Node) -> Node -> Node-> [Node]
findPathHelper nexts fromNode toNode = 
  case Map.findWithDefault Nothing (fromNode, toNode) nexts of
    Just n -> (findPathHelper nexts fromNode n) ++ (findPathHelper nexts n toNode) 
    Nothing -> [toNode]
               
processPath :: Node -> [Node] -> [(Node, Port)]
processPath start nodeList =
  let (newLst, _) = foldl (\(lst, prev) node -> case findEdge prev node of
            Just e -> ((prev, fport e):lst, node)
            Nothing -> ([], node) --Shoudn't happen, all of the edges should exist
        ) ([], start) nodeList in
  reverse newLst
  
makePolicy :: Graph -> [(Host, Node, Port)] -> Policy
makePolicy graph hostList =
  let (dists, nexts) = floydWarshall graph in
  let pol = foldl (\policy (h1, n1, p1) ->
          foldl (\policy (h2, n2, p2) -> if n1 == n2 then policy else
                                       let path = findPath (dists, nexts) n1 n2 in
                                       case path of
                                         Nothing -> policy
                                         Just p->
                                           foldl (\policy (ni, pi) -> PoBasic (DlSrc h1 `And` DlDst h2 `And` Switch (mac ni)) [Forward (Physical pi) unmodified] `PoUnion` policy)
                                             policy p) policy hostList) PoBottom hostList in
  foldl (\policy (host, node, port) -> PoBasic (DlDst host `And` Switch (mac node)) [Forward (Physical port) unmodified] `PoUnion` policy) pol hostList
  
main = controller (makePolicy tree treeHostList)