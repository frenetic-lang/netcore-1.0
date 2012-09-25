module ShortestPath
       where

import Data.Array.IO
import qualified Data.HashTable.IO as H
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe

data Edge = Edge {dest :: Node,
                  fport :: Int,
                  bport :: Int} deriving (Eq, Ord)
            
instance Show Edge where
  show e = show ((mac (dest e)), fport e, bport e)

data Node = Node {edges :: Set.Set Edge,
                  ip :: Int,
                  mac :: Int} deriving (Show, Eq, Ord)

type Hashtable k v = H.CuckooHashTable k v

--List of nodes, max id being used, number of nodes in the graph
type Graph = Set.Set Node

--An empty graph to start with
empty :: Graph
empty =  Set.empty

--Adds a new node to the graph with the given ip address and mac address
addNode :: Graph -> Int -> Int -> Graph
addNode graph ipAddr macAddr =
  let new_node = Node {edges = Set.empty, ip = ipAddr, mac = macAddr} in
  Set.insert new_node graph
  
addEdge :: Graph -> Node -> Node -> Int -> Int -> Graph
addEdge graph start end portForward portBackward =
  let  new_edge1 = Edge {dest=end, fport=portForward, bport=portBackward}
       new_edge2 = Edge {dest=start, fport=portBackward, bport=portForward}
       n1New = Node {edges=Set.insert new_edge1 (edges start), ip=ip(start), mac=mac(start)}
       n2New = Node {edges=Set.insert new_edge2 (edges end), ip=ip(end), mac=mac(end)} in
  Set.insert n2New (Set.insert n1New (Set.delete start (Set.delete end graph)))
  
adjacent n1 n2 =
  Set.null $ Set.filter (\edge -> dest edge == n2) (edges n1)

set_up_distances :: Graph -> Map.Map (Node, Node) (Maybe Int)
set_up_distances graph =
  Set.foldl (\m n1 -> Set.foldl (\m n2 -> let val = if n1 == n2 then Just 0 else if adjacent n1 n2 then Just 1 else Nothing in
                          Map.insert (n1, n2) val m) m graph) Map.empty graph
  
set_up_nexts :: Graph -> Map.Map (Node, Node) (Maybe Node)
set_up_nexts graph =
  Set.foldl (\m n1 -> Set.foldl (\m n2 -> Map.insert (n1, n2) Nothing m) m graph) Map.empty graph

floydMarshall :: Graph -> (Map.Map (Node, Node) (Maybe Int), Map.Map (Node, Node) (Maybe Node))
floydMarshall graph =
  let innerLoop (dists, nexts, knode, inode) jnode =
        let distIJ = Map.findWithDefault Nothing (inode, jnode) dists    --Note that the default case should never happen
            distIK = Map.findWithDefault Nothing (inode, knode) dists
            distKJ = Map.findWithDefault Nothing (knode, jnode) dists
            curNext = Map.findWithDefault Nothing (inode, jnode) nexts in
        let newval (Just ij) (Just ik) (Just kj) = if ij > ik + kj then (Just (ik + kj), Just knode) else (Just ij, curNext)
            newval Nothing (Just ik) (Just kj) = (Just (ik + kj), Just knode)
            newval (Just ij) Nothing Nothing = (Just (ij), curNext)
            newval _ _ _ = (Nothing, curNext) in
        let (newDist, newNext) = newval distIJ distIK distKJ in
        (Map.insert (inode, jnode) newDist dists, Map.insert (inode, jnode) newNext nexts, knode, inode)
      middleLoop (dists, nexts, knode) inode =
        let (dists, nexts, knode, inode) = Set.foldl innerLoop (dists, nexts, knode, inode) graph in
        (dists, nexts, knode)
      outerLoop (dists, nexts) knode =
        let (dists, nexts, knode) = Set.foldl middleLoop (dists, nexts, knode) graph in
        (dists, nexts) in
  Set.foldl outerLoop (set_up_distances graph, set_up_nexts graph) graph

findPath :: (Map.Map (Node, Node) (Maybe Int), Map.Map (Node, Node) (Maybe Node)) -> Node -> Node-> Maybe ([Node])
findPath (dists, nexts) fromNode toNode = 
  if Data.Maybe.isNothing (Map.findWithDefault Nothing (fromNode, toNode) dists) then Nothing else (Just (findPathHelper nexts fromNode toNode))
                                                                                        
findPathHelper :: Map.Map (Node, Node) (Maybe Node) -> Node -> Node-> [Node]
findPathHelper nexts fromNode toNode = 
  case Map.findWithDefault Nothing (fromNode, toNode) nexts of
    Just n -> (findPathHelper nexts fromNode n) ++ (findPathHelper nexts n toNode) 
    Nothing -> [toNode]

{-
floydMarshall :: Graph -> Map (Node, Node) (Maybe Int)
floydMarshall graph =
  let graphList = graph >>= H.toList
      count = length graphList
      distances = set_up_distances graph in
  let innerLoop (dists, k, i) j=
        let newDists = do d <- dists
                          let minD = min (readArray (i, j) d) (readArray (i, k) d + readArray (k, j) d)
                          return (writeArray (i, j) minD d) in
        (newDists, k, j)
      middleLoop (dists, k) i =
        let newDists = foldl innerLoop (dists, k, i) [0..count-1] in
        (newDists, k)
      outerLoop dists k =
        foldl middleLoop (dists, k) [0..count-1] in
  foldl outerLoop distances [0..count-1]
  -}
{-
--Applies a function to a Node by its mac address, returns the graph
--and the new node
mapNodeMac :: Graph -> Int -> (Graph, Node)
mapNodeMac  mac =
  
  
--Adds an edge to the graph for two nodes by mac address.
--If either node does not exist, the node is created
addEdgeMac :: Graph -> Int -> Int -> Int -> Int -> Graph
addEdgeMac (nodes, _, _) macStart macEnd portStart portEnd =
  

replaceNth n f (x:xs)
  | n == 0 = (f x):xs
  | otherwise = x:replaceNth (n-1) f xs
replaceNth n newVal [] =
  fail "Trying to replace a value that is out of range"

updatePath :: [[Maybe Int]] -> Int -> Int -> Maybe Int -> [[Maybe Int]]
updatePath path ind1 ind2 val =
  replaceNth ind1 (\x -> replaceNth ind2 (\y -> val) x) path

getVal :: [[Maybe Int]] -> Int -> Int -> Maybe Int
getVal path ind1 ind2 = 
  ((path !! ind1) !! ind2)
  
calcBest :: [[Maybe Int]] -> Int -> Int -> Int -> Maybe Int
calcBest path k i j =
  let inter1 = getVal path i k
      inter2 = getVal path k j
      direct = getVal path i j in
  let combine =
        case (inter1, inter2) of
          (Just a, Just b) -> Just (a + b)
          _ -> Nothing in
  case (direct, combine) of
    (Just a, Just b) -> Just (min a b)
    (Just a, Nothing) -> Just a
    (Nothing, Just b) -> Just b
    (Nothing, Nothing) -> Nothing
      
generatePaths :: Int -> Int -> [[Maybe Int]]
generatePaths i j =
  foldr (\x ac-> (foldr (\y acc-> Nothing:acc) [] [1..j]):ac)  [] [1..i]

floydMarshall :: Graph -> Int
floydMarshall graph= 
  let n = length graph in
  let outerLoop path k =
        middleLoop path k 0
      middleLoop path k i =
        innerLoop path k i 0
      innerLoop path k i j =
        updatePath path i j (calcBest path k i j)
        in
  1-}