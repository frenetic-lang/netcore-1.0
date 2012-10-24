module Frenetic.Topo
  ( Graph
  , Node  
  , buildGraph
  , Link
  , LLink
  , Element(Switch,Host)
  , Switch
  , Host  
  , Port
  , Loc(Loc)  
  , isHost
  , isSwitch
  , lHosts
  , hosts  
  , lSwitches
  , switches    
  , links
  , lLinks
  , linkToLoc
  , ports
  , lPorts
  , filterElements
  , filterNodes
  , getElement
  , getSwitch
  , getHost
  , getPort
  , getEdge
  , subgraph  
  , toUnitWeight  
  ) where

import Data.Graph.Inductive.Graph hiding (Graph)
import Data.Graph.Inductive.Tree 
import Data.Word
import Data.Maybe
import Data.Tuple
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Generics

-- |A switch's unique identifier.
type Switch = Word64

-- |A Host's unique identifier.
type Host = Word64

-- |The number of a physical port.
type Port = Word16

-- |'Loc' uniquely identifies a port at a switch.
data Loc = Loc Switch Port
  deriving (Eq, Ord, Show, Data, Typeable)

data Element
  = Switch Switch 
  | Host Host
  deriving (Eq, Ord, Show)

isHost :: Element -> Bool
isHost (Switch _) = False
isHost (Host _) = True                         

isSwitch :: Element -> Bool
isSwitch (Switch _) = True
isSwitch (Host _) = False                       

type Link = (Element,Element,Port)

linkToLoc :: Link -> Loc
linkToLoc (Switch s,_,p) = Loc s p 
linkToLoc (e,_,_) = error ("linkToLoc: " ++ show e ++ " is not a switch")

type LLink = ((Node,Element),(Node,Element),Port)

type Graph = Gr Element Port

-- | Build a graph from list of undirected edges labeled with their ports
-- Ensures that the resulting graph is undirected-equivalent, and labels each
-- "directed" edge with the appropriate port to send a packet over that edge
-- from the source switch.
buildGraph :: [((Element,Port), (Element,Port))] -> Graph
buildGraph links = mkGraph nodes edges where  
  elements = Set.toList $
             foldl (\ s ((e1, _), (e2, _)) -> 
                     Set.insert e1 (Set.insert e2 s)) Set.empty links
  nodes = zip [0..] elements  
  getUid e = case lookup e (map swap nodes) of 
    Just n -> n
    Nothing -> error ("buildGraph: Missing uid for " ++ show e)
  edges = Set.toList $
          foldl (\ s ((e1,p1), (e2, p2)) -> 
                       let (n1,n2) = (getUid e1, getUid e2) in 
                       Set.insert (n1,n2,p1) (Set.insert (n2,n1,p2) s)) Set.empty links

lElements :: Graph -> [(Node,Element)]
lElements topo = labNodes topo
  
elements :: Graph -> [Element]
elements topo = map snd $ lElements topo

lHosts :: Graph -> [(Node,Host)]
lHosts topo = foldr f [] l
  where l = labNodes topo
        f (n,Host h) l = (n,h):l
        f e _ = error ("lHosts: " ++ show e)

hosts :: Graph -> [Host]
hosts topo = foldr f [] l
  where l = labNodes topo  
        f (_,Host h) l = h:l
        f e _ = error ("lHosts: " ++ show e)
                
lSwitches :: Graph -> [(Node,Switch)]
lSwitches topo = foldr f [] l
  where l = labNodes topo
        f (n,Switch s) l = (n,s):l
        f e _ = error ("lSwitches: " ++ show e)

switches :: Graph -> [Switch]
switches topo = foldr f [] l
  where l = labNodes topo  
        f (_,Switch s) l = s:l
        f e _ = error ("switches: " ++ show e)

links :: Graph -> [Link]
links topo = map f l 
  where l = labEdges topo
        f (n1,n2,p) = (fromJust $ getElement topo n1, fromJust $ getElement topo n2, p)

lLinks :: Graph -> [LLink]
lLinks topo = map f l 
  where l = labEdges topo
        f (n1,n2,p) = ((n1, fromJust $ getElement topo n1), (n2, fromJust $ getElement topo n2), p)

-- |Get the (dest, port) of a switch in the topology
lPorts :: Graph -> Node -> [(Node,(Element,Port))]
lPorts topo n = map (\ (n,p) -> (n, (fromJust $ getElement topo n, p))) $ lsuc topo n

-- |Get the ports of a switch in the topology.
ports :: Graph -> Node -> [(Element,Port)]
ports topo = map snd . lPorts topo

getElement :: Graph -> Node -> Maybe Element
getElement topo n = lab topo n 
                    
getSwitch :: Graph -> Node -> Maybe Switch
getSwitch topo n = case lab topo n of
                     Just (Switch s) -> Just s  
                     _ -> Nothing

getHost :: Graph -> Node -> Maybe Host
getHost topo n = case lab topo n of
                     Just (Host h) -> Just h
                     _ -> Nothing

-- | Maybe get the label of the edge from n1 to n2
getPort :: Graph -> Node -> Node -> Maybe Port
getPort topo n1 n2 = lookup n2 (lsuc topo n1)

-- hack!
-- | Get the normalized pair of locations that one location is on
getEdge :: Graph -> Loc -> (Loc,Loc)
getEdge topo loc@(Loc switch port) = 
  normal (Loc (getId $ getElement topo n) port, Loc (getId $ getElement topo n') port')
  where 
    (n,_) = fromJust $ List.find (\(n,s) -> fromIntegral s == switch) $ lSwitches topo
    getId (Just (Switch s)) = fromIntegral s
    getId x = error ("getId: " ++ show x)
    normal (l1, l2) = if l1 < l2 then (l1, l2) else (l2, l1)
    (n', port') = fromJust $ List.find (\(_, port') -> port == port') $ lsuc topo n

filterElements :: (Element -> Bool) -> Graph -> Graph
filterElements pred topo = delNodes badNodes topo where
  badNodes = map fst . filter (not . pred . snd) . labNodes $ topo
  
filterNodes :: (Node -> Bool) -> Graph -> Graph
filterNodes pred topo = delNodes badNodes topo where
  badNodes = map fst . filter (not . pred . fst) . labNodes $ topo

-- | Get the subgraph only containing nodes
subgraph :: Set.Set Node -> Graph -> Graph
subgraph nodes topo = filterNodes (\ n -> Set.member n nodes) topo

-- |Convert a topology to a graph with weight 1 on each edge, for use with
-- Data.Graph.Inductive.Query.SP's shortest path algorithms
toUnitWeight :: Graph -> Gr Element Int
toUnitWeight topo = emap (const 1) topo

-- -- BEGIN CDM STUFF
-- -- |Get the l-1 nodes of the topology
-- -- By definition, these are the nodes incident to no cycle in the topology
-- lOne :: Topo -> Topo 
-- lOne gr = 
--   let inCycle (v, label) = (foldl (\x -> \y -> if y == v then x+1 else x) 0 (leveling gr (v, label))) > 1 
--   in filterGr inCycle gr 
-- 
-- lOneClose :: (Graph gr) => gr a b -> [Node] -> [Node]
-- lOneClose g nodes = nodes
-- 
-- leveling :: (Graph gr) => gr a b -> LNode a -> [Node]
-- leveling gr a = 
--   let explore path visited (i,l) = 
--         case () of  _ 
--                       |  (isJust (List.find (\x -> i == x) path)) -> i : visited 
--                       |  (isJust (List.find (\x -> i == x) visited)) -> i : visited 
--                       |  otherwise -> 
--                         let n_path = i : path in
--                         let edges = labNodes (filterGr (\x -> isJust (getEdgeLabel gr i (fst x))) gr) in
--                           foldl (\x -> \y ->  x  ++ explore n_path (i : visited) y) [] edges 
--   in explore [] [] a
-- 
-- -- END CDM STUFF
-- 
