module ShortestPath
       where

import qualified Debug.Trace as Trace
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe
import Frenetic.NetCore hiding (Switch)
import qualified Frenetic.NetCore
import Frenetic.NetCore.Semantics
import Frenetic.Topo

adjacent :: Graph -> Node -> Node -> Bool
adjacent gr n1 n2 =
  Data.Maybe.isJust (getPort gr n1 n2)

set_up_distances :: Graph -> Map.Map (Node, Node) (Maybe Int)
set_up_distances graph =
  let switchLst = lSwitches graph in
  foldl (\m (n1, s1) -> foldl (\m (n2, s2) ->
                          let val =
                                if n1 == n2 then Just 0 else
                                  if adjacent graph n1 n2 then Just 1 else
                                    Nothing in
                          Map.insert (n1, n2) val m) m switchLst)
  Map.empty switchLst
  
set_up_nexts :: Graph -> Map.Map (Node, Node) (Maybe Node)
set_up_nexts graph =
  let switchLst = lSwitches graph in
  foldl (\m (n1, s1) ->
          foldl (\m (n2, s2) ->
                  Map.insert (n1,n2) Nothing m) m switchLst) Map.empty switchLst

floydWarshall :: Graph -> (Map.Map (Node, Node) (Maybe Int),
                          Map.Map (Node, Node) (Maybe Node))
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
      nodes = map (\(n, s) -> n) (lSwitches graph) in
  foldl outerLoop (set_up_distances graph, set_up_nexts graph) nodes

{- Finds the shortest path from one node to another using the results
   of the floydWarshall methdod.  The path is a maybe list of
   the next node to go to and the port to get there.
-}
findPath :: Graph ->
                 (Map.Map (Node, Node) (Maybe Int),
                  Map.Map (Node, Node) (Maybe Node))
                 -> Node -> Node -> Maybe [(Node, Port)]
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
               
processPath :: Graph -> Node -> [Node] -> [(Node, Port)]
processPath graph start switchLst =
  let (newLst, _) = foldl (\(lst, prev) switch ->
                            case getPort graph prev switch of
                              Just p -> ((prev, p):lst, switch)
                              Nothing -> ([], switch)
                              --Above shoudn't happen, all edges should exist
        ) ([], start) switchLst in
  reverse newLst

--Assumes that hosts have exactly one port, port 0, as definined in Topo.
makePolicy :: Graph -> Policy
makePolicy graph =
  let (dists, nexts) = floydWarshall graph
      hostList = lHosts graph
      updateList lst (hNode, host) ((sNode, ((Switch sNum), p)):t) = 
        case getPort graph sNode hNode of
          Just port -> (host,sNode,sNum,port):lst
          _ -> lst
      updateList lst (hNode, host) ((sNode, (_, p)):t) =
        updateList lst (hNode, host) t
      updateList lst host [] = lst
      hostSwitchList =
        foldl (\lst (n, h) -> updateList lst (n, h) (lPorts graph n)) [] hostList in
  let updatePolicy pol path h1 h2 =
        foldl (\policy (ni, pi) ->
                case getSwitch graph ni of
                  Nothing -> policy --Shouldn't happen
                  Just sNum ->
                    PoBasic
                    (DlSrc (EthernetAddress h1) `And`
                     DlDst (EthernetAddress h2) `And`
                     Frenetic.NetCore.Switch sNum)
                    [Forward (Physical pi) unmodified]
                    `PoUnion` policy)
        pol path in
  let pol = foldl (\policy (h1, n1, s1, p1) ->
                    foldl (\policy (h2, n2, s1, p2) ->
                            if n1 == n2 then policy else
                              let path = findPath graph (dists, nexts) n1 n2 in
                              case path of
                                Nothing -> policy
                                Just p-> updatePolicy policy p h1 h2)
                    policy hostSwitchList)
            PoBottom hostSwitchList in
  foldl (\policy (host, sNode, sNum,  port) ->
          PoBasic (DlDst (EthernetAddress (host))
                   `And` (Frenetic.NetCore.Switch (sNum)))
          [Forward (Physical port) unmodified]
          `PoUnion` policy) pol hostSwitchList

--sample data from the net command of mininet
netOutput :: String
netOutput = "s5 <-> s6-eth3 s7-eth3\n" ++
            "s6 <-> h1-eth0 h2-eth0 s5-eth1\n" ++
            "s7 <-> h3-eth0 h4-eth0 s5-eth2"

sampleTopo :: [((Element, Port), (Element, Port))]
sampleTopo = [((Host(1), 0), (Switch(106), 1)), ((Host(2), 0), (Switch(106), 2)), ((Host(3), 0), (Switch(107), 1)),
              ((Host(4), 0), (Switch(107), 2)), ((Switch(105), 1), (Switch(106), 3)), ((Switch(105), 2), (Switch(107), 3))]
  
main = controller (makePolicy (buildGraph sampleTopo))