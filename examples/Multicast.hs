module Multicast
       where
import System.IO
import qualified Debug.Trace as Trace
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe
import Frenetic.NetCore hiding (Switch)
import qualified Frenetic.NetCore as Net
import Frenetic.NetCore.Semantics
import Frenetic.Topo
import Control.Concurrent
import Data.IORef
import Control.Monad (forever)

type Multicast = (Graph,  IPAddressPrefix)

{-Takes the topology of the network, the current multicast tree, and 
  the node to add and returns a new multicast tree-}
addToMulticast :: Graph -> Multicast -> Node -> Multicast
addToMulticast topo (tree, addr) node =
  let topo2 = Trace.trace ("Adding " ++ (show node) ++ "to tree") topo in
  case getElement topo2 node of
    Just elem ->
      let elems = lElements tree in
      if elems == [] then ((buildGraphExplicitNodes [elem] []), addr)
      else
        case List.find (\(n, e) -> n == node) elems of
          Just _ -> (tree, addr)
          Nothing -> (addToSpanningTree topo tree node, addr)
    Nothing -> error "The node to add wasn't in the graph"

{- Takes the topology, the tree, the included nodes, and the node to add
   and returns the graph of the new multicast tree.  Does breadth first search
   from the node to add to find which nodes need to be included.
 -}
addToSpanningTree :: Graph -> Graph -> Node -> Graph
addToSpanningTree graph tree node =
  case getElement graph node of
    Nothing -> error "The node given was not in the graph"
    Just elem -> addToSpanningTreeHelper graph tree node elem

addToSpanningTreeHelper :: Graph -> Graph -> Node -> Element -> Graph
addToSpanningTreeHelper graph tree node elem =
  let included = lElements tree in
  let helper pathMap [] = tree
      helper pathMap ((curNode, curElem, prevNode):t) =
        case List.find (\(n,e) -> e == curElem) included of
          Just (entry,_) -> rebuildTree graph tree curNode
                    (Map.insert curNode prevNode pathMap)
          Nothing ->
            if Map.member curNode pathMap 
            then helper pathMap t
            else
              (let newLst = 
                     foldl (\acc (n, (e,p)) -> (n, e, curNode):acc)
                     (List.reverse t) (lPorts graph curNode) in
               helper (Map.insert curNode prevNode pathMap)
               (List.reverse newLst)) in
   helper (Map.empty) [(node, elem, node)]

{-Takes in the topology, the spanning tree, the node by which the new nodes
  will be connected to the tree, and a map of nodes to their previous node
  for the path to add.  Returns the new spanning tree graph -}
rebuildTree :: Graph -> Graph -> Node -> Map.Map Node Node -> Graph  
rebuildTree graph tree entry pathMap =
  let elemLst = lElements tree in
  let lstFromTree = foldl (\acc1 (n1, e1) ->
          foldl (\acc2 (n2, e2) ->
                  case (getPort tree n1 n2, getPort tree n2 n1) of
                    (Just p1, Just p2) | n1 < n2 -> ((e1, p1), (e2, p2)):acc2
                    _ -> acc2)
          acc1 elemLst) [] elemLst in
  let helper acc curNode =
        case Map.lookup curNode pathMap of
          Nothing -> error "Map should never not have a value for the key"
          Just prev | prev == curNode -> acc
          Just prev -> 
            (case (getPort graph curNode prev, getPort graph prev curNode,
                   getElement graph curNode, getElement graph prev) of
               (Just p1, Just p2, Just e1, Just e2) ->
                 helper (((e1, p1),(e2, p2)):acc) prev
               _ -> error "Mismatched edges in graph") in
  let buildLst = helper lstFromTree entry in
  buildGraph buildLst

-- |An empty multicast tree
empty :: IPAddressPrefix -> Multicast
empty prefix = (buildGraph [], prefix)

samplePrefix :: IPAddressPrefix
samplePrefix = IPAddressPrefix (IPAddress(4009754625)) 32

-- | a sample graph for testing
testTopo :: Graph
testTopo = buildGraph [((Host(1), 0), (Switch(101), 1)), ((Host(2), 0), (Switch(107), 3)), ((Host(3), 0), (Switch(108), 4)),
                       ((Host(4), 0), (Switch(108), 5)), ((Host(5), 0), (Switch(109), 3)), ((Host(6), 0), (Switch(103), 2)),
                       ((Switch(101), 2), (Switch(102), 1)), ((Switch(101), 3), (Switch(108), 1)), ((Switch(102), 2), (Switch(103), 1)),
                       ((Switch(102), 3), (Switch(104), 1)), ((Switch(103), 3), (Switch(105), 2)), ((Switch(103), 4), (Switch(107), 1)),
                       ((Switch(104), 2), (Switch(105), 1)), ((Switch(105), 3), (Switch(106), 1)), ((Switch(105), 4), (Switch(108), 2)),
                       ((Switch(106), 2), (Switch(107), 2)), ((Switch(107), 4), (Switch(109), 2)), ((Switch(108), 3), (Switch(109), 1))]

-- | takes a multicast and creates a forwarding policy
makePolicy :: Multicast -> Policy
makePolicy (tree, ipAddr) =
  foldl (\policy (sNode, s1) ->
          let forwardLst =
                foldl (\forwardList (_, port) -> 
                        (Forward (Physical port) unmodified):forwardList)
                [] (ports tree sNode) in
          (PoBasic (NwDst (ipAddr) `And` Net.Switch (s1))
           forwardLst) `PoUnion` policy) PoBottom (lSwitches tree)

-- | Set up the channels for getting the packets and making a new query
requestsByLocation :: IPAddressPrefix ->
                      IO (Chan EthernetAddress,  Chan Policy)
requestsByLocation treeAddress = do
  locChan <- newChan
  polChan <- newChan
  (pktChan, act) <- getPkts
  let loop locs = do
        (Loc sw port, pkt) <- readChan pktChan
        let srcMac = pktDlSrc pkt
        putStrLn ("The src mac address is :" ++ (show srcMac))
        hFlush stdout
        case Map.lookup srcMac locs of
          Just _ -> do
            putStrLn ("im doing nothing")
            loop locs
          otherwise -> do
            let locs' = Map.insert srcMac (DlSrc srcMac) locs
            writeChan locChan srcMac
            let knownHosts = prOr (Map.elems locs')
            writeChan polChan $ Any ==>act
              --Any `Or` (Not (DlSrc broadcastAddress <||> knownHosts)) `And` (NwDst(treeAddress))==> act
            loop locs'
  writeChan polChan (Any ==> act)--(Any `Or` (Not (DlSrc broadcastAddress)) `And` (NwDst(treeAddress)) ==> act)
  forkIO (loop Map.empty)
  return (locChan, polChan)
                
-- | Takes in the entry requests and builds a new multicast and policy
placeRoutes :: Graph -> Chan EthernetAddress -> IPAddressPrefix -> IO (Chan Policy)
placeRoutes topo pktChan treeAddr = do
  polChan <- newChan
  multicastTreeRef <- newIORef (empty treeAddr)
  writeChan polChan PoBottom
  forkIO $ forever $ do
    dlSrc <- readChan pktChan
    multicastTree <- readIORef multicastTreeRef
    case List.find (\(node, h) -> EthernetAddress(h) == dlSrc) (lHosts topo) of
      Nothing -> error "The host to add was not in the topology"
      Just (node, _) -> do 
        let newTree = addToMulticast topo multicastTree node
        let newPol = makePolicy newTree
        writeChan polChan newPol
        writeIORef multicastTreeRef newTree
  return polChan

-- | coordinates channels; takes the graph's topology and the multicast address
multicastTopo :: Graph -> IPAddressPrefix -> IO (Chan Policy)
multicastTopo topo treeAddr = do
  (joinRequestsChan, queryPolChan) <- requestsByLocation treeAddr
  fwdPolChan <- placeRoutes topo joinRequestsChan treeAddr
  bothPolsChan <- both queryPolChan fwdPolChan
  polChan <- newChan
  forkIO $ forever $ do
    (queryPol, fwdPol) <- readChan bothPolsChan
    let fwdPol2 = Trace.trace ("Writing new policy "++(show (fwdPol <+> queryPol))) fwdPol
    writeChan polChan (fwdPol2 <+> queryPol)
  return polChan
  
main = do
  polChan <- multicastTopo testTopo (IPAddressPrefix (IPAddress(4009754625)) 32)
  dynController polChan