module Frenetic.PolicyGen
  ( queries
  , realFlood
  , simuFlood
  , simuFloodQuery
  , shortestPath
  , multicast
  ) where

import Frenetic.Common
import Data.Graph.Inductive.Graph hiding (Graph)
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Query.MST
import Data.Graph.Inductive.Query.SP
import Data.Graph.Inductive.Internal.RootPath
import Data.Maybe
import qualified Data.Set as Set
import Data.Tuple
import Frenetic.NetCore.Types hiding (Switch)
import Frenetic.NetCore.Short
import Frenetic.Pattern
import Frenetic.Topo
import qualified Frenetic.Topo as Topo
import qualified Frenetic.NetCore.Types as NetCore
import System.IO.Unsafe
import Nettle.Ethernet.EthernetAddress (EthernetAddress (..), broadcastAddress)

import Debug.Trace

-- |Convert a list to a list of adjacent pairs
toHops :: [a] -> [(a, a)]
toHops [] = []
toHops [_] = []
toHops all@(_ : tail) = zip all tail

-- |Get an infinite stream of fresh queries
queries :: IO [[Action]]
queries = do
  (_, q) <- countPkts 1
  rest <- unsafeInterleaveIO queries
  return (q : rest)

-- | For each switch, just allPorts every packet.
realFlood :: Graph -> Policy
realFlood topo = mconcat policies where
  ss = switches topo
  policies = [NetCore.Switch (fromIntegral s) ==> allPorts unmodified | s <- ss]

-- | For each switch, direct each packet to every port on that switch that's in
-- the topology.  Different from realFlood when dealing with subgraphs because
-- simuFlood preserves behavior when composed into a larger graph.
simuFlood :: Graph -> Policy
simuFlood topo = mconcat policies where
  ss = lSwitches topo
  policies = [NetCore.Switch s <&&> validPort n ==>
              modify (ports' n) | (n,s) <- ss]
  ports' n = zip (map snd $ ports topo n) (repeat unmodified)
  validPort n = prOr . map (\(_,p) -> IngressPort p) $ ports topo n

-- |Simulate flooding, and observe all packets with query.
simuFloodQuery :: Graph -> [Action] -> Policy
simuFloodQuery topo q = mconcat policies where
  ss = lSwitches topo
  policies = [NetCore.Switch s <&&> validPort n ==>
              (modify (ports' n) <+> q) | (n,s) <- ss]
  ports' n = zip (map snd $ ports topo n) (repeat unmodified)
  validPort n = prOr . map (\(_,p) -> IngressPort p) $ ports topo n

-- |Construct an all-pairs-shortest-path routing policy between hosts, using the
-- ID of the host as the MAC address.
shortestPath :: Graph -> Policy
shortestPath topo = policy' where
  policy' = crunch policy $ map (fromJust . getSwitch topo) $ map fst $ lSwitches topo
  policy = mconcat policies
  routingTopo = toUnitWeight topo
  hostsSet = Set.fromList (lHosts topo)
  policies = map pathPolicy $ Set.toList hostsSet
  pathPolicy x@(n,h1) = mconcat policies where
    otherHosts = Set.delete x hostsSet
    paths = spTree n routingTopo
    policies = [ buildPath (EthernetAddress (fromIntegral h1))
                           (EthernetAddress (fromIntegral h2)) $
                 getLPathNodes n2 paths
               | (n2,h2) <- Set.toList otherHosts ]
  buildPath _ _ [] = PoBottom
  buildPath _ _ [_] = PoBottom
  buildPath source dest path = mconcat policies where
    hops = toHops path
    policies = mapMaybe (buildHop source dest) $ hops
  buildHop source dest (n1,n2) =
    if not (Set.null . Set.filter (\ (n,_) -> n == n1) $ hostsSet) then Nothing
    else Just (NetCore.Switch (fromJust $ getSwitch topo n1) <&&>
               DlSrc source <&&> DlDst dest ==> forward [destPort])
    where
    destPort = case getPort topo n1 n2 of
                 Just port -> port
                 Nothing -> error "Tried to find nonexistent port."

-- |Construct a policy that routes traffic to DlDst:FF:FF:FF:FF:FF:FF to all
-- hosts except the one it came in on
multicast :: Graph -> Policy
multicast topo = mconcat policies <%> DlDst broadcastAddress where
  routingTopo = toUnitWeight topo
  hostsSet = Set.fromList (hosts topo)
  tree = msTree routingTopo
  hops = Set.fromList .
         concatMap (\pair -> [pair, swap pair]) .
         concatMap toHops .
         map (map fst) .
         map (\(LP x) -> x) $ tree
  policies = [ buildSwitch n | (n,_) <- lSwitches topo ]
  buildSwitch s = mconcat policies where
    ports = Set.fromList [p | (dest,(_,p)) <- lPorts topo s,
                              Set.member (s, dest) hops]
    s' = fromIntegral s
    policies = [ inport s' p ==> forward (Set.toList $ Set.delete p ports)
               | p <- Set.toList ports ]
