{-# LANGUAGE
    TypeSynonymInstances
 #-}

module Policies.ShortestPath where

import Topologies
import Frenetic.Compat
import Frenetic.Pattern
import Frenetic.NetCore.Types
import Frenetic.NetCore.Action
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Graph.Inductive.Query.SP as SP
import qualified Data.Set as Set

mkPolicy :: Topology -> Policy
mkPolicy topo = combine paths
  where
    combine [] = PoBasic (PrPattern top) dropPkt
    combine [(h1,h2,[])] = combine []
    combine [(h1,h2,p)] = mkPathPolicy topo h1 h2 p
    combine ((h1,h2,[]):ps) = combine ps
    combine ((h1,h2,p):ps) =
      let p1 = mkPathPolicy topo h1 h2 p
          p2 = combine ps
      in PoUnion p1 p2
    paths = [(h1,h2,SP.sp h1 h2 stripped_topo) | h1 <- hosts topo, h2 <- hosts topo, h1 /= h2]
    stripped_topo :: Gr Int Int
    stripped_topo = mkGraph (map (\n -> (n,1)) (nodes topo))
                     $ map (\(n1,n2) -> (n1,n2,1)) $ edges topo

mkPathPolicy :: Topology -> Node -> Node -> [Node] -> Policy
mkPathPolicy topo src dst (h1:path) = mk path
  where
    srcIp = Wildcard (ip topo src) 0
    dstIp = Wildcard (ip topo dst) 0
    typ = Wildcard 0x800 0
    pat = top {ptrnNwSrc = srcIp, ptrnNwDst = dstIp, ptrnDlTyp = typ}
    mk [] = PoBasic (PrPattern pat) dropPkt
    mk (s:[dst]) =
      let Just outPort = port topo s dst in
        PoBasic (PrPattern pat) (forward outPort)
    mk (s:s':ss) = case port topo s s' of
      Just outPort ->
        let
          p1 = PoBasic (PrPattern pat) (forward outPort)
          p2 = mk (s':ss)
        in PoUnion p1 p2
      Nothing -> error "shortest path: no port connecting neighbors on the path."

printPath :: (Node,Node,Path) -> String
printPath (h1,h2,path)= (show h1) ++ (show h2) ++ foldl (\s n -> s ++ "\n" ++ show n ) "" path

