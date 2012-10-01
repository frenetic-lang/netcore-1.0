module TopoSample (testDFS) where

import Data.Graph.Inductive
import Frenetic.Topo
import Frenetic.TopoGen

--s = (0,-2)
--h1 = (1, -2)
--h2 = (2, -2)
--h3 = (3, -2)
--
--ns = [s,h1,h2,h3]
--es = [(0,1,()), (0,2,()), (0,3,())]
--
--topo :: Gr N ()
--topo = mkGraph ns es
--

--right now DFS doesn't work, mainly because I don't need DFS
--for lONE closing, I need something slightly different
--this is to help me debug/identify what I really need
testDFS :: Int -> LNode Int -> [Node]
testDFS n m = Frenetic.Topo.dfs (linearHosts n) m



