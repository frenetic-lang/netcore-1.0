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

testDFS :: Int -> LNode Int -> [Node]
testDFS n m = Frenetic.Topo.dfs (linearHosts n) m



