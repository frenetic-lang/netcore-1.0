module TopoSample (testDFS) where

import Data.Graph.Inductive
import Frenetic.Topo
import Frenetic.TopoGen
import Frenetic.TopoParser
import Text.ParserCombinators.Parsec

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

testParse :: Either ParseError [(Node, [Node])]
testParse = parseTopo "s5 <-> s6-eth3 s7-eth3\ns6 <-> h1-eth0 h2-eth0 s5-eth1\ns7 <-> h3-eth0 h4-eth0 s5-eth2\n"


