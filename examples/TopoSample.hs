module TopoSample 
  (testDFS
   , testMakeE
  ) where

import Data.Graph.Inductive
import Data.Either
import Frenetic.Topo
import Frenetic.TopoGen
import Frenetic.TopoParser
import Frenetic.NetCore
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

topol :: Int -> Topo
topol n = linearHosts n 
--

--right now DFS doesn't work, mainly because I don't need DFS
--for lONE closing, I need something slightly different
--this is to help me debug/identify what I really need
testDFS :: Topo -> LNode Int -> [Node]
testDFS t m = Frenetic.Topo.dfs t m

testParse :: Either ParseError [(LNode Char, [LNode Char])]
testParse = parseTopo "s5 <-> s6-eth3 s7-eth3\ns6 <-> h1-eth0 h2-eth0 s5-eth1\ns7 <-> h3-eth0 h4-eth0 s5-eth2\n"

testMakeE :: Either ParseError [((Node, Port), (Node, Port))]
testMakeE = case testParse of 
            Right b -> Right (makeEdgeList b)
            Left a -> Left a

testMakeTop :: Either ParseError Topo
testMakeTop = case testMakeE of
              Right b -> Right (buildGraph b)
              Left a -> Left a

