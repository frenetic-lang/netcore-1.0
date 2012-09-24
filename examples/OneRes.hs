module OneRes where

import Control.Concurrent
import Control.Monad
import Frenetic.NetCore
import System.IO
import Data.Graph
import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

--best way to store graph in haskell?
--data Topology (mac, switch, level) neighbors = 
--  Node (mac, switch, level) ([Topology (mac,switch,level) neighbors])


displayGraph :: Gr String Double  
displayGraph = undir (buildGr [([], 2, "start", [(2.0, 1)]), ([], 1, "end",[(2.0, 3)]), ([], 3, "start", [])])

--populate the metadata of a graph to provide one-resiliant forwarding
oneres :: Gr String Double -> Gr String Double
oneres g = g

--identify l -1 edges in a graph g and annotate them 
lOneClose :: Gr String Double -> Gr String Double
lOneClose g = g



main = do
  --let top = getTop
  --let pol = buildPolicy(top)
  --controller pol
  controller (Any ==> allPorts unmodified)


