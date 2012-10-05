module OneRes where

import Control.Concurrent
import Control.Monad
import Frenetic.NetCore
import Frenetic.Topo
import System.IO
import Data.Graph.Inductive
import Data.Either
import Frenetic.Topo
import Frenetic.TopoGen
import Frenetic.TopoParser
import Text.ParserCombinators.Parsec

-- This controller will grow into providing oneRes for a statis topology
-- currently, provides 'nores' as I'm still debugging the grapha algorithms to
-- suport reseliency

parseT :: Either ParseError [(LNode Char, [LNode Char])]
parseT = parseTopo "s5 <-> s6-eth3 s7-eth3\ns6 <-> h1-eth0 h2-eth0 s5-eth1\ns7 <-> h3-eth0 h4-eth0 s5-eth2\n"

makeE :: Either ParseError [((Node, Port), (Node, Port))]
makeE = case parseT of 
            Right b -> Right (makeEdgeList b)
            Left a -> Left a

makeTop :: Topo
makeTop = case makeE of
              Right b -> (buildGraph b)
              Left a -> linearHosts 10



mkResRouting :: Topo -> Policy
mkResRouting t = 
  (Any ==> allPorts unmodified)

policy = mkResRouting makeTop

main = do
  controller policy
  --controller (Any ==> allPorts unmodified)


