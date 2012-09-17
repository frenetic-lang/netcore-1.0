module OneRes where

import Control.Concurrent
import Control.Monad
import Frenetic.NetCore
import System.IO

--best way to store graph in haskell?
--data Topology (mac, switch, level) neighbors = 
--  Node (mac, switch, level) ([Topology (mac,switch,level) neighbors])


--buildPolicy :: Topology -> Policy
--buildPolicy top = 

--detectFailure :: 
--detectFailure = 
 
--repairFailure

--is PoUnion smart? Will calls to union blow up complexity of policy
--unnecessarily 
main = do
  --let top = getTop
  --let pol = buildPolicy(top)
  --controller pol
  controller (Any ==> allPorts unmodified)


