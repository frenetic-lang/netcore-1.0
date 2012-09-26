module OneRes where

import Control.Concurrent
import Control.Monad
import Frenetic.NetCore
import Frenetic.Topo
import System.IO



main = do
  --let top = getTop
  --let pol = buildPolicy(top)
  --controller pol
  controller (Any ==> allPorts unmodified)


