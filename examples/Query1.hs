module Query1 where

import Control.Concurrent
import Control.Monad
import Frenetic.NetCore
import System.IO

main = do
  (ch, queryAct) <- countPkts 1000
  let pol = Any ==> (allPorts unmodified <+> queryAct)
  forkIO $ forever $ do
    (sw, n) <- readChan ch
    putStrLn ("Counter is: " ++ show n)
    hFlush stdout
  controller pol
