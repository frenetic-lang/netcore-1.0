module Query1 where

import Data.MultiSet as MS
import Control.Concurrent
import Control.Monad
import Frenetic.NetCore
import System.IO

main = do
  (ch, queryAct) <- query 1000
  let pol = PoBasic (PrPattern top) (allPorts unmodified <+> queryAct)
  forkIO $ forever $ do
    (sw, n) <- readChan ch
    putStrLn ("Counter is: " ++ show n)
    hFlush stdout
  polChan <- newChan
  writeChan polChan pol
  freneticServer polChan
