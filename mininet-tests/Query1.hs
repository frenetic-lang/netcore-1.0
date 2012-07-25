module Query1 where

import Data.MultiSet as MS
import Control.Concurrent
import Control.Monad
import Frenetic.NetCore
import System.IO

main = do
  (ch, q) <- query 1000
  let queryAct = Action MS.empty [q]
  let pol = PoBasic (PrPattern top) (flood `unionAction` queryAct)
  forkIO $ forever $ do
    (sw, n) <- readChan ch
    putStrLn ("Counter is: " ++ show n)
    hFlush stdout
  polChan <- newChan
  writeChan polChan pol
  freneticServer polChan
