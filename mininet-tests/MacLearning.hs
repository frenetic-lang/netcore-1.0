module MacLearning where

import Control.Concurrent
import Control.Monad
import Frenetic.NetCore
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.MultiSet as MS

learningSwitch = do
  (pktChan, q) <- pktQuery
  let defaultAct = Action (MS.singleton (PhysicalFlood, top)) [q]
  polChan <- newChan
  
  let toPred (sw, srcMac) = PrUnion (PrPattern (patDlSrc srcMac)) (PrTo sw)
  let loop :: Map (Switch, Word48) Port -> IO ()
      loop routes = do
        (sw, pkt) <- readChan pktChan
        let routes' = Map.insert (sw, pktDlSrc pkt) (pktInPort pkt) routes
        let pol' = poNaryUnion $ 
                     map (\(src, port) -> PoBasic (toPred src) (forward port))
                         (Map.toList routes')
        let unmatched = prNaryUnion (map toPred (Map.keys routes'))
        let pol = PoUnion pol' (PoBasic unmatched defaultAct)
        writeChan polChan pol
        loop routes'
  forkIO (loop Map.empty)
  writeChan polChan (PoBasic top defaultAct)
  return polChan

main = do
  polChan <- learningSwitch
  freneticServer polChan
