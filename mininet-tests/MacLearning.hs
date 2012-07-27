module MacLearning where

import Control.Concurrent
import Control.Monad
import Frenetic.NetCore
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.MultiSet as MS
import System.Log.Logger

learningSwitch = do
  (pktChan, q) <- pktQuery 
  let defaultAct = Action (MS.singleton (PhysicalFlood, top)) [q]
  polChan <- newChan
  
  let toPred (sw, dstMac) = PrIntersect (PrPattern (patDlDst dstMac)) (PrTo sw)

  let loop :: Map (Switch, Word48) Port -> Policy -> IO ()
      loop routes fwdPol = do
        (sw, pkt) <- readChan pktChan
        let routes' = Map.insert (sw, pktDlSrc pkt) (pktInPort pkt) routes
        let fwdPol' = case Map.lookup (sw, pktDlDst pkt) routes' of
              Nothing -> fwdPol
              Just port -> 
                let pat = dlDst (pktDlDst pkt) <&> dlSrc (pktDlSrc pkt) <&>
                            inPort (pktInPort pkt)
                    act = forward port
                  in PoUnion (PoBasic pat act) fwdPol
        let queryPol = PoBasic (PrNegate (poDom fwdPol)) defaultAct
        writeChan polChan (PoUnion fwdPol' queryPol)
        loop routes' fwdPol'
  writeChan polChan (PoBasic top defaultAct)
  forkIO (loop Map.empty PoBottom)
  return polChan

main = do
  polChan <- learningSwitch
  freneticServer polChan
