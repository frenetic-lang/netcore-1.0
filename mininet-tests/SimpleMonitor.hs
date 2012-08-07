module SimpleMonitor where

import Control.Concurrent
import Control.Monad (forever)
import Frenetic.NetCore
import Frenetic.Pattern (match)
import System.IO
import Data.Time.Clock (getCurrentTime)

monitor = do
  (pktChan, getAct) <- getPkts
  polChan <- newChan

  let printStats pat chan = do
        putStrLn $ "Monitoring flow: " ++ show pat
        forkIO $ forever $ do
          (sw, count) <- readChan chan
          t <- getCurrentTime
          putStrLn $ show t ++ ": " ++ show pat ++ ": " ++ show count
        return ()
  let loop :: Predicate -- ^observed flows
           -> Policy
           -> IO ()
      loop observedPat pol = do
        (sw, pkt) <- readChan pktChan
        let pat = exactMatch pkt
        case pat `match` observedPat of
          True -> do
            putStrLn $ show pat ++ " already seen in " ++ show observedPat
            loop observedPat pol
          False -> do
            (countChan, countAct) <- countPkts 5000
            printStats pat countChan
            let pol' = pol <+> (pat ==> countAct)
            let pat' = observedPat <||> pat
            writeChan polChan (pol' <+> (neg pat' ==> getAct))
            loop pat' pol'
  writeChan polChan (matchAll ==> getAct)
  forkIO (loop matchNone PoBottom)
  return polChan

main = do
  monitorChan <- monitor
  polChan <- newChan
  forkIO $ forever $ do
    pol <- readChan monitorChan
    writeChan polChan (pol <+> (matchAll ==> allPorts unmodified))
  pktChan <- newChan
  dynController polChan pktChan