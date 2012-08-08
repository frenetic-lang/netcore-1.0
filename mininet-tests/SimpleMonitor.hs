module SimpleMonitor where

import Control.Concurrent
import Control.Monad (forever)
import Frenetic.NetCore
import Frenetic.Pattern (match)
import System.IO
-- industrial strength time library, also impossible to use
import Data.Time.Clock (getCurrentTime, utctDayTime)
import qualified Data.Map as Map

monitor = do
  (pktChan, getAct) <- getPkts
  polChan <- newChan

  let printStats flow chan = do
        putStrLn $ "Now monitoring " ++ flow
        forkIO $ forever $ do
              (sw, count) <- readChan chan
              t_ <- getCurrentTime
              let t = show t_
              putStrLn $ t ++ ": " ++ show count ++ " packets for " ++ flow
        return ()
  let loop :: Predicate -- ^observed flows
           -> Policy
           -> IO ()
      loop observedPat pol = do
        (sw, pkt) <- readChan pktChan
        let pat = dlSrc (pktDlSrc pkt) <&&> dlDst (pktDlDst pkt)
        case pat `match` observedPat of
          True -> do
            putStrLn $ show pat ++ " already seen in " ++ show observedPat
            loop observedPat pol
          False -> do
            (countChan, countAct) <- countPkts 5000
            printStats (show pat) countChan
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