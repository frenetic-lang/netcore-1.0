module Monitor where

import Control.Concurrent
import Data.Word
import qualified Data.Set as Set
import qualified Data.Map as Map
import Frenetic.NetCore
import Frenetic.Common (mergeChan, bothChan)
import Frenetic.NetCore.Types (poDom)
import MacLearning (learningSwitch)

data Msg
  = Block 
  | Unblock 
  | Unmonitor

monitor :: EthernetAddress -- ^host to monitor
        -> Word32 -- ^IP address of host, to prevent flow table explosion
        -> Chan (Msg, Word32) -- ^'monitor' will write to this channel
        -> IO Policy -- ^the monitoring policy
monitor eth ip actionChan = do
  (countChan, countAction) <- countPkts 5000
  let loop :: Map.Map Switch Float -- per-switch traffic
           -> IO ()
      loop traffic = do
        (sw, count) <- readChan countChan
        let upd v score = 2.0 * v + score / 2 
        let traffic' = Map.insertWith upd sw (fromIntegral count) traffic
        case Map.lookup sw traffic' of
          Nothing -> fail "sw should be in traffic'"
          Just score -> case score < 2.0 of
            True -> writeChan actionChan (Unmonitor, ip)
                     -- this thread terminates
            False -> do
              case score > 10.0 of
                True -> writeChan actionChan (Block, ip)
                False -> writeChan actionChan (Unblock, ip)
              loop traffic'
  return (dlSrc eth <&&> dlTyp 0x0800 <&&> nwSrc ip ==> countAction)


dosDetector :: IO (Chan (Policy, Predicate))
dosDetector = do
  (pktChan, inspectPkt) <- getPkts
  cmdChan <- newChan
  resultChan <- newChan

  let buildPolPred :: Map.Map Word32 Policy -- ^monitors
                   -> Set.Set Word32 -- ^blocked addresses
                   -> (Policy, Predicate)
      buildPolPred monitors blocked = (pol, pred) 
        where monPol = foldr (<+>) PoBottom (Map.elems monitors)
              pol = monPol <+> (neg (poDom monPol) ==> inspectPkt)
              f srcIP = nwSrc srcIP <&&> nwProto 0x0800
              pred = neg (foldr (<||>) matchNone (map f (Set.elems blocked)))

  let loop :: Map.Map Word32 Policy
           -> Set.Set Word32
           -> IO ()
      loop monitors blocked = do
        chan <- mergeChan pktChan cmdChan
        msg <- readChan chan
        case msg of
          
          Left (sw, Packet{pktDlSrc=srcMac,pktNwSrc=Just srcIP}) -> 
            case Map.member srcIP monitors of 
              True -> loop monitors blocked -- already monitoring
              False -> do
                pol <- monitor srcMac srcIP cmdChan
                let monitors' = Map.insert srcIP pol monitors
                writeChan resultChan (buildPolPred monitors' blocked)
                loop monitors' blocked
          Left _ -> do
            -- TODO(arjun): something?
            loop monitors blocked
          Right (Block, srcIP) -> do
            let blocked' = Set.insert srcIP blocked
            writeChan resultChan (buildPolPred monitors blocked')
            loop monitors blocked'
          Right (Unblock, srcIP) -> do
            let blocked' = Set.delete srcIP blocked
            writeChan resultChan (buildPolPred monitors blocked')
            loop monitors blocked'
          Right (Unmonitor, srcIP) -> do
            let blocked' = Set.delete srcIP blocked
            let monitors' = Map.delete srcIP monitors
            writeChan resultChan (buildPolPred monitors' blocked')
            loop monitors' blocked'

  forkIO (loop Map.empty Set.empty)
  writeChan resultChan (buildPolPred Map.empty Set.empty)
  return resultChan

main = do
  routeChan <- learningSwitch
  blockChan <- dosDetector
  polChan <- newChan
  routesAndBlocksChan <- bothChan routeChan blockChan
  
  let loop = do
        (routes, (monitors, restriction)) <- readChan routesAndBlocksChan
        writeChan polChan ((routes <%> restriction) <+> monitors)
        loop   

  forkIO loop
  
  pktChan <- newChan
  dynController polChan pktChan