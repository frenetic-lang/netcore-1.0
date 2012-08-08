module Monitor where

import Control.Concurrent
import Data.Word
import qualified Data.Set as Set
import qualified Data.Map as Map
import Frenetic.NetCore
import Frenetic.Common (mergeChan, bothChan)
import Frenetic.NetCore.Types (poDom)
import MacLearning (learningSwitch)
import System.Log.Logger

data Msg
  = Block 
  | Unblock 
  | Unmonitor
  deriving (Show)

monitor :: EthernetAddress -- ^ethernet address to monitor
        -> Word32 -- ^IP address to monitor
        -> Chan (Msg, Word32) -- ^'monitor' will write to this channel
        -> IO Policy -- ^the monitoring policy
monitor eth ip actionChan = do
  (countChan, countAction) <- countPkts 1000
  putStrLn $ "Monitoring " ++ show (eth, ip)
  let loop :: Map.Map Switch (Float, Integer) -- per-switch traffic
           -> IO ()
      loop traffic = do
        (sw, count) <- readChan countChan
        let (oldScore, oldCount) = case Map.lookup sw traffic of
              Nothing -> (0, count)
              Just v -> v
        let traffic' = Map.insert sw 
                         (oldScore / 2 + fromIntegral (count - oldCount) + 1, count)
                         traffic
        case Map.lookup sw traffic' of
          Nothing -> fail "sw should be in traffic'"
          Just (score, _) -> do
            putStrLn $ show ip ++ " has score " ++ show score
            if score < 0 then
              writeChan actionChan (Unmonitor, ip)
                     -- this thread terminates
            else do
              if score > 10.0 && oldScore <= 10.0 then do
                putStrLn $ "Blocking " ++ show ip ++ " with score " ++ 
                           show score
                writeChan actionChan (Block, ip)
              else if score < 10.0 && oldScore >= 10.0 then do
                putStrLn $ "Unblocking due to score " ++ show ip
                writeChan actionChan (Unblock, ip)
              else
                return ()
              loop traffic'
  forkIO (loop Map.empty)
  -- TODO(arjun): do not bake this optimization in here
  return (dlSrc eth <&&> dlTyp 0x0800 <&&> nwSrc ip ==> countAction)


dosDetector :: IO (Chan (Policy, Predicate))
dosDetector = do
  (pktChan, inspectPkt) <- getPkts
  cmdChan <- newChan
  resultChan <- newChan
  chan <- mergeChan pktChan cmdChan

  let buildPolPred :: Map.Map Word32 Policy -- ^monitors
                   -- TODO(arjun): block on IP and Eth
                   -> Set.Set Word32 -- ^blocked addresses
                   -> (Policy, Predicate)
      buildPolPred monitors blocked = (pol, pred) 
        where monPol = foldr (<+>) PoBottom (Map.elems monitors) -- TODO(arjun): use unions
              pol = monPol <+> (neg (poDom monPol) ==> inspectPkt)
              f srcIP = nwSrc srcIP <&&> dlTyp 0x0800
              pred = neg (foldr (<||>) matchNone (map f (Set.elems blocked)))


  let loop :: Map.Map Word32 Policy
           -> Set.Set Word32
           -> IO ()
      loop monitors blocked = do
        msg <- readChan chan
        case msg of
          Left (sw, Packet{pktDlSrc=srcMac,pktNwSrc=Just srcIP}) -> 
            case Map.member srcIP monitors of 
              True -> 
                loop monitors blocked -- already monitoring
              False -> do
                pol <- monitor srcMac srcIP cmdChan
                let monitors' = Map.insert srcIP pol monitors
                writeChan resultChan (buildPolPred monitors' blocked)
                loop monitors' blocked
          Left (_, pkt) -> do
            -- We only monitor dlTyp == 0x0800, so this should never occur.
            errorM "monitor" $ "received non-IP packet " ++ show pkt
            loop monitors blocked
          Right (Block, srcIP) -> do
            let blocked' = Set.insert srcIP blocked
            let (pol, pred) = buildPolPred monitors blocked'
            putStrLn $ "Restriction " ++ show pred
            writeChan resultChan (pol, pred)
            loop monitors blocked'
          Right (Unblock, srcIP) -> do
            let blocked' = Set.delete srcIP blocked
            putStrLn $ "Unblocking and updating policy " ++ show srcIP
            writeChan resultChan (buildPolPred monitors blocked')
            loop monitors blocked'
          Right (Unmonitor, srcIP) -> do
            let blocked' = Set.delete srcIP blocked
            let monitors' = Map.delete srcIP monitors
            putStrLn $ "Unmonitor " ++ show srcIP
            writeChan resultChan (buildPolPred monitors' blocked')
            loop monitors' blocked'

  forkIO (loop Map.empty Set.empty >> putStrLn "TERMINATOR")
  writeChan resultChan (buildPolPred Map.empty Set.empty)
  return resultChan

-- TODO(arjun): export the inner loop to get a Chan Pol -> Chan Pol
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