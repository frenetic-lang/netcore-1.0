module Monitor
  ( main
  , monitor
  ) where

import Control.Concurrent
import Data.Word
import qualified Data.Set as Set
import qualified Data.Map as Map
import Frenetic.NetCore
import Control.Monad (forever)
import Frenetic.NetCore.Types (poDom)
import MacLearning (learningSwitch)
import System.Log.Logger
import Text.Printf

data Msg
  = Block
  | Unblock
  | Unmonitor
  deriving (Show)

monitorHost :: EthernetAddress -- ^ethernet address to monitor
            -> Word32 -- ^IP address to monitor
            -> Chan (Msg, Word32) -- ^'monitor' will write to this channel
            -> IO Policy -- ^the monitoring policy
monitorHost eth ip actionChan = do
  (countChan, countAction) <- countBytes 1000
  infoM "monitor" $ "monitoring " ++ show ip
  let loop :: Map.Map Switch (Float, Integer) -- per-switch traffic
           -> IO ()
      loop traffic = do
        (sw, count) <- readChan countChan
        let (oldScore, oldCount) = case Map.lookup sw traffic of
              Nothing -> (0, count)
              Just v -> v
        let traffic' = Map.insert sw
                         (oldScore * 0.7 + fromIntegral (count - oldCount),
                          count)
                         traffic
        case Map.lookup sw traffic' of
          Nothing -> fail "sw should be in traffic'"
          Just (score, _) -> do
            debugM "monitor" $  show ip ++ " has score " ++ (printf "%7.2f" score)
            if score < 0 then do
              infoM "monitor" $ "unmonitoring " ++ show ip
              writeChan actionChan (Unmonitor, ip)
                     -- this thread terminates
            else do
              if score > 1000.0 && oldScore <= 1000.0 then do
                infoM "monitor" $ "blocking " ++ show ip
                writeChan actionChan (Block, ip)
              else if score < 10.0 && oldScore >= 10.0 then do
                infoM "monitor" $ "unblocking " ++ show ip
                writeChan actionChan (Unblock, ip)
              else
                return ()
              loop traffic'
  forkIO (loop Map.empty)
  -- TODO(arjun): do not bake this optimization in here
  return (dlSrc eth <&&> dlTyp 0x0800 <&&> nwSrc ip ==> countAction)

monitoringProcess :: IO (Chan (Policy, Predicate))
monitoringProcess = do
  (pktChan, inspectPkt) <- getPkts
  cmdChan <- newChan
  resultChan <- newChan
  chan <- select pktChan cmdChan

  let buildPolPred :: Map.Map Word32 Policy -- ^monitors
                   -- TODO(arjun): block on IP and Eth
                   -> Set.Set Word32 -- ^blocked addresses
                   -> (Policy, Predicate)
      buildPolPred monitors blocked = (pol, pred)
              -- TODO(arjun): use unions
        where monPol = foldr (<+>) PoBottom (Map.elems monitors)
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
                pol <- monitorHost srcMac srcIP cmdChan
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
            writeChan resultChan (pol, pred)
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

-- |'monitorHosts routeChan' runs a monitoring process that detects
-- hosts that send too much traffic. The resulting policy is a
-- restriction of 'routeChan' to exclude those hosts.
monitor :: Chan Policy -- ^policy that establishes connnectivity
                       --  between hosts
        -> IO (Chan Policy) -- ^a restriction of 'routeChan'
monitor routeChan = do
  resultChan <- newChan
  monitorChan <- monitoringProcess
  chan <- both routeChan monitorChan
  forkIO $ forever $ do
    (routes, (monitors, restriction)) <- readChan chan
    writeChan resultChan ((routes <%> restriction) <+> monitors)
  return resultChan

-- |Runs a learning switch and a monitoring process that blocks hosts that
-- send too much data. Blocked hosts are eventually unblocked, if they
-- stop sending traffic.
main = do
  routeChan <- learningSwitch
  polChan <- monitor routeChan
  pktChan <- newChan
  dynController polChan pktChan
