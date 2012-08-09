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

score :: Float -- ^old score
      -> Integer -- ^old byte counter
      -> Integer -- ^new byte counter
      -> (Float, Integer) -- ^new score
score score count count' = (0.7 * score + fromIntegral (count' - count), count')

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
              Nothing -> (0, 0)
              Just v -> v
        let traffic' = Map.insert sw (score oldScore oldCount count) traffic
        let score = case Map.lookup sw traffic' of
              Nothing -> error "sw should be in traffic'"
              Just (s, _) -> s
        debugM "monitor" $  show ip ++ " has score " ++ (printf "%7.2f" score) 
                            ++ show (oldScore, oldCount, count)
        let action = if score < 1 then Just Unmonitor
                     else if score > 5000 && oldScore <= 5000 then Just Block
                     else if score < 100 && oldScore >= 100 then Just Unblock
                     else Nothing
        case action of
          Nothing -> loop traffic'
          Just act -> do 
            writeChan actionChan (act, ip)
            infoM "monitor" $ show act ++ " " ++ show ip
            loop traffic'
  forkIO (loop Map.empty)
  -- TODO(arjun): do not bake this optimization in here
  return (dlSrc eth <&&> dlTyp 0x0800 <&&> nwSrc ip ==> countAction)

buildPolPred :: Action
             -> Map.Map Word32 Policy -- ^monitors
             -- TODO(arjun): block on IP and Eth
             -> Set.Set Word32 -- ^blocked addresses
             -> (Policy, Predicate)
buildPolPred inspectPkt monitors blocked = (pol, pred)
        -- TODO(arjun): use unions
  where monPol = foldr (<+>) PoBottom (Map.elems monitors)
        pol = monPol <+> (neg (poDom monPol) ==> inspectPkt)
        f srcIP = nwSrc srcIP <&&> dlTyp 0x0800
        pred = neg (foldr (<||>) matchNone (map f (Set.elems blocked)))


monitoringProcess :: IO (Chan (Policy, Predicate))
monitoringProcess = do
  (pktChan, inspectPkt) <- getPkts
  cmdChan <- newChan
  resultChan <- newChan
  chan <- select pktChan cmdChan

  let loop :: Map.Map Word32 Policy
           -> Set.Set Word32
           -> IO ()
      loop monitors blocked = do
        msg <- readChan chan
        (monitors', blocked') <- case msg of
          Left (sw, Packet{pktDlSrc=srcMac,pktNwSrc=Just srcIP}) ->
            case Map.member srcIP monitors of
              True -> return (monitors, blocked) -- already monitoring
              False -> do
                pol <- monitorHost srcMac srcIP cmdChan
                return (Map.insert srcIP pol monitors, blocked)
          Left (_, pkt) ->
            -- We only monitor dlTyp == 0x0800, so this should never occur.
            return (monitors, blocked)
          Right (Block, srcIP) -> return (monitors, Set.insert srcIP blocked)
          Right (Unblock, srcIP) -> return (monitors, Set.delete srcIP blocked)
          Right (Unmonitor, srcIP) -> 
            return (Map.delete srcIP monitors, Set.delete srcIP blocked)
        writeChan resultChan (buildPolPred inspectPkt monitors' blocked')
        loop monitors' blocked'
  forkIO (loop Map.empty Set.empty)
  writeChan resultChan (buildPolPred inspectPkt Map.empty Set.empty)
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
