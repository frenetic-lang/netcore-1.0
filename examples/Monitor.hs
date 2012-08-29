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

newScore :: Float -- ^old score
      -> Integer -- ^old byte counter
      -> Integer -- ^new byte counter
      -> (Float, Integer) -- ^new score
newScore score count count' =
   (0.7 * score + fromIntegral (count' - count), count')

nextState :: Float -> Float -> Maybe Msg
nextState score score' =
  if score' < 1 then Just Unmonitor
  else if score' > 5000 && score <= 5000 then Just Block
  else if score' < 100 && score >= 100 then Just Unblock
  else Nothing

-- |'monitorHost eth ip chan' produces a policy that monitors traffic from a
-- single host. Monitor writes commands to 'chan' that direct
-- 'monitoringProcess' to either block, unblock, or stop monitoring the host.
monitorHost :: EthernetAddress -- ^ethernet address to monitor
            -> IPAddress -- ^IP address to monitor
            -> Chan (Msg, IPAddress) -- ^'monitor' will write to this channel
            -> IO Policy -- ^the monitoring policy
monitorHost eth ip actionChan = do
  (countChan, countAction) <- countBytes 1000
  infoM "monitor" $ "monitoring " ++ show ip
  let loop :: Map.Map Switch (Float, Integer) -- per-switch traffic
           -> IO ()
      loop traffic = do
        (sw, count') <- readChan countChan
        let (score, count) = case Map.lookup sw traffic of
              Nothing -> (0, 0)
              Just v -> v
        let traffic' = Map.insert sw (newScore score count count') traffic
        let score' = case Map.lookup sw traffic' of
              Nothing -> error "sw should be in traffic'"
              Just (s, _) -> s
        debugM "monitor" $  show ip ++ " has score " ++ printf "%7.2f" score'
        let action = nextState score score'
        case action of
          Nothing -> loop traffic'
          Just act -> do
            writeChan actionChan (act, ip)
            infoM "monitor" $ show act ++ " " ++ show ip
            loop traffic'
  forkIO (loop Map.empty)
  -- TODO(arjun): do not bake this optimization in here
  return (DlSrc eth <&&> DlTyp 0x0800 <&&> NwSrc (IPAddressPrefix ip 32) ==> countAction)

monitoringProcess :: IO (Chan (Policy, Predicate))
monitoringProcess = do
  (pktChan, inspectPkt) <- getPkts
  cmdChan <- newChan
  resultChan <- newChan
  chan <- select pktChan cmdChan
  let loop :: Map.Map IPAddress Policy
           -> Set.Set IPAddress
           -> IO ()
      loop monitors blocked = do
        msg <- readChan chan
        -- Calculate the new monitoring policy and blocking predicate.
        (monitors', blocked') <- case msg of
          -- Monitor new hosts
          Left (sw, Packet{pktDlSrc=srcMac,pktNwSrc=Just srcIP}) ->
            case Map.member srcIP monitors of
              True -> return (monitors, blocked) -- already monitoring
              False -> do
                pol <- monitorHost srcMac srcIP cmdChan
                return (Map.insert srcIP pol monitors, blocked)
          Left (_, pkt) ->
            -- We only monitor dlTyp == 0x0800, so this should never occur.
            return (monitors, blocked)
          -- Block if directed to do so by monitorHost
          Right (Block, srcIP) -> return (monitors, Set.insert srcIP blocked)
          -- Unblock if directed
          Right (Unblock, srcIP) -> return (monitors, Set.delete srcIP blocked)
          -- Unmonitor if directed
          Right (Unmonitor, srcIP) ->
            return (Map.delete srcIP monitors, Set.delete srcIP blocked)
        -- run all monitors'
        let monitoringPol = foldr (<+>) PoBottom (Map.elems monitors)
        -- block all blocked' hosts IP traffic
        let mkBlock srcIP = NwSrc (IPAddressPrefix srcIP 32) <&&> DlTyp 0x0800
        let blockingPred =
              Not $ foldr (<||>) None (map mkBlock (Set.elems blocked'))
        -- run monitoringPol, and send all other packets to the controller,
        -- so we can monitor them.
        let pol = monitoringPol <+> (Not (poDom monitoringPol) ==> inspectPkt)
        writeChan resultChan (pol, blockingPred)
        loop monitors' blocked'
  forkIO (loop Map.empty Set.empty)
  writeChan resultChan (Any ==> inspectPkt, None)
  return resultChan

-- |'monitor routeChan' runs a monitoring process that detects
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
