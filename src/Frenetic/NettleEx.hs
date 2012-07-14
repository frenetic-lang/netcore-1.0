-- |Nettle with additional features. None of this code is Frenetic-specific.
module Frenetic.NettleEx
  ( Nettle
  , sendTransaction
  , module Nettle.OpenFlow
  , module Nettle.Servers.Server
  , closeServer
  , acceptSwitch
  , startOpenFlowServerEx
  -- TODO(arjun): should not be exported, IMO.
  , txHandlers 
  ) where

-- Do not commingle with Frenetic. Do not import Frenetic.Util.
import Control.Concurrent.Chan
import Data.Map (Map)
import qualified Data.Map as Map
import Nettle.OpenFlow hiding (intersect)
import qualified Nettle.Servers.Server as Server
import Nettle.Servers.Server hiding (acceptSwitch, closeServer)

data Nettle = Nettle {
  server :: OpenFlowServer,
  switches :: IORef (Map SwitchID SwitchHandle),
  nextTxId :: IORef TransactionID,
  -- ^ Transaction IDs in the semi-open interval '[0, nextTxId)' are in use.
  -- @sendTransaction@ tries to reserve 'nextTxId' atomically. There could be
  -- available transaction IDs within the range, but we will miss them
  -- until 'nextTxId' changes.
  txHandlers :: IORef (Map TransactionID (SCMessage -> IO ()))
}

startOpenFlowServerEx :: Maybe HostName -> ServerPortNumber -> IO Nettle
startOpenFlowServerEx host port = do
  server <- Server.startOpenFlowServer Nothing -- bind to this address
                                6633    -- port to listen on
  switches <- newIORef Map.empty
  nextTxId <- newIORef 10
  txHandlers <- newIORef Map.empty
  return (Nettle server switches nextTxId txHandlers)

acceptSwitch :: Nettle -> IO (SwitchHandle, SwitchFeatures)
acceptSwitch nettle = do
  (switch, switchFeatures) <- Server.acceptSwitch (server nettle)
  modifyIORef (switches nettle) (Map.insert (handle2SwitchID switch) switch)
  return (switch, switchFeatures)

closeServer :: Nettle -> IO ()
closeServer nettle = Server.closeServer (server nettle)

-- |spin-lock until we acquire a 'TransactionID'
reserveTxId :: Nettle -> IO TransactionID
reserveTxId nettle@(Nettle _ _ nextTxId _) = do
  let getNoWrap n = case n == maxBound of
        False -> (n + 1, Just n)
        True -> (n, Nothing)
  r <- atomicModifyIORef nextTxId getNoWrap
  case r of
    Just n -> return n
    Nothing -> reserveTxId nettle

releaseTxId :: TransactionID -> Nettle -> IO ()
releaseTxId n (Nettle _ _ nextTxId _) = do
  let release m = case m == n of
        False -> (m, ())
        True -> (m - 1, ())
  atomicModifyIORef nextTxId release

csMsgWithResponse :: CSMessage -> Bool
csMsgWithResponse msg = case msg of
  CSHello -> True
  CSEchoRequest _ -> True
  FeaturesRequest -> True
  StatsRequest _ -> True
  BarrierRequest -> True
  GetQueueConfig _ -> True
  otherwise -> False

hasMoreReplies :: SCMessage -> Bool
hasMoreReplies msg = case msg of
  StatsReply (FlowStatsReply True _) -> True
  StatsReply (TableStatsReply True _) -> True
  StatsReply (PortStatsReply True _) -> True
  StatsReply (QueueStatsReply True _) -> True
  otherwise -> False

sendTransaction :: Nettle
                -> SwitchHandle -- ^target switch
                -> [CSMessage] -- ^related messages
                -> ([SCMessage] -> IO ()) -- ^callback
                -> IO ()
sendTransaction nettle@(Nettle _ _ _ txHandlers) sw reqs callback = do
  txId <- reserveTxId nettle
  resps <- newIORef ([] :: [SCMessage])
  remainingResps  <- newIORef (length (filter csMsgWithResponse reqs))
  let handler msg = do
        modifyIORef resps (msg:) -- Nettle client operates in one thread
        unless (hasMoreReplies msg) $ do
          modifyIORef remainingResps (\x -> x - 1)
          n <- readIORef remainingResps
          when (n == 0) $ do
            resps <- readIORef resps
            atomicModifyIORef txHandlers (\hs -> (Map.delete txId hs, ()))
            releaseTxId txId nettle
            callback resps
  atomicModifyIORef txHandlers (\hs -> (Map.insert txId handler hs, ()))
  mapM_ (sendToSwitch sw) (zip [txId ..] reqs)
  return ()
