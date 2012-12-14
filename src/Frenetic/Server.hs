module Frenetic.Server
  ( controller
  , dynController
  , controllerProgram
  , consistentController
  , debugController
  , debugDynController
  , remoteController
  ) where

import Frenetic.Hosts.Nettle
import Frenetic.NetCore.Types
import Frenetic.NetCore.JSON
import Frenetic.Common
import Control.Exception (mask, try, SomeException)
import Control.Monad
import Network
import System.IO
import Text.JSON.Generic
import Data.Generics
import System.Log.Logger
import qualified Data.Map as M

-- |Starts an OpenFlow controller that runs dynamic NetCore policies.
--
-- The controller reads NetCore policies from the given channel. When
-- the controller receives a policy on the channel, it compiles it and
-- reconfigures the network to run it.
dynController :: Maybe String
                 -> Chan Policy
                 -> IO ()
dynController host polChan = do
  chan <- newChan
  forkIO $ forever $ do
    pol <- readChan polChan
    writeChan chan (Policy pol)
  nettleServer defaultNettleServerOpts{host=host} chan

-- |Starts an OpenFlow controller that runs a static NetCore policy.
controller :: Maybe String
              -> Policy 
              -> IO ()
controller host policy = do
  ch <- newChan
  writeChan ch policy
  dynController host ch

-- |Starts an OpenFlow controller that runs dynamic NetCore programs.
--
-- Unlike policies, which only specify the forwarding behavior of the network,
-- programs can also configure queues.
controllerProgram :: Maybe String -> Chan Program -> IO ()
controllerProgram host = nettleServer defaultNettleServerOpts{host=host}

-- |Starts an OpenFlow controller that runs dynamic NetCore programs.
--
-- Unlike policies, which only specify the forwarding behavior of the network,
-- programs can also configure queues.
-- consistentController :: Chan (Policy, SwitchID -> [Port] -> IO ()
consistentController host = consistentNettleServer defaultNettleServerOpts{host=host}

-- |Identical to @dynController@, except that the controller logs
-- policies and packets deployed to the network, as well as incoming
-- packets and queries from the network, to @logChan@.
debugDynController :: Maybe String 
                      -> Chan String 
                      -> Chan Policy 
                      -> IO ()
debugDynController host logChan polChan = do
  chan <- newChan
  forkIO $ forever $ do
    pol <- readChan polChan
    writeChan chan (Policy pol)
  nettleServer (NettleServerOpts{ logIO = Just logChan, host = host }) chan

-- |Identical to @controller@, except that the controller logs packets
-- injected into the network and queries gathered from the network.
debugController :: Maybe String 
                   -> Chan String 
                   -> Policy 
                   -> IO ()
debugController host logChan pol = do
  ch <- newChan
  writeChan ch pol
  debugDynController host logChan ch


-- |Start a controller that receives and deploys JSON-formatted policies.
-- Queries and switch events are sent back in JSON format.  Note that this
-- controller only supports a single connection.
remoteController :: Maybe String
                 -> PortNumber    -- ^Port on which to listen for JSON messages.
                 -> Int           -- ^Timeout (in milliseconds).
                                  -- A negative value indicates no timeout.
                 -> IO ()
remoteController host port timeout = withSocketsDo $ do
  -- Start the controller.
  toNetCoreChan <- newChan
  fromNetCoreChan <- newChan
  let opts = defaultNettleServerOpts { host=host }
  forkIO $ forever $ nettleRemoteServer opts toNetCoreChan
  -- Threads started to handle a remote connection will need to be cleaned up
  -- if the connection dies.
  deadChildren <- newEmptyMVar
  -- Open the socket.
  sock <- listenOn $ PortNumber port
  let loop :: Socket -> IO ()
      loop sock = do
          (hdl, hostName, portNum) <- accept sock
          debugM "remoteController" $ (show (hostName, portNum)) ++ " connected."
          -- Ensure there are no lingering records of previously killed
          -- children.
          ignored <- tryTakeMVar deadChildren
          -- Handle the new connection.
          th1 <-
            myForkFinally
              (catch
                (handleRemoteToNetCore hdl timeout toNetCoreChan fromNetCoreChan)
                (\err -> warningM "controller.remoteController" $ show err))
              (\_ -> do {me <- myThreadId; putMVar deadChildren me})
          th2 <-
            myForkFinally
              (catch
                (handleNetCoreToRemote hdl fromNetCoreChan)
                (\err -> warningM "controller.remoteController" $ show err))
              (\_ -> do {me <- myThreadId; putMVar deadChildren me})
          -- Tear it down.
          let children = [th1, th2]
          deadChild <- takeMVar deadChildren
          mapM (\id -> killThread id) $ filter (deadChild /=) children
          catch (hClose hdl)
                (\err -> warningM "controller.remoteController" $ show err)
          debugM "remoteController" $ (show (hostName, portNum)) ++ " closed."
          loop sock
  loop sock
  sClose sock

-- Handle incoming messages.
handleRemoteToNetCore hdl timeout toNetCoreChan fromNetCoreChan = do
  let handleRemoteToNetCore' maybeIns = do
      let defaultQMap = M.empty
      tryMsg <- hGetNetCoreMessage hdl timeout
      case (tryMsg, maybeIns) of
        (Left err,_) -> do
          debugM "remoteController" $ "Error getting message: " ++ show err
          hPutNetCoreError hdl err
          handleRemoteToNetCore' maybeIns
        (Right (MsgPolicy pol'), _) -> do
          debugM "remoteController" $ "Received new policy: " ++ show pol'
          (cbs', ins') <- makeRuntime fromNetCoreChan pol'
          writeChan toNetCoreChan (defaultQMap, cbs', ins', pol')
          handleRemoteToNetCore' $ Just ins'
        (Right (MsgSendPacket id loc pkt), Just ins) -> do
          debugM "remoteController" "Received packet to send."
          let (_,chans) = unzip $ filter (\(id', c) -> id == id') ins
          mapM (\c -> writeChan c (loc, pkt)) chans
          handleRemoteToNetCore' maybeIns
        (Right (MsgSendPacket id loc pkt), Nothing) -> do
          debugM "remoteController" "Received packet to send before first policy."
          hPutNetCoreError hdl ("Received 'send packet' message before the " ++
                                "first policy: " ++
                                show (MsgSendPacket id loc pkt))
        (Right msg, _) -> do
          debugM "remoteController" $ "Unexpected message: " ++ show msg
          hPutNetCoreError hdl $ "Unexpected message: " ++ show msg
          handleRemoteToNetCore' maybeIns
  handleRemoteToNetCore' Nothing

-- Handle output from the network.
handleNetCoreToRemote hdl fromNetCoreChan = do
  msg <- readChan fromNetCoreChan
  hPutNetCoreMessage hdl msg
  handleNetCoreToRemote hdl fromNetCoreChan

-- Duplication from Control.Concurrent in base 6.0.0.0, as we haven't
-- upgraded yet.
myForkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
myForkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then

