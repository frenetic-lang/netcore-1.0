module Frenetic.Server
  ( controller
  , dynController
  , controllerProgram
  , consistentController    
  , debugController
  , debugDynController
  ) where

import Frenetic.Hosts.Nettle
import Frenetic.NetCore.Types
import Frenetic.Common
import Control.Monad

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
consistentController = consistentNettleServer

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

