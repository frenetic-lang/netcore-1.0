module Frenetic.Server
  ( controller
  , dynController
  , controllerProgram
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
dynController :: Chan Policy
              -> IO ()
dynController polChan = do
  chan <- newChan
  forkIO $ forever $ do
    pol <- readChan polChan
    writeChan chan (Policy pol)
  nettleServer defaultNettleServerOpts chan

-- |Starts an OpenFlow controller that runs a static NetCore policy.
controller :: Policy -> IO ()
controller policy = do
  ch <- newChan
  writeChan ch policy
  dynController ch

-- |Starts an OpenFlow controller that runs dynamic NetCore programs.
--
-- Unlike policies, which only specify the forwarding behavior of the network,
-- programs can also configure queues.
controllerProgram :: Chan Program -> IO ()
controllerProgram = nettleServer defaultNettleServerOpts

-- |Identical to @dynController@, except that the controller logs
-- policies and packets deployed to the network, as well as incoming
-- packets and queries from the network, to @logChan@.
debugDynController :: Chan String -> Chan Policy -> IO ()
debugDynController logChan polChan = do
  chan <- newChan
  forkIO $ forever $ do
    pol <- readChan polChan
    writeChan chan (Policy pol)
  nettleServer (NettleServerOpts { logIO = Just logChan }) chan

-- |Identical to @controller@, except that the controller logs packets
-- injected into the network and queries gathered from the network.
debugController :: Chan String -> Policy -> IO ()
debugController logChan pol = do
  ch <- newChan
  writeChan ch pol
  debugDynController logChan ch

