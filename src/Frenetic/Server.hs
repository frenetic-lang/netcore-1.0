module Frenetic.Server
  ( controller
  , dynController
  , controllerProgram
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
  nettleServer chan

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
controllerProgram = nettleServer