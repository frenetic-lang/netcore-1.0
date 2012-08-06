module Frenetic.Server
  ( controller
  , dynController
  ) where

import Frenetic.Hosts.Nettle
import Frenetic.NetCore.Types
import Frenetic.Common

-- |Starts an OpenFlow controller that runs dynamic NetCore programs.
--
-- The controller reads NetCore programs from the given channel. When
-- the controller receives a program on the channel, it compiles it and
-- reconfigures the network to run it.
dynController :: Chan Policy 
              -> Chan (Loc, ByteString) -- ^packets to emit
              -> IO ()
dynController = nettleServer

-- |Starts an OpenFlow controller that runs a static NetCore program.
controller :: Policy -> IO ()
controller policy = do
  ch <- newChan
  writeChan ch policy
  pktChan <- newChan
  dynController ch pktChan