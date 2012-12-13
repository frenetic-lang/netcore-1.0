module ReserveWeb where

import Frenetic.NetCore
import Control.Concurrent

main addr = do
  chan <- newChan
  writeChan chan $
    WithQueue 1 1 200 $ \q ->
      Policy $
        (Switch 1 <&&> IngressPort 2 ==> [Forward (ToQueue q) unmodified]) <+>
        (Switch 1 <&&> IngressPort 1 ==> [Forward (Physical 2) unmodified])
  controllerProgram addr chan