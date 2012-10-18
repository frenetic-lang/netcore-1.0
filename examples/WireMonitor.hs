module WireMonitor where

import Control.Concurrent
import Control.Monad (forever)
import Frenetic.NetCore

policy = Any ==> allPorts unmodified

main = do (countChan, countAction) <- countBytes 1000
          let policy = IngressPort 1 ==> forward [2] <+>
                       IngressPort 2 ==> forward [1] <+>
                       Any ==> countAction
          forkIO $ forever $ do
            (x,bytes) <- readChan countChan
            putStrLn (show x ++ " Bytes: " ++ show bytes)
          controller policy
