module RemoteController where

import Frenetic.NetCore
import Frenetic.NetCore.JSON
import Frenetic.NetCore.Semantics
import Frenetic.Server
import Text.JSON.Generic
import Data.Generics

main addr = do
  -- putStrLn . show . encodeJSON . MsgPolicy . PolProcessIn Any $ [ActFwd AllPorts unmodified]
  remoteController addr 6634 (-1)

