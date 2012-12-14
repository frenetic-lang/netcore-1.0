module NetStatus where

import Frenetic.NetCore

main addr = do
  let f evt = putStrLn (show evt)
  let pol = Any ==> [Forward AllPorts unmodified] <+>
            Any ==> [MonitorSwitch f]
  controller addr pol
