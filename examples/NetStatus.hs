module NetStatus where

import Frenetic.NetCore

main = do
  let f evt = putStrLn (show evt)
  let pol = Any ==> [Forward AllPorts unmodified] <+>
            Any ==> [MonitorSwitch f]
  controller pol
