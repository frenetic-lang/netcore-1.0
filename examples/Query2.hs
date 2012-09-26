module Query2 where

import Frenetic.NetCore

main = do
  let f (sw, n) = do
        putStrLn ("Counter is: " ++ show n)
  let pol = Any ==> [Forward AllPorts unmodified] <+>
            (Switch 5 <&&> DlSrc (EthernetAddress 1)) ==> [CountPackets 0 1000 f]
  controller pol
