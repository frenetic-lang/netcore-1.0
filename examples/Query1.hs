module Query1 where

import Frenetic.NetCore
import System.IO (hFlush, stdout)

main addr = do
  let f (sw, n) = do
        putStrLn ("Counter is: " ++ show n)
        hFlush stdout
  let pol = Any ==> [Forward AllPorts unmodified] <+>
            Any ==> [CountPackets 0 1000 f]
  controller addr pol
