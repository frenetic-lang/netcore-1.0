module Query1 where

import Frenetic.NetCore
import qualified Data.MultiSet as MS

main = do
  let f (sw, n) = do
        putStrLn ("Counter is: " ++ show n)
  let pol = (Any ==> (MS.singleton $ Forward AllPorts unmodified)) `PoUnion` 
            (Any ==> (MS.singleton $ CountPackets 0 1000 f))
  controller pol
