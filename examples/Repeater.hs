module Repeater where

import Frenetic.NetCore

policy = Any ==> allPorts unmodified
-- policy = Not (TpSrcPort 80) ==> 
--          [Forward (Physical 1) unmodified]
--         <+>
--         (Not (TpSrcPort 22) ==> 
--          [Forward (Physical 2) unmodified]
--          <+>
--          (TpSrcPort 22 <||> TpSrcPort 80) ==> 
--          [Forward (Physical 3) unmodified])
--
main = controller policy
