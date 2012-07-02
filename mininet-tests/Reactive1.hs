module Reactive1 where

import Frenetic.NetCore

pat1 :: Pattern
pat1 = top {
  ptrnNwProto = Wildcard 0xFF 0xFE
}

pat2 :: Pattern
pat2 = top {
  ptrnNwProto = Wildcard 0xFE 0xFE
}

-- pat1 and pat2 cover all ports, but the optimizer isn't clever enough to
-- condense them. So, we end up using reactive specialization.
policy = PoUnion (PoBasic (PrPattern pat1) flood)
                 (PoBasic (PrPattern pat2) flood)

main = freneticServer policy  
