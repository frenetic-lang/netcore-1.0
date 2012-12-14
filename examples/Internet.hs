module Internet where

import Frenetic.NetCore

-- IP packets with destination not 10.0.0/24
public = -- And (PrPattern $ top { ptrnDlTyp = Exact 0x800 })
                 (Not $ NwDst (IPAddressPrefix (IPAddress 0x0a000000) 24))

policy = (Not public ==> allPorts unmodified) <+>
         (public ==> forward [3])

main addr =
  controller addr policy
