
module VlanBug where

import Frenetic.NetCore
import Frenetic.NetCore.Types

-- Expected topology:
-- 
--   h1 -- (2) s101 (1) --- (1) s102  (2) -- h2
--                                    (3) -- h3
--
-- where "h1 -- (2) s101" means that host 1 is connected to 
-- switch 101 on the switch's port 2.

-- Goal: tag traffic from h1 with VLAN 1 and forward out port 1.  Switch 102
-- should forward all traffic tagged with VLAN 1 out port 2, but it forwards 
-- out port 3 instead.
policy =  (Switch 101 <&&> IngressPort 2) ==> 
            [ Forward (Physical 1) unmodified { modifyDlVlan = Just (Just 1) } ]
      <+> (Switch 102 <&&> DlVlan (Just 1)) ==> forward [2]
      <+> (Switch 102 <&&> DlVlan Nothing) ==> forward [3]

main = controller policy

