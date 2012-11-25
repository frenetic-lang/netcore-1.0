module Frenetic.Update1 where

import Frenetic.Common
import Nettle.OpenFlow.Match
import Frenetic.NetCore.Compiler
import Frenetic.NetCore.Types
import Frenetic.NetCore.Short

internal_policy p ver =
  p <%> (DlVlan (Just ver))
  
edge_policy p ver =
  (Sequence p (Any ==> [Forward InPort (modDlVlan (Just ver))])) <%> (DlVlan Nothing)
  
gen_update_pols orig ver switches extPorts =
  (internal_policy orig ver,
   Sequence (edge_policy orig ver) 
  ((prOr $ map (\sw -> ((prOr $ map IngressPort (extPorts sw)) <&&> (Switch sw))) switches) 
   ==> [Forward InPort stripDlVlan]))
  