module Frenetic.Update1 where

import Frenetic.Common
import Nettle.OpenFlow.Match
import Frenetic.NetCore.Compiler
import Frenetic.NetCore.Types
import Frenetic.NetCore.Short
import Frenetic.NetCore.Util (poDom)

infixr 5 <!>
(<!>) pol acts = pol <+> (Not (poDom pol) ==> acts)

internal_policy p ver =
  p <%> (DlVlan (Just ver))
  
edge_policy p ver =
  (Sequence p (Any ==> [Forward InPort (modDlVlan (Just ver))])) <%> (DlVlan Nothing)

strip_policy switches extPorts =
  (((prOr $ map (\sw -> ((prOr $ map IngressPort (extPorts sw)) <&&> (Switch sw))) switches) 
    ==> [Forward InPort stripDlVlan]) <!> [Forward InPort unmodified])
  
gen_update_pols orig ver switches extPorts =
  (Sequence (internal_policy orig ver) (strip_policy switches extPorts),
   Sequence (edge_policy orig ver) (strip_policy switches extPorts))
  