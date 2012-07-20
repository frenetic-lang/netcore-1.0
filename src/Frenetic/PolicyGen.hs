module Frenetic.PolicyGen
  ( realFlood
  , simuFlood
  ) where

import Data.Graph.Inductive.Graph
import Frenetic.NetCore.API
import Frenetic.NetCore.Short
import Frenetic.Pattern
import Frenetic.Topo

-- | For each switch, just flood every packet.
realFlood :: Topo -> Policy
realFlood topo = poNaryUnion policies where
  ss = switches topo
  policies = [PrTo (fromIntegral s) ==> flood | s <- ss]

-- | For each switch, direct each packet to every port on that switch that's in
-- the topology.  Different from realFlood when dealing with subgraphs because
-- simuFlood preserves behavior when composed into a larger graph.
simuFlood :: Topo -> Policy
simuFlood topo =  poNaryUnion policies where
  ss = switches topo
  policies = [PrTo (fromIntegral s) <&> validPort s ==> forwardMods (ports' s) | s <- ss]
  ports' s = zip (ports topo s) (repeat top)
  validPort s = prNaryUnion . map (\p -> inPort p) $ ports topo s
