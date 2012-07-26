module Frenetic.PolicyGen
  ( queries
  , realFlood
  , simuFlood
  , simuFloodQuery
  ) where

import Data.Graph.Inductive.Graph
import Frenetic.NetCore.API
import Frenetic.NetCore.Short
import Frenetic.Pattern
import Frenetic.Topo
import System.IO.Unsafe

-- |Get an infinite stream of fresh queries
queries :: IO [Query]
queries = do
  (_, q) <- query 1
  rest <- unsafeInterleaveIO queries
  return (q : rest)

-- | For each switch, just flood every packet.
realFlood :: Topo -> Policy
realFlood topo = poNaryUnion policies where
  ss = switches topo
  policies = [PrTo (fromIntegral s) ==> flood | s <- ss]

-- | For each switch, direct each packet to every port on that switch that's in
-- the topology.  Different from realFlood when dealing with subgraphs because
-- simuFlood preserves behavior when composed into a larger graph.
simuFlood :: Topo -> Policy
simuFlood topo = poNaryUnion policies where
  ss = switches topo
  policies = [PrTo (fromIntegral s) <&> validPort s ==>
              forwardMods (ports' s) | s <- ss]
  ports' s = zip (ports topo s) (repeat top)
  validPort s = prNaryUnion . map (\p -> inPort p) $ ports topo s

-- |Simulate flooding, and observe all packets with query.
simuFloodQuery :: Topo -> Query -> Policy
simuFloodQuery topo q = poNaryUnion policies where
  ss = switches topo
  policies = [PrTo (fromIntegral s) <&> validPort s ==>
              forwardModsQuery (ports' s) [q] | s <- ss]
  ports' s = zip (ports topo s) (repeat top)
  validPort s = prNaryUnion . map (\p -> inPort p) $ ports topo s
