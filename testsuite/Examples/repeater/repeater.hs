import Frenetic.Server
import Frenetic.NetCore.Types
import Frenetic.NetCore.Action
import Frenetic.Compat
import Frenetic.Pattern
import Topologies
import qualified Policies.ShortestPath as SP
import System.IO (readFile)
import Debug.Trace (trace, traceShow)

main = freneticServer $ PoUnion policy3 arp_policy
--main = do
--  topoInput <- readFile "../topologies/repeater_topo.top"
--  let topo :: Topology
--      topo = parseTopo topoInput
--  let policy = SP.mkPolicy topo
--  traceShow policy $ freneticServer policy

dlTypIP = Wildcard 0x800 0

policy2 = PoUnion
           (PoBasic (PrPattern top{ptrnInPort = (Just 1)})
                    (forward 2))
           (PoBasic (PrPattern top{ptrnInPort = (Just 2)})
                    (forward 1))

policy3 = PoUnion
           (PoBasic (PrPattern top{ptrnNwSrc = h1,
                                   ptrnNwDst = h2,
                                   ptrnDlTyp = dlTypIP})
                    (forward 2))
           (PoBasic (PrPattern top{ptrnNwSrc = h2,
                                   ptrnNwDst = h1,
                                   ptrnDlTyp = dlTypIP})
                    (forward 1))
    where h1 = Wildcard 0xA000001 0
          h2 = Wildcard 0xA000002 0

arp_policy :: Policy
arp_policy = PoBasic (PrPattern top{ptrnDlTyp = arpType}) flood
    where arpType = Wildcard 0x806 0

flood_policy :: Policy
flood_policy = PoBasic (PrPattern top) flood

