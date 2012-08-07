import Frenetic.Server
import Frenetic.NetCore.Types
import Frenetic.Compat
import Frenetic.Pattern
import Topologies
import qualified Policies.ShortestPath as SP
import System.IO
import Frenetic.NetCore.Action
import Debug.Trace (trace)

main = do
  topoInput <- readFile "../topologies/routing_topo.top"
  let topo :: Topology
      topo = parseTopo topoInput
  let policy = SP.mkPolicy topo
  hPutStrLn stderr ("COLE: " ++ show policy)
  freneticServer (PoUnion policy arpFlood)

arpFlood :: Policy
arpFlood = PoBasic (PrPattern top{ptrnDlTyp = Wildcard 0x806 0}) flood
