module PacketOut where

import Frenetic.NetCore
import Control.Concurrent
import Frenetic.NetworkFrames
import Control.Monad

pkt = arpReply (EthernetAddress 10) 200 (EthernetAddress 5) 3000

main = do
  pktOutChan <- newChan
  forkIO $ forever $ do
    threadDelay 1000000   
    writeChan pktOutChan (Loc 1 1, pkt)
    writeChan pktOutChan (Loc 1 2, pkt)
  controller $
    Any ==> [Forward AllPorts unmodified] <+>
    (SendPackets pktOutChan <%> IngressPort 1)