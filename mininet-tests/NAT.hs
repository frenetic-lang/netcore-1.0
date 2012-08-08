-- ^Uses NetCore to implement a Network Address Translator
--
-- Does not work on networks with loops.
module NAT where

import Control.Concurrent
import Frenetic.NetCore
import Frenetic.EthernetAddress
import qualified Data.Map as Map
import Frenetic.Common (mergeChan)
import Frenetic.NetCore.Types (poDom, modifyNwSrc)
import Data.Word
import Data.Bits


-- Shorthand for hard-coded IP addresses
addr :: (Word8, Word8, Word8, Word8) -> Word32
addr (a, b, c, d) = 
  let a' = fromIntegral a
      b' = fromIntegral b
      c' = fromIntegral c
      d' = fromIntegral d
    in shift a' 24 .|. shift b' 16 .|. shift c' 8 .|. d'

-- Fake "NAT" MAC address
fakeMAC = 0x1

-- Fake "NAT" IP address
fakeIP = addr (10,0,0,100)

-- IP packets with destination not 10.0.0/24
private = nwDstPrefix 0x0a000000 24
arp = dlTyp 0x0806
ip = dlTyp 0x0800
gateway = dlDst $ ethernetAddress 0 0 0 0 0 1

policy = (arp ==> allPorts unmodified) <+>
         (private ==> allPorts unmodified) <+>
         (gateway ==> modify [(3, modDlDst $ ethernetAddress 0x00 0x00 0x00 0x00 0x00 0x11)])
--   where mods = ethMod {modifyNwSrc = Just $ addr (10,0,0,100)}
--         ethMod = (modDlDst $ ethernetAddress 0xb6 0x42 0xf3 0x1b 0x8e 0x31)

main = controller policy
