module Frenetic.NetCore.Types
  ( -- * Basic types
    Switch
  , Port
  , Queue (..)
  , Vlan
  , Loc (..)
  , PseudoPort (..)
  -- * Actions
  , Action (..)
  , Modification (..)
  , unmodified
  -- * Predicates
  , Predicate (..)
  -- * Packets
  , Packet (..)
  , LocPacket
  -- * Policies
  , Policy (..)
  , Program (..)
  -- * Channel Interface
  , countPkts
  , countBytes
  , getPkts
  ) where

import Frenetic.Common
import Data.Bits
import Data.IORef
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Word
import Frenetic.Pattern
import System.IO.Unsafe
import Data.Maybe (catMaybes)
import Nettle.Ethernet.EthernetAddress
import Nettle.IPv4.IPAddress
import Nettle.OpenFlow (SwitchFeatures)

-- |A switch's unique identifier.
type Switch = Word64

-- |The number of a physical port.
type Port = Word16

-- |The identifier of a queue at a port.
type QueueID = Word32

-- |'Loc' uniquely identifies a port at a switch.
data Loc = Loc Switch Port
  deriving (Eq, Ord, Show)

type LocPacket = (Loc, Packet)

-- |Logical ports.
data PseudoPort
  = Physical Port
  | AllPorts
  | ToQueue Queue
  deriving (Eq, Ord, Show)

-- |VLAN tags. Only the lower 12-bits are used.
type Vlan = Word16

-- |Packets' headers.
data Packet = Packet {
  pktDlSrc :: EthernetAddress, -- ^source ethernet address
  pktDlDst :: EthernetAddress, -- ^destination ethernet address
  pktDlTyp :: Word16, -- ^ethernet type code (e.g., 0x800 for IP packets)
  pktDlVlan :: Maybe Vlan,  -- ^VLAN tag
  pktDlVlanPcp :: Word8, -- ^VLAN priority code
  pktNwSrc :: Maybe IPAddress, -- ^source IP address for IP packets
  pktNwDst :: Maybe IPAddress, -- ^destination IP address for IP packets
  pktNwProto :: Word8, -- ^IP protocol number (e.g., 6 for TCP segments)
  pktNwTos :: Word8, -- ^IP TOS field
  pktTpSrc :: Maybe Word16, -- ^source port for IP packets
  pktTpDst :: Maybe Word16 -- ^destination port for IP packets
} deriving (Show, Eq, Ord)

-- |Predicates to match packets.
data Predicate
  = DlSrc EthernetAddress -- ^Match ethernet source address
  | DlDst EthernetAddress -- ^Match ethernet destination address
  | DlTyp Word16 -- ^Match ethernet type code (e.g., 0x0800 for IP packets)
  | DlVlan (Maybe Word16) -- ^Match VLAN tag
  | DlVlanPcp Word8 -- ^Match VLAN priority
  | NwSrc IPAddressPrefix -- ^Match source IP address
  | NwDst IPAddressPrefix -- ^Match destination IP address
  | NwProto Word8 -- ^Match IP protocol code (e.g., 0x6 indicates TCP segments)
  | NwTos Word8 -- ^Match IP TOS field
  | TpSrcPort Word16 -- ^Match IP source port
  | TpDstPort Word16 -- ^Match IP destination port
  | IngressPort Word16 -- ^Match the ingress port on which packets arrive
  | Switch Switch -- ^Match only at this switch
  | Or Predicate Predicate -- ^Match either predicates
  | And Predicate Predicate -- ^Match both predicates
  | Not Predicate -- ^Not P matches packets that do not match P.
  | Any -- ^Matches all packets
  | None -- ^Matches no packets
  deriving (Eq, Ord, Show)

{-| Policies denote functions from (switch, packet) to packets. -}
data Policy
  = PoBottom -- ^Performs no actions.
  | PoBasic Predicate [Action]
     -- ^Performs the given action on packets matching the given predicate.
  | PoUnion Policy Policy -- ^Performs the actions of both P1 and P2.
  | Restrict Policy Predicate
  | SendPackets (Chan (Loc, ByteString))
    -- ^If a program writes a located packet to this channel, the controller
    -- will send the packet to its location.
  deriving (Eq)

data Queue = Queue Switch Port QueueID Word16
  deriving (Eq, Ord, Show)

data Program
  = Policy Policy
  | WithQueue Switch Port Word16 (Queue -> Program)
  | ProgramUnion Program Program

-- |For each fields with a value Just v, modify that field to be v.
--  If the field is Nothing then there is no modification of that field.
data Modification = Modification {
  modifyDlSrc :: Maybe EthernetAddress,
  modifyDlDst :: Maybe EthernetAddress,
  modifyDlVlan :: Maybe (Maybe Vlan),
  modifyDlVlanPcp :: Maybe Word8,
  modifyNwSrc :: Maybe IPAddress,
  modifyNwDst :: Maybe IPAddress,
  modifyNwTos :: Maybe Word8,
  modifyTpSrc :: Maybe Word16,
  modifyTpDst :: Maybe Word16
} deriving (Ord, Eq, Show)

unmodified :: Modification
unmodified = Modification Nothing Nothing Nothing Nothing Nothing Nothing
                          Nothing Nothing Nothing

type QueryID = Int

nextQueryID :: IORef QueryID
nextQueryID = unsafePerformIO $ newIORef 0

getNextQueryID :: IO QueryID
getNextQueryID = atomicModifyIORef nextQueryID (\i -> (i + 1, i))

data SwitchEvent
  = SwitchConnected Switch SwitchFeatures
  | SwitchDisconnected Switch

data Action
  = Forward PseudoPort Modification
  | CountPackets {
      idOfQuery :: QueryID,
      queryInterval :: Int,
      counterAction :: (Switch, Integer) -> IO ()
    }
  | CountBytes {
      idOfQuery :: QueryID,
      queryInterval :: Int,
      counterAction :: (Switch, Integer) -> IO ()
    }
  | GetPacket {
      idOfQuery :: QueryID,
      getPacketAction :: (Loc, Packet) -> IO ()
    }
  | MonitorSwitch {
      idOfQuery :: QueryID,
      monitorAction :: SwitchEvent -> IO ()
  }
    
      
instance Eq Action where
  (Forward p m) == (Forward p' m') = (p,m) == (p',m')
  q1 == q2 = idOfQuery q1 == idOfQuery q2

instance Show Action where
  show (Forward p m) = show p ++ show m
  show (CountBytes{..}) = 
    "CountBytes (interval=" ++ show queryInterval ++ "ms, id=" ++
       show idOfQuery ++ ")"
  show (CountPackets{..}) = 
    "CountPackets (interval=" ++ show queryInterval ++ "ms, id=" ++
       show idOfQuery ++ ")"
  show (GetPacket{..}) = "GetPacket(id=" ++ show idOfQuery ++  ")"
  show (MonitorSwitch{..}) = "MonitorSwitch"

instance Show Policy where
  show pol = case pol of
    PoBottom -> "PoBottom"
    PoBasic pred acts -> "PoBasic " ++ show pred ++ " " ++ show acts
    PoUnion pol1 pol2 -> "PoUnion (" ++ show pol1 ++ ") (" ++ show pol2 ++ ")"
    Restrict pol' pred -> "Restrict (" ++ show pol' ++ ") (" ++ show pred ++ ")"
    SendPackets _ -> "SendPackets"

-- |Periodically polls the network to counts the number of packets received.
--
-- Returns an 'Action' and a channel. When the 'Action' is used in the
-- active 'Policy', the controller periodically reads the packet counters
-- on the network. The controller returns the number of matching packets
-- on each switch.
countPkts :: Int -- ^polling interval, in milliseconds
          -> IO (Chan (Switch, Integer), [Action])
countPkts millisecondInterval = do
  ch <- newChan
  queryID <- getNextQueryID
  let q = CountPackets queryID millisecondInterval (writeChan ch)
  return (ch, [q])

-- |Periodically polls the network to counts the number of bytes received.
--
-- Returns an 'Action' and a channel. When the 'Action' is used in the
-- active 'Policy', the controller periodically reads the packet counters
-- on the network. The controller returns the number of matching packets
-- on each switch.
countBytes :: Int -- ^polling interval, in milliseconds
           -> IO (Chan (Switch, Integer), [Action])
countBytes millisecondInterval = do
  ch <- newChan
  queryID <- getNextQueryID
  let q = CountBytes queryID millisecondInterval (writeChan ch)
  return (ch, [q])

-- |Sends packets to the controller.
--
-- Returns an 'Action' and a channel. When the 'Action' is used in the active
-- 'Policy', all matching packets are sent to the controller. These packets
-- are written into the channel.
getPkts :: IO (Chan (Loc, Packet), [Action])
getPkts = do
  ch <- newChan
  queryID <- getNextQueryID
  let q = GetPacket queryID (writeChan ch)
  return (ch, [q])

