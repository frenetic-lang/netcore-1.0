module Frenetic.NetCore.Types
  ( -- * Basic types
    Switch
  , Port
  , Vlan
  , Loc (..)
  , PseudoPort (..)
  -- * Actions
  , Action (..)
  , Counter (..)
  , Modification (..)
  , unmodified
  , isPktQuery
  , isForward
  -- ** Basic actions
  , countPkts
  , countBytes
  , getPkts
  -- * Predicates
  , Predicate (..)
  , exactMatch
  -- * Packets
  , Packet (..)
  -- * Policies
  , Policy (..)
  -- * Tools
  , modifiedFields
  , prUnIntersect
  , prUnUnion
  , poUnUnion
  , poDom
  , EthernetAddress (..)
  , IPAddress (..)
  , IPAddressPrefix (..)
  , broadcastAddress
  , ethernetAddress
  , ipAddress
  , size
  ) where

import Frenetic.Common
import Data.Bits
import Data.IORef
import qualified Data.List as List
import qualified Data.MultiSet as MS
import qualified Data.Set as Set
import Data.Word
import Frenetic.Pattern
import System.IO.Unsafe
import Data.Maybe (catMaybes)
import Nettle.Ethernet.EthernetAddress
import Nettle.IPv4.IPAddress

-- |A switch's unique identifier.
type Switch = Word64

-- |The number of a physical port.
type Port = Word16

-- |'Loc' uniquely identifies a port at a switch.
data Loc = Loc Switch Port
  deriving (Eq, Ord, Show)

-- |Logical ports.
data PseudoPort
  = Physical Port
  | AllPorts
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
  pktTpDst :: Maybe Word16, -- ^destination port for IP packets
  pktInPort :: Port -- ^ingress port on the switch where the packet was
                    -- received
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
  | PoBasic Predicate (MS.MultiSet Action)
     -- ^Performs the given action on packets matching the given predicate.
  | PoUnion Policy Policy -- ^Performs the actions of both P1 and P2.
  deriving (Eq, Ord, Show)

-- |Names of common header fields.
data Field
  = FDlSrc | FDlDst | FDlVlan | FDlVlanPcp | FNwSrc | FNwDst | FNwTos 
  | FTpSrc | FTpDst | FNwProto | FInPort
  deriving (Eq, Ord, Show)

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

-- |A predicate that exactly matches a packet's headers.
exactMatch :: Packet -> Predicate
exactMatch (Packet{..}) = foldl f None lst
  where f pr Nothing = pr
        f pr (Just pr') = And pr pr'
        lst = [ Just (DlSrc pktDlSrc), 
                Just (DlDst pktDlDst),
                Just (DlTyp pktDlTyp),
                Just (DlVlan pktDlVlan),
                Just (DlVlanPcp pktDlVlanPcp),
                case pktNwSrc of
                  Nothing -> Nothing
                  Just v -> Just (NwSrc (IPAddressPrefix v 32)),
                case pktNwDst of
                  Nothing -> Nothing
                  Just v -> Just (NwDst (IPAddressPrefix v 32)),
                Just (NwProto pktNwProto), 
                Just (NwTos pktNwTos), 
                fmap TpSrcPort pktTpSrc,
                fmap TpDstPort pktTpDst, 
                Just (IngressPort pktInPort) ]

unmodified :: Modification
unmodified = Modification Nothing Nothing Nothing Nothing Nothing Nothing
                          Nothing Nothing Nothing


modifiedFields :: Modification -> Set Field
modifiedFields (Modification{..}) = Set.fromList (catMaybes fields) where
  fields = [ case modifyDlSrc of { Just _ -> Just FDlSrc; Nothing -> Nothing }
           , case modifyDlDst of { Just _ -> Just FDlDst; Nothing -> Nothing }
           , case modifyDlVlan of { Just _ -> Just FDlVlan; Nothing -> Nothing }
           , case modifyDlVlanPcp of { Just _ -> Just FDlVlanPcp;
                                       Nothing -> Nothing }
           , case modifyNwSrc of { Just _ -> Just FNwSrc; Nothing -> Nothing }
           , case modifyNwDst of { Just _ -> Just FNwDst; Nothing -> Nothing }
           , case modifyNwTos of { Just _ -> Just FNwTos; Nothing -> Nothing }
           , case modifyTpSrc of { Just _ -> Just FTpSrc; Nothing -> Nothing }
           , case modifyTpDst of { Just _ -> Just FTpDst; Nothing -> Nothing }
           ]

type QueryID = Int

nextQueryID :: IORef QueryID
nextQueryID = unsafePerformIO $ newIORef 0

getNextQueryID :: IO QueryID
getNextQueryID = atomicModifyIORef nextQueryID (\i -> (i + 1, i))

data Counter = CountPackets | CountBytes deriving (Eq, Ord)

data Action
  = Forward PseudoPort Modification
  | NumPktQuery {
      idOfQuery :: QueryID,
      numPktQueryChan :: Chan (Switch, Integer),
      queryInterval :: Int,
      countField :: Counter
    }
  | PktQuery {
      pktQueryChan :: Chan (Switch, Packet),
      idOfQuery :: QueryID
    }
  deriving (Eq)

instance Ord Action where
  compare (Forward p m) (Forward p' m') = compare (p,m) (p',m')
  compare (Forward _ _) _ = LT
  compare _ (Forward _ _) = GT
  compare q1 q2 = compare (idOfQuery q1) (idOfQuery q2)

instance Show Action where
  show (Forward p m) = show p ++ show m
  show (NumPktQuery{..}) =
    "countPkts(interval=" ++ show queryInterval ++ "ms, id=" ++
    show idOfQuery ++ ")"
  show (PktQuery{..}) = "getPkts(id=" ++ show idOfQuery ++  ")"


isPktQuery (PktQuery _ _) = True
isPktQuery _               = False

isForward :: Action -> Bool
isForward (Forward _ _) = True
isForward _ = False

mkCountQuery :: Counter -> Int -> IO (Chan (Switch, Integer), MultiSet Action)
mkCountQuery counter millisecondInterval = do
  ch <- newChan
  queryID <- getNextQueryID
  let q = NumPktQuery queryID ch millisecondInterval counter
  return (ch, MS.singleton q)

-- ^Periodically polls the network to counts the number of packets received.
--
-- Returns an 'Action' and a channel. When the 'Action' is used in the
-- active 'Policy', the controller periodically reads the packet counters
-- on the network. The controller returns the number of matching packets
-- on each switch.
countPkts :: Int -- ^polling interval, in milliseconds
          -> IO (Chan (Switch, Integer), MultiSet Action)
countPkts = mkCountQuery CountPackets

-- ^Periodically polls the network to counts the number of bytes received.
--
-- Returns an 'Action' and a channel. When the 'Action' is used in the
-- active 'Policy', the controller periodically reads the packet counters
-- on the network. The controller returns the number of matching packets
-- on each switch.
countBytes :: Int -- ^polling interval, in milliseconds
           -> IO (Chan (Switch, Integer), MultiSet Action)
countBytes = mkCountQuery CountBytes


-- ^Sends packets to the controller.
--
-- Returns an 'Action' and a channel. When the 'Action' is used in the active
-- 'Policy', all matching packets are sent to the controller. These packets
-- are written into the channel.
getPkts :: IO (Chan (Switch, Packet), MultiSet Action)
getPkts = do
  ch <- newChan
  queryID <- getNextQueryID
  let q = PktQuery ch queryID
  return (ch, MS.singleton q)

-- |Get back all predicates in the intersection.  Does not return any naked
-- intersections.
prUnIntersect :: Predicate -> [Predicate]
prUnIntersect po = List.unfoldr f [po] where
  f predicates = case predicates of
    [] -> Nothing
    (And p1 p2) : rest -> f (p1 : (p2 : rest))
    p : rest -> Just (p, rest)

-- |Get back all predicates in the union.  Does not return any naked unions.
prUnUnion :: Predicate -> [Predicate]
prUnUnion po = List.unfoldr f [po] where
  f predicates = case predicates of
    [] -> Nothing
    (Or p1 p2) : rest -> f (p1 : (p2 : rest))
    p : rest -> Just (p, rest)


-- |Get back all basic policies in the union.  Does not return any unions.
poUnUnion :: Policy -> [Policy]
poUnUnion po = List.unfoldr f [po] where
  f policies = case policies of
    [] -> Nothing
    (PoUnion p1 p2) : rest -> f (p1 : (p2 : rest))
    p : rest -> Just (p, rest)

-- |Returns a predicate that matches the domain of the policy.
poDom :: Policy -> Predicate
poDom PoBottom = None
poDom (PoBasic pred _) = pred
poDom (PoUnion pol1 pol2) = Or (poDom pol1) (poDom pol2)

-- |Returns the approximate size of the policy
size :: Policy -> Int
size PoBottom = 1
size (PoBasic p _) = prSize p + 1
size (PoUnion p1 p2) = size p1 + size p2 + 1

-- |Returns the approximate size of the predicate
prSize :: Predicate -> Int
prSize (Or p1 p2) = prSize p1 + prSize p2 + 1
prSize (And p1 p2) = prSize p1 + prSize p2 + 1
prSize (Not p) = prSize p + 1
prSize _ = 1
