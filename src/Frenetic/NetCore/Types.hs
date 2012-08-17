module Frenetic.NetCore.Types
  ( -- * Basic types
    Switch
  , Port
  , Vlan
  , Loc (..)
  , PseudoPort (..)
  -- * Actions
  , Action (..)
  , Query (..)
  , Counter (..)
  , Modification (..)
  , unmodified
  , isPktQuery
  -- ** Basic actions
  , countPkts
  , countBytes
  , getPkts
  -- ** Inspecting actions
  , actionForwardsTo
  -- * Patterns
  , Pattern (..)
  , matchedFields
  -- * Predicates
  , Predicate (..)
  , exactMatch
  -- * Packets
  , Packet (..)
  -- * Policies
  , Policy (..)
  -- * Tools
  , interesting
  , modifiedFields
  , prUnIntersect
  , prUnUnion
  , poUnUnion
  , poDom
  , module Frenetic.EthernetAddress
  , module Frenetic.IPAddress
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
import Frenetic.EthernetAddress
import Frenetic.IPAddress

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
  pktNwSrc :: Maybe Word32, -- ^source IP address for IP packets
  pktNwDst :: Maybe Word32, -- ^destination IP address for IP packets
  pktNwProto :: Word8, -- ^IP protocol number (e.g., 6 for TCP segments)
  pktNwTos :: Word8, -- ^IP TOS field
  pktTpSrc :: Maybe Word16, -- ^source port for IP packets
  pktTpDst :: Maybe Word16, -- ^destination port for IP packets
  pktInPort :: Port -- ^ingress port on the switch where the packet was
                    -- received
} deriving (Show, Eq, Ord)

-- |Patterns to match packets. Patterns translate directly to a single OpenFlow
-- match rule.
data Pattern = Pattern {
    ptrnDlSrc :: Wildcard EthernetAddress
  , ptrnDlDst :: Wildcard EthernetAddress
  , ptrnDlTyp :: Wildcard Word16
  , ptrnDlVlan :: Wildcard (Maybe Vlan)
  , ptrnDlVlanPcp :: Wildcard Word8
  , ptrnNwSrc :: Prefix Word32
  , ptrnNwDst :: Prefix Word32
  , ptrnNwProto :: Wildcard Word8
  , ptrnNwTos :: Wildcard Word8
  , ptrnTpSrc :: Wildcard Word16
  , ptrnTpDst :: Wildcard Word16
  , ptrnInPort :: Wildcard Port
 } deriving (Ord, Eq)

matchedFields :: Pattern -> [Field]
matchedFields (Pattern{..}) = catMaybes fields where
  fields = [ case ptrnDlSrc of { Exact _ -> Just FDlSrc; Wildcard -> Nothing }
           , case ptrnDlDst of { Exact _ -> Just FDlDst; Wildcard -> Nothing }
           , case ptrnDlVlan of { Exact _ -> Just FDlVlan; Wildcard -> Nothing }
           , case ptrnDlVlanPcp of { Exact _ -> Just FDlVlanPcp;
                                     Wildcard -> Nothing }
           , case ptrnNwSrc of { Prefix _ 32 -> Just FNwSrc; 
                                 otherwise -> Nothing }
           , case ptrnNwDst of { Prefix _ 32 -> Just FNwDst; 
                                 otherwise -> Nothing }
           , case ptrnNwProto of { Exact _ -> Just FNwProto; Wildcard -> Nothing }
           , case ptrnNwTos of { Exact _ -> Just FNwTos; Wildcard -> Nothing }
           , case ptrnTpSrc of { Exact _ -> Just FTpSrc; Wildcard -> Nothing }
           , case ptrnTpDst of { Exact _ -> Just FTpDst; Wildcard -> Nothing }
           , case ptrnInPort of { Exact _ -> Just FInPort; Wildcard -> Nothing }
           ]

-- |Predicates to match packets.
data Predicate
  = DlSrc EthernetAddress -- ^Match ethernet source address
  | DlDst EthernetAddress -- ^Match ethernet destination address
  | DlTyp Word16 -- ^Match ethernet type code (e.g., 0x0800 for IP packets)
  | DlVlan (Maybe Word16) -- ^Match VLAN tag
  | DlVlanPcp Word8 -- ^Match VLAN priority
  | NwSrc (Prefix Word32) -- ^Match source IP address
  | NwDst (Prefix Word32) -- ^Match destination IP address
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
  modifyNwSrc :: Maybe Word32,
  modifyNwDst :: Maybe Word32,
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
                fmap (\v -> NwSrc (Prefix v 32)) pktNwSrc, 
                fmap (\v -> NwDst (Prefix v 32)) pktNwDst,
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


instance Matchable Pattern where
  top = Pattern {
    ptrnDlSrc = top
    , ptrnDlDst = top
    , ptrnDlTyp = top
    , ptrnDlVlan = top
    , ptrnDlVlanPcp = top
    , ptrnNwSrc = top
    , ptrnNwDst = top
    , ptrnNwProto = top
    , ptrnNwTos = top
    , ptrnTpSrc = top
    , ptrnTpDst = top
    , ptrnInPort = top
    }

  intersect p1 p2 = do ptrnDlSrc' <- intersect (ptrnDlSrc p1) (ptrnDlSrc p2)
                       ptrnDlDst' <- intersect (ptrnDlDst p1) (ptrnDlDst p2)
                       ptrnDlTyp' <- intersect (ptrnDlTyp p1) (ptrnDlTyp p2)
                       ptrnDlVlan' <- intersect (ptrnDlVlan p1) (ptrnDlVlan p2)
                       ptrnDlVlanPcp' <- intersect (ptrnDlVlanPcp p1) (ptrnDlVlanPcp p2)
                       ptrnNwSrc' <- intersect (ptrnNwSrc p1) (ptrnNwSrc p2)
                       ptrnNwDst' <- intersect (ptrnNwDst p1) (ptrnNwDst p2)
                       ptrnNwProto' <- intersect (ptrnNwProto p1) (ptrnNwProto p2)
                       ptrnNwTos' <- intersect (ptrnNwTos p1) (ptrnNwTos p2)
                       ptrnTpSrc' <- intersect (ptrnTpSrc p1) (ptrnTpSrc p2)
                       ptrnTpDst' <- intersect (ptrnTpDst p1) (ptrnTpDst p2)
                       ptrnInPort' <- intersect (ptrnInPort p1) (ptrnInPort p2)
                       return Pattern {
                         ptrnDlSrc = ptrnDlSrc'
                         , ptrnDlDst = ptrnDlDst'
                         , ptrnDlTyp = ptrnDlTyp'
                         , ptrnDlVlan = ptrnDlVlan'
                         , ptrnDlVlanPcp = ptrnDlVlanPcp'
                         , ptrnNwSrc = ptrnNwSrc'
                         , ptrnNwDst = ptrnNwDst'
                         , ptrnNwProto = ptrnNwProto'
                         , ptrnNwTos = ptrnNwTos'
                         , ptrnTpSrc = ptrnTpSrc'
                         , ptrnTpDst = ptrnTpDst'
                         , ptrnInPort = ptrnInPort'
                         }

instance Show Pattern where
  show p = "{" ++ contents ++ "}"  where
    contents = concat (List.intersperse ", " (interesting " = " p))

-- |Build a list of the non-wildcarded patterns with sep between field and value
interesting :: String -> Pattern -> [String]
interesting sep (Pattern {..}) = filter (/= "") lines where
  lines = [ case ptrnDlSrc     of {Exact v -> "DlSrc"     ++ sep ++ show v; Wildcard -> ""}
          , case ptrnDlDst     of {Exact v -> "DlDst"     ++ sep ++ show v; Wildcard -> ""}
          , case ptrnDlTyp     of {Exact v -> "DlTyp"     ++ sep ++ show v; Wildcard -> ""}
          , case ptrnDlVlan    of {Exact v -> "DlVlan"    ++ sep ++ show v; Wildcard -> ""}
          , case ptrnDlVlanPcp of {Exact v -> "DlVlanPcp" ++ sep ++ show v; Wildcard -> ""}
          , case ptrnNwSrc     of {Prefix _ 0 -> ""; p -> "NwSrc" ++ sep ++ show p}
          , case ptrnNwDst     of {Prefix _ 0 -> ""; p -> "NwDst" ++ sep ++ show p}
          , case ptrnNwProto   of {Exact v -> "NwProto"   ++ sep ++ show v; Wildcard -> ""}
          , case ptrnNwTos     of {Exact v -> "NwTos"     ++ sep ++ show v; Wildcard -> ""}
          , case ptrnTpSrc     of {Exact v -> "TpSrc"     ++ sep ++ show v; Wildcard -> ""}
          , case ptrnTpDst     of {Exact v -> "TpDst"     ++ sep ++ show v; Wildcard -> ""}
          , case ptrnInPort    of {Exact v -> "InPort"    ++ sep ++ show v; Wildcard -> ""}
          ]

type QueryID = Int

nextQueryID :: IORef QueryID
nextQueryID = unsafePerformIO $ newIORef 0

getNextQueryID :: IO QueryID
getNextQueryID = atomicModifyIORef nextQueryID (\i -> (i + 1, i))

data Counter = CountPackets | CountBytes deriving (Eq, Ord)

data Query
  = NumPktQuery {
      idOfQuery :: QueryID,
      numPktQueryChan :: Chan (Switch, Integer),
      queryInterval :: Int,
      countField :: Counter,
      totalVal :: IORef Integer,
      lastVal :: IORef Integer
    }
  | PktQuery {
      pktQueryChan :: Chan (Switch, Packet),
      idOfQuery :: QueryID
    }
  deriving (Eq)

-- |Actions to perform on packets.
data Action = Action {
  actionForwards :: MS.MultiSet (PseudoPort, Modification),
  actionQueries :: MS.MultiSet Query
} deriving (Eq, Ord, Show)

isPktQuery (PktQuery _ _) = True
isPktQuery _               = False

actionForwardsTo :: Action -> MS.MultiSet PseudoPort
actionForwardsTo (Action m _) =
  MS.map fst m

mkCountQuery :: Counter -> Int -> IO (Chan (Switch, Integer), Action)
mkCountQuery counter millisecondInterval = do
  ch <- newChan
  queryID <- getNextQueryID
  total <- newIORef 0
  last <- newIORef 0
  let q = NumPktQuery queryID ch millisecondInterval counter total last
  return (ch, Action MS.empty (MS.singleton q))


-- ^Periodically polls the network to counts the number of packets received.
--
-- Returns an 'Action' and a channel. When the 'Action' is used in the
-- active 'Policy', the controller periodically reads the packet counters
-- on the network. The controller returns the number of matching packets
-- on each switch.
countPkts :: Int -- ^polling interval, in milliseconds
          -> IO (Chan (Switch, Integer), Action)
countPkts = mkCountQuery CountPackets

-- ^Periodically polls the network to counts the number of bytes received.
--
-- Returns an 'Action' and a channel. When the 'Action' is used in the
-- active 'Policy', the controller periodically reads the packet counters
-- on the network. The controller returns the number of matching packets
-- on each switch.
countBytes :: Int -- ^polling interval, in milliseconds
           -> IO (Chan (Switch, Integer), Action)
countBytes = mkCountQuery CountBytes


-- ^Sends packets to the controller.
--
-- Returns an 'Action' and a channel. When the 'Action' is used in the active
-- 'Policy', all matching packets are sent to the controller. These packets
-- are written into the channel.
getPkts :: IO (Chan (Switch, Packet), Action)
getPkts = do
  ch <- newChan
  queryID <- getNextQueryID
  let q = PktQuery ch queryID
  return (ch, Action MS.empty (MS.singleton q))

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

{-| Policies denote functions from (switch, packet) to packets. -}
data Policy
  = PoBottom -- ^Performs no actions.
  | PoBasic Predicate Action -- ^Performs the given action on packets matching the given predicate.
  | PoUnion Policy Policy -- ^Performs the actions of both P1 and P2.
  deriving (Eq, Ord, Show)

{-
instance Show Predicate where
  show (Switch s) = "switch(" ++ show s ++ ")"
  show (Or pr1 pr2) = "(" ++ show pr1 ++ ") \\/ (" ++ show pr2 ++ ")"
  show (And pr1 pr2) = "(" ++ show pr1 ++ ") /\\ (" ++ show pr2 ++ ")"
  show (Not pr) = "~(" ++ show pr ++ ")"
-}

instance Ord Query where
  compare q1 q2 = compare qid1 qid2 where
    qid1 = idOfQuery q1
    qid2 = idOfQuery q2

{-
instance Show Action where
  show (Action fwd q) = "<fwd=" ++ show (MS.toAscList fwd) ++ " q=" ++ show q ++ ">"
-}

instance Show Query where
  show (NumPktQuery{..}) =
    "countPkts(interval=" ++ show queryInterval ++ "ms, id=" ++
    show idOfQuery ++ ")"
  show (PktQuery{..}) = "getPkts(id=" ++ show idOfQuery ++  ")"

{-
instance Show Policy where
  show PoBottom = "(PoBottom)"
  show (PoBasic pr as) = "(" ++ show pr ++ ") -> " ++ show as
  show (PoUnion po1 po2) = "(" ++ show po1 ++ ") \\/ (" ++ show po2 ++ ")"
-}

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
