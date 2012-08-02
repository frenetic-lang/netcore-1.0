--------------------------------------------------------------------------------
-- The Frenetic Project                                                       --
-- frenetic@frenetic-lang.org                                                 --
--------------------------------------------------------------------------------
-- Licensed to the Frenetic Project by one or more contributors. See the      --
-- NOTICE file distributed with this work for additional information          --
-- regarding copyright and ownership. The Frenetic Project licenses this      --
-- file to you under the following license.                                   --
--                                                                            --
-- Redistribution and use in source and binary forms, with or without         --
-- modification, are permitted provided the following conditions are met:     --
-- - Redistributions of source code must retain the above copyright           --
--   notice, this list of conditions and the following disclaimer.            --
-- - Redistributions of binaries must reproduce the above copyright           --
--   notice, this list of conditions and the following disclaimer in          --
--   the documentation or other materials provided with the distribution.     --
-- - The names of the copyright holds and contributors may not be used to     --
--   endorse or promote products derived from this work without specific      --
--   prior written permission.                                                --
--                                                                            --
-- Unless required by applicable law or agreed to in writing, software        --
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  --
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   --
-- LICENSE file distributed with this work for specific language governing    --
-- permissions and limitations under the License.                             --
--------------------------------------------------------------------------------
-- /src/Frenetic/NetCore/API.hs                                               --
--------------------------------------------------------------------------------

module Frenetic.NetCore.API
  ( -- * Basic types
    Switch
  , Port
  , Vlan
  , Loc (..)
  , PseudoPort (..)
  , Word48
  -- * Actions
  , Action (..)
  , Rewrite
  , Forward
  , Query (..)
  , isPktQuery
  -- ** Basic actions
  , query
  , pktQuery
  -- ** Action composition
  , unionAction
  -- ** Inspecting actions
  , actionForwardsTo
  -- * Patterns
  , Pattern (..)
  -- * Predicates
  , Predicate (..)
  -- * Packets
  , Packet (..)
  -- * Policies
  , Policy (..)
  -- * Tools
  , idOfQuery
  , interesting
  , interestingFields
  , prUnIntersect
  , prUnUnion
  , poUnUnion
  , poDom
  ) where

import Data.Bits
import Data.IORef
import qualified Data.List as List
import qualified Data.MultiSet as MS
import qualified Data.Set as Set
import Data.Word
import Frenetic.LargeWord
import Frenetic.Pattern
import Frenetic.Util
import System.IO.Unsafe

{-| The type of switches in the network. -}
type Switch = Word64

{-| The type of switch ports. -}
type Port = Word16

{-| The type of vlan tags -}
type Vlan = Word16

-- |Fully qualified port locations
data Loc = Loc Switch Port deriving (Eq, Ord, Show)

{-| The type of logical switch ports. -}
data PseudoPort = Physical Port | PhysicalFlood deriving (Ord, Eq, Show)

{-| Auxillary value for ethernet addresses.  -}
type Word48 = LargeKey Word8 (LargeKey Word8 (LargeKey Word8 (LargeKey Word8 (LargeKey Word8 Word8))))

-- |Frenetic packets
data Packet = Packet {
  pktDlSrc :: Word48,
  pktDlDst :: Word48,
  pktDlTyp :: Word16,
  pktDlVlan :: Vlan,
  pktDlVlanPcp :: Word8,
  pktNwSrc :: Maybe Word32,
  pktNwDst :: Maybe Word32,
  pktNwProto :: Word8,
  pktNwTos :: Word8,
  pktTpSrc :: Maybe Word16,
  pktTpDst :: Maybe Word16,
  pktInPort :: Port
} deriving (Show, Eq, Ord)

-- |Frenetic patterns
data Pattern = Pattern {
  ptrnDlSrc :: Wildcard Word48
  , ptrnDlDst :: Wildcard Word48
  , ptrnDlTyp :: Wildcard Word16
  , ptrnDlVlan :: Wildcard Vlan
  , ptrnDlVlanPcp :: Wildcard Word8
  , ptrnNwSrc :: Prefix Word32
  , ptrnNwDst :: Prefix Word32
  , ptrnNwProto :: Wildcard Word8
  , ptrnNwTos :: Wildcard Word8
  , ptrnTpSrc :: Wildcard Word16
  , ptrnTpDst :: Wildcard Word16
  , ptrnInPort :: Wildcard Port
  } deriving (Ord, Eq)

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

-- |Build a list of the non-wildcarded pattern fields
interestingFields :: Pattern -> Set String
interestingFields (Pattern {..}) = Set.fromList stringList where
  stringList = filter (\l -> l /= "") $ lines 
  lines = [ case ptrnDlSrc     of {Exact v -> "DlSrc";      Wildcard -> ""}
          , case ptrnDlDst     of {Exact v -> "DlDst";      Wildcard -> ""}
          , case ptrnDlTyp     of {Exact v -> "DlTyp";      Wildcard -> ""}
          , case ptrnDlVlan    of {Exact v -> "DlVlan";     Wildcard -> ""}
          , case ptrnDlVlanPcp of {Exact v -> "DlVlanPcp";  Wildcard -> ""}
          , case ptrnNwSrc     of {Prefix _ 0 -> "";        p -> "NwSrc"}
          , case ptrnNwDst     of {Prefix _ 0 -> "";        p -> "NwDst"}
          , case ptrnNwProto   of {Exact v -> "NwProto";    Wildcard -> ""}
          , case ptrnNwTos     of {Exact v -> "NwTos";      Wildcard -> ""}
          , case ptrnTpSrc     of {Exact v -> "TpSrc";      Wildcard -> ""}
          , case ptrnTpDst     of {Exact v -> "TpDst";      Wildcard -> ""}
          , case ptrnInPort    of {Exact v -> "InPort";     Wildcard -> ""}
          ]

-- |Build a list of the non-wildcarded patterns with sep between field and value
interesting :: String -> Pattern -> [String]
interesting sep (Pattern {..}) = filter (\l -> l /= "") $ lines where
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

type Rewrite = Pattern

-- | Forward represents a set of forwards with modifications on a switch,
-- including forwarding multiple packets, so long as they are *different*
-- packets (i.e., have a different modification applied to each).  Flood is
-- encoded by using the PseudoPort Flood rather than
type Forward = MS.MultiSet (PseudoPort, Rewrite)

type QueryID = Int

nextQueryID :: IORef QueryID
nextQueryID = unsafePerformIO $ newIORef 0

data Query
  = NumPktQuery QueryID (Chan (Switch, Integer)) Int
  | PktQuery { pktQueryChan :: (Chan (Switch, Packet)), pktQueryID :: QueryID }
  deriving (Eq)

data Action = Action {
  actionForwards :: Forward,
  actionQueries :: [Query]
} deriving (Eq, Ord)

isPktQuery (PktQuery _ _) = True
isPktQuery _               = False

actionForwardsTo :: Action -> MS.MultiSet PseudoPort
actionForwardsTo (Action m _) = 
  MS.map fst m

unionForward :: Forward -> Forward -> Forward
unionForward m1 m2 =
  MS.union m1 m2

unionAction :: Action -> Action -> Action
unionAction (Action fwd1 q1) (Action fwd2 q2) =
  Action (unionForward fwd1 fwd2) (unionQuery q1 q2)
    where unionQuery xs ys = xs ++ filter (\y -> not (y `elem` xs)) ys

query :: Int -> IO (Chan (Switch, Integer), Query)
query millisecondInterval = do
  ch <- newChan
  queryID <- readIORef nextQueryID
  modifyIORef nextQueryID (+ 1)
  return (ch, NumPktQuery queryID ch millisecondInterval)

pktQuery :: IO (Chan (Switch, Packet), Query)
pktQuery = do
  ch <- newChan
  queryID <- readIORef nextQueryID
  modifyIORef nextQueryID (+1)
  return (ch, PktQuery ch queryID)

idOfQuery :: Query -> QueryID
idOfQuery (NumPktQuery queryID _ _) = queryID
idOfQuery (PktQuery {pktQueryID=queryID}) = queryID

{-| Predicates denote sets of (switch, packet) pairs. -}
data Predicate = PrPattern Pattern
               | PrTo Switch
               | PrUnion Predicate Predicate
               | PrIntersect Predicate Predicate
               | PrNegate Predicate
  deriving (Eq, Ord)

-- |Get back all predicates in the intersection.  Does not return any naked intersections.
prUnIntersect :: Predicate -> [Predicate]
prUnIntersect po = List.unfoldr f [po] where
  f predicates = case predicates of 
    [] -> Nothing
    (PrIntersect p1 p2) : rest -> f (p1 : (p2 : rest))
    p : rest -> Just (p, rest)

-- |Get back all predicates in the union.  Does not return any naked unions.
prUnUnion :: Predicate -> [Predicate]
prUnUnion po = List.unfoldr f [po] where
  f predicates = case predicates of 
    [] -> Nothing
    (PrUnion p1 p2) : rest -> f (p1 : (p2 : rest))
    p : rest -> Just (p, rest)

{-| Policies denote functions from (switch, packet) to packets. -}
data Policy = PoBottom
            | PoBasic Predicate Action
            | PoUnion Policy Policy
            deriving (Eq, Ord)

instance Show Predicate where
  show (PrPattern pat) = show pat
  show (PrTo s) = "switch(" ++ show s ++ ")"
  show (PrUnion pr1 pr2) = "(" ++ show pr1 ++ ") \\/ (" ++ show pr2 ++ ")"
  show (PrIntersect pr1 pr2) = "(" ++ show pr1 ++ ") /\\ (" ++ show pr2 ++ ")"
  show (PrNegate pr) = "~(" ++ show pr ++ ")"

instance Matchable Predicate where
  top = PrPattern top
  intersect p1 p2 = Just (PrIntersect p1 p2)

instance Ord Query where
  compare q1 q2 = compare qid1 qid2 where
    qid1 = idOfQuery q1
    qid2 = idOfQuery q2

instance Show Action where
  show (Action fwd q) = "<fwd=" ++ show (MS.toAscList fwd) ++ " q=" ++ show q ++ ">"

instance Show Query where
  show (NumPktQuery qid _ _) = "NumPkt " ++ show qid
  show (PktQuery _ qid) = "Pkt " ++ show qid

instance Show Policy where
  show PoBottom = "(PoBottom)"
  show (PoBasic pr as) = "(" ++ show pr ++ ") -> " ++ show as
  show (PoUnion po1 po2) = "(" ++ show po1 ++ ") \\/ (" ++ show po2 ++ ")"

-- |Get back all basic policies in the union.  Does not return any unions.
poUnUnion :: Policy -> [Policy]
poUnUnion po = List.unfoldr f [po] where
  f policies = case policies of 
    [] -> Nothing
    (PoUnion p1 p2) : rest -> f (p1 : (p2 : rest))
    p : rest -> Just (p, rest)

-- |Returns a predicate that matches the domain of the policy.
poDom :: Policy -> Predicate
poDom PoBottom = PrNegate top
poDom (PoBasic pred _) = pred
poDom (PoUnion pol1 pol2) = PrUnion (poDom pol1) (poDom pol2)
