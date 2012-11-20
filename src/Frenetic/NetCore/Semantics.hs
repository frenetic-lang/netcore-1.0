-- |Composes NetCore policies and predicates, and defines how these policies
-- interpret abstract packets.
module Frenetic.NetCore.Semantics
  ( evalProgram
  , Id (..)
  , Act (..)
  , Pol (..)
  , In (..)
  , Out (..)
  , Callback (..)
  , Callbacks
  , callbackDelayStream
  , callbackInterval
  , evalPol
  , evalAct
  , desugarPolicy
  , synthRestrictPol
  , isForward
  , isGetPacket
  , isQuery
  , matchPkt
  , actOnMatch
  , preimgOfAct
  , seqAct
  )  where

import Prelude hiding (pred)
import Nettle.OpenFlow.Match
import Nettle.OpenFlow.Packet (BufferID)
import Nettle.IPv4.IPAddress
import Frenetic.NetCore.Reduce (isEmptyPredicate)
import Frenetic.Pattern
import Frenetic.Common
import Frenetic.Topo (Switch,Port,Loc)
import Frenetic.NetCore.Types
import Frenetic.NetCore.Short
import qualified Data.ByteString.Lazy as BS
import Control.Monad.State
import qualified Data.Map as M

import Data.Generics

{-# LANGUAGE DeriveDataTypeable #-}

type Id = Int

data Prog
  = ProgPol Pol
  | ProgLetQueue Switch Port Word16 (Queue -> Prog)
  | ProgUnion Prog Prog

data Pol
  = PolEmpty
  | PolProcessIn Predicate [Act]
  | PolUnion Pol Pol
  | PolRestrict Pol Predicate
  | PolGenPacket Id
  | PolSeq Pol Pol
  deriving (Show, Eq, Data, Typeable)

data Act
  = ActFwd PseudoPort Modification
  | ActQueryPktCounter Id
  | ActQueryByteCounter Id
  | ActGetPkt Id
  | ActMonSwitch Id
  deriving (Show, Eq, Data, Typeable)

data In
  = InPkt Loc Packet (Maybe BufferID)
  | InGenPkt Id Switch PseudoPort Packet ByteString
  | InCounters Id Switch Predicate Integer {- packets -} Integer {- bytes -}
  | InSwitchEvt SwitchEvent
  deriving (Show, Eq, Data, Typeable)

data Out
  = OutPkt Switch PseudoPort Packet (Either BufferID ByteString)
  | OutIncrPktCounter Id
  | OutIncrByteCounter Id Integer
  | OutUpdPktCounter Id Switch Predicate Integer
  | OutUpdByteCounter Id Switch Predicate Integer
  | OutGetPkt Id Loc Packet
  | OutNothing
  | OutSwitchEvt Id SwitchEvent
  deriving (Show, Eq, Data, Typeable)

data Callback
  = CallbackByteCounter Int ((Switch, Integer) -> IO ())
  | CallbackPktCounter Int ((Switch, Integer) -> IO ())
  | CallbackGetPkt ((Loc, Packet) -> IO ())
  | CallbackMonSwitch (SwitchEvent -> IO ())

instance Show Callback where
  show (CallbackByteCounter n _) = "CallbackByteCounter " ++ show n
  show (CallbackPktCounter n _) = "CallbackPktCounter " ++ show n
  show (CallbackGetPkt _) = "CallbackGetPkt"
  show (CallbackMonSwitch _) = "CallbackMonSwitch"

type Callbacks = Map Id Callback

seqAct :: Act -> Act -> Maybe Act
seqAct (ActFwd p mods1) (ActFwd InPort mods2) = Just (ActFwd p mods3)
  where mods3 = Modification
                  (modifyDlSrc mods2 `ifLeft` modifyDlSrc mods1)
                  (modifyDlDst mods2 `ifLeft` modifyDlDst mods1)
                  (modifyDlVlan mods2 `ifLeft` modifyDlVlan mods1)
                  (modifyDlVlanPcp mods2 `ifLeft` modifyDlVlanPcp mods1)
                  (modifyNwSrc mods2 `ifLeft` modifyNwSrc mods1)
                  (modifyNwDst mods2 `ifLeft` modifyNwDst mods1)
                  (modifyNwTos mods2 `ifLeft` modifyNwTos mods1)
                  (modifyTpSrc mods2 `ifLeft` modifyTpSrc mods1)
                  (modifyTpDst mods2 `ifLeft` modifyTpDst mods1)

seqAct (ActFwd _ mods1) (ActFwd p mods2) = Just (ActFwd p mods3)
  where mods3 = Modification
                  (modifyDlSrc mods2 `ifLeft` modifyDlSrc mods1)
                  (modifyDlDst mods2 `ifLeft` modifyDlDst mods1)
                  (modifyDlVlan mods2 `ifLeft` modifyDlVlan mods1)
                  (modifyDlVlanPcp mods2 `ifLeft` modifyDlVlanPcp mods1)
                  (modifyNwSrc mods2 `ifLeft` modifyNwSrc mods1)
                  (modifyNwDst mods2 `ifLeft` modifyNwDst mods1)
                  (modifyNwTos mods2 `ifLeft` modifyNwTos mods1)
                  (modifyTpSrc mods2 `ifLeft` modifyTpSrc mods1)
                  (modifyTpDst mods2 `ifLeft` modifyTpDst mods1)
seqAct _ _ = Nothing


outToIn :: Out -> Maybe In
-- TODO(arjun): pol1;pol2 doesn't work if pol1 emits to a pseudoport?
outToIn (OutPkt sw (Physical pt) pk (Left buf)) = 
  Just (InPkt (Loc sw pt) pk (Just buf))
-- TODO(arjun): perhaps Out should carry the channel ID
-- outToIn (OutPkt sw pt pk (Right bytes)) = Just (InGenPkt ??? sw pt pk bytes)
outToIn _ = Nothing

isGetPacket (ActGetPkt{}) = True
isGetPacket _ = False

isForward :: Act -> Bool
isForward (ActFwd _ _) = True
isForward _ = False

isQuery :: Act -> Bool
isQuery act = case act of
  ActQueryPktCounter {} -> True
  ActQueryByteCounter {} -> True
  ActGetPkt {} -> True
  ActFwd {} -> False
  ActMonSwitch {} -> True

-- |Restricts the policy's domain to 'pred'. Does not eliminate
-- 'Restrict' expressions, but does restrict their restrictions.
synthRestrictPol :: Pol -> Predicate -> Pol
synthRestrictPol pol pred = case pol of
  PolEmpty -> PolEmpty
  PolProcessIn pred' acts -> 
    PolProcessIn (And pred' pred) acts
  PolUnion pol1 pol2 ->
    PolUnion (synthRestrictPol pol1 pred) (synthRestrictPol pol2 pred)
  PolSeq pol1 pol2 ->
    PolSeq (synthRestrictPol pol1 pred) pol2 -- No need to restrict pol2!
  PolRestrict (PolGenPacket chan) pred' -> 
    PolRestrict (PolGenPacket chan) (And pred pred')
  PolRestrict pol pred' -> 
    PolRestrict pol (And pred pred')
  PolGenPacket chan -> 
    PolRestrict (PolGenPacket chan) pred

inPacket :: In -> Maybe Packet
inPacket (InPkt _ pkt _) = Just pkt
inPacket (InGenPkt _ _ _ pkt _) = Just pkt
inPacket (InCounters _ _ _ _ _) = Nothing
inPacket (InSwitchEvt _) = Nothing

evalPol = pol
evalAct = action

pol :: Pol -> In -> [Out]
pol PolEmpty _ = []
pol (PolUnion p1 p2) inp = pol p1 inp ++ pol p2 inp
pol (PolProcessIn pr acts) inp = 
  if pred pr inp then map (\a -> action a inp) acts else []
pol (PolRestrict p pr) inp = 
  if pred pr inp then pol p inp else []
pol (PolGenPacket x) inp = case inp of
  InGenPkt y sw pt pkt raw -> 
    if x == y then [OutPkt sw pt pkt (Right raw)] else []
  otherwise -> []
pol (PolSeq p1 p2) inp = concatMap (pol p2) (mapMaybe outToIn (pol p1 inp))

action :: Act -> In -> Out
action (ActFwd pt mods) (InPkt (Loc sw _) hdrs maybePkt) = case maybePkt of
  Just pkt -> OutPkt sw pt hdrs (Left pkt)
  Nothing -> OutNothing
action (ActFwd _ _) (InGenPkt _ _ _ _ _) = OutNothing
action (ActFwd _ _) (InCounters _ _ _ _ _) = OutNothing
action (ActFwd _ _) (InSwitchEvt _) = OutNothing
action (ActQueryPktCounter x) (InCounters y sw pred numPkts _) =
  if x == y then
    OutUpdPktCounter x sw pred numPkts
  else
    OutNothing
action (ActQueryPktCounter _) (InPkt _ _ _) = OutNothing
action (ActQueryPktCounter x) (InGenPkt _ _ _ _ _) = OutIncrPktCounter x
action (ActQueryPktCounter _) (InSwitchEvt _) = OutNothing
action (ActQueryByteCounter x) (InCounters y sw pred _ numBytes) =
  if x == y then
    OutUpdByteCounter x sw pred numBytes
  else
    OutNothing
action (ActQueryByteCounter _) (InPkt _ _ _) = OutNothing
action (ActQueryByteCounter x) (InGenPkt _ _ _ _ pkt) =
  OutIncrByteCounter x (fromIntegral (BS.length pkt))
action (ActQueryByteCounter _) (InSwitchEvt _) = OutNothing
action (ActGetPkt x) (InPkt loc hdrs _) = OutGetPkt x loc hdrs
action (ActGetPkt _) (InGenPkt _ _ _ _ _) = OutNothing
action (ActGetPkt _) (InCounters _ _ _ _ _) = OutNothing
action (ActGetPkt _) (InSwitchEvt _) = OutNothing
action (ActMonSwitch x) (InSwitchEvt evt) = OutSwitchEvt x evt
action (ActMonSwitch _) (InPkt _ _ _) = OutNothing
action (ActMonSwitch _) (InGenPkt _ _ _ _ _) = OutNothing
action (ActMonSwitch _) (InCounters _ _ _ _ _) = OutNothing

ifLeft :: Maybe a -> Maybe a -> Maybe a
ifLeft (Just x) _ = Just x
ifLeft Nothing y = y

eqIfJust :: Eq a => Maybe a -> Maybe a -> Maybe (Maybe a)
eqIfJust (Just x) (Just y) = if x == y then Just Nothing else Nothing
eqIfJust (Just x) Nothing = Just Nothing
eqIfJust Nothing rhs = Just rhs

eqIfJustIP :: Maybe IPAddress 
           -> IPAddressPrefix 
           -> Maybe IPAddressPrefix
eqIfJustIP (Just x) y = 
  if x `elemOfPrefix` y then Just (IPAddressPrefix (IPAddress 0) 0) else Nothing
eqIfJustIP Nothing rhs = Just rhs

preimgOfAct :: Act -> Match -> Maybe Match
preimgOfAct (ActFwd (Physical pt) (Modification{..})) (Match{..}) = do
  inPort <- eqIfJust (Just pt) inPort
  srcEthAddress <- eqIfJust modifyDlSrc srcEthAddress
  dstEthAddress <- eqIfJust modifyDlDst dstEthAddress
  let unvlan Nothing = 0xffff
      unvlan (Just v) = v
  vLANID <- eqIfJust (fmap unvlan modifyDlVlan) vLANID
  vLANPriority <- eqIfJust modifyDlVlanPcp vLANPriority
  ipTypeOfService <- eqIfJust modifyNwTos ipTypeOfService
  srcIPAddress <- eqIfJustIP modifyNwSrc srcIPAddress
  dstIPAddress <- eqIfJustIP modifyNwDst dstIPAddress
  srcTransportPort <- eqIfJust modifyTpSrc srcTransportPort
  dstTransportPort <- eqIfJust modifyTpDst dstTransportPort
  return $ Match inPort srcEthAddress dstEthAddress vLANID vLANPriority
                 ethFrameType ipTypeOfService matchIPProtocol srcIPAddress 
                 dstIPAddress srcTransportPort dstTransportPort
preimgOfAct _ _ = Nothing

actOnMatch :: Act -> Match -> Maybe Match
actOnMatch (ActFwd (Physical pt) (Modification{..})) (Match{..}) =
  Just $ Match (Just pt)
           (modifyDlSrc `ifLeft` srcEthAddress)
           (modifyDlDst `ifLeft` dstEthAddress)
           (case modifyDlVlan of
              Just Nothing -> Just 0xffff
              Just (Just vlan) -> Just vlan
              Nothing -> vLANID)
           (modifyDlVlanPcp `ifLeft` vLANPriority)
           ethFrameType
           (modifyNwTos `ifLeft` ipTypeOfService)
           matchIPProtocol
           (case modifyNwSrc of
              Just ip -> IPAddressPrefix ip 32
              Nothing -> srcIPAddress)
           (case modifyNwDst of
              Just ip -> IPAddressPrefix ip 32
              Nothing -> dstIPAddress)
           (modifyTpSrc `ifLeft` srcTransportPort)
           (modifyTpDst `ifLeft` dstTransportPort)
actOnMatch (ActFwd AllPorts _) _ = Nothing
actOnMatch (ActFwd InPort _) _ = Nothing -- TODO(mjr): Not sure what this func is...
actOnMatch (ActFwd (ToQueue _) _) _ = Nothing -- TODO(arjun): easy IMO
actOnMatch (ActQueryPktCounter _) _ = Nothing
actOnMatch (ActQueryByteCounter _) _ = Nothing
actOnMatch (ActGetPkt _) _ = Nothing
actOnMatch (ActMonSwitch _) _ = Nothing

-- |When a packet-specific predicate is applied to a non-packet input, we
-- produce this default value. 
nonPktDefault = False

counterMatches :: In -> Predicate -> Bool
counterMatches (InCounters _ _ pred _ _) pred' = 
  isEmptyPredicate (And pred (Not pred'))
counterMatches _ _ = False

-- |'pktHeaderIs inPkt sel v' tests if the input packet's header is 'v'. If
-- 'inPkt' is not a packet, but some other type of input, 'pktHeaderIs' returns
-- 'False'.
pktHeaderIs :: Eq a => In -> (Packet -> a) -> a -> Bool
pktHeaderIs inp sel v = case inPacket inp of
  Just pkt -> sel pkt == v
  Nothing -> nonPktDefault

-- |Use 'tpPktHeaderIs' it match OpenFlow headers that only meaningful for
-- certain types of packets (e.g., TCP ports and IP protocol.)
tpPktHeaderIs :: Eq a => In -> (Packet -> Maybe a) -> a -> Bool
tpPktHeaderIs inp sel v = case inPacket inp of
  Just pkt -> case sel pkt of
    Nothing -> False
    Just v' -> v' == v
  Nothing -> nonPktDefault

pred :: Predicate -> In -> Bool
pred Any _ = True
pred None _ = False
pred (Or pr1 pr2) inp = pred pr1 inp || pred pr2 inp
pred (And pr1 pr2) inp = pred pr1 inp && pred pr2 inp
pred (Not pr) inp = not (pred pr inp)
pred (DlSrc eth) inp = 
  pktHeaderIs inp pktDlSrc eth || counterMatches inp (DlSrc eth)
pred (DlDst eth) inp =
  pktHeaderIs inp pktDlDst eth || counterMatches inp (DlDst eth)
pred (DlTyp typ) inp = 
  pktHeaderIs inp pktDlTyp typ || counterMatches inp (DlTyp typ)
pred (DlVlan vlan) inp = 
  pktHeaderIs inp pktDlVlan vlan || counterMatches inp (DlVlan vlan)
pred (DlVlanPcp pcp) inp =
  pktHeaderIs inp pktDlVlanPcp pcp || counterMatches inp (DlVlanPcp pcp)
pred (NwSrc (IPAddressPrefix prefix len)) inp = case inPacket inp of
  Just Packet{..} -> case pktNwSrc of
    Nothing -> len == 0
    Just ip -> ip `elemOfPrefix` (IPAddressPrefix prefix len)
  Nothing -> nonPktDefault
pred (NwDst (IPAddressPrefix prefix len)) inp = case inPacket inp of
  Just Packet{..} -> case pktNwDst of
    Nothing -> len == 0
    Just ip -> ip `elemOfPrefix` (IPAddressPrefix prefix len)
  Nothing -> nonPktDefault
pred (NwProto proto) inp =
  pktHeaderIs inp pktNwProto proto || counterMatches inp (NwProto proto)
pred (TpSrcPort pt) inp =
  tpPktHeaderIs inp pktTpSrc pt || counterMatches inp (TpSrcPort pt)
pred (TpDstPort pt) inp =
  tpPktHeaderIs inp pktTpDst pt || counterMatches inp (TpDstPort pt)
pred (NwTos tos) inp =
  pktHeaderIs inp pktNwTos tos || counterMatches inp (NwTos tos)
pred (Switch sw) inp = case inp of
  InPkt (Loc sw' _) _ _ -> sw' == sw
  InGenPkt _ sw' _ _ _ -> sw' == sw
  InSwitchEvt (SwitchConnected sw' _) -> sw == sw'
  InSwitchEvt (SwitchDisconnected sw') -> sw == sw'
  InSwitchEvt (PortEvent sw' _ _ _) -> sw == sw'
  InCounters _ _ pred _ _ -> isEmptyPredicate (And pred (Not (Switch sw)))
pred (IngressPort pt) inp = case inp of
  InPkt (Loc _ pt') _ _ -> pt == pt'
  InGenPkt _ _ pt' _ _ -> case pt' of
    Physical p -> p == pt
    otherwise -> False
  InSwitchEvt (PortEvent _ pt' _ _) -> pt == pt'
  InSwitchEvt (SwitchConnected _ _) -> nonPktDefault
  InSwitchEvt (SwitchDisconnected _) -> nonPktDefault
  InCounters _ _ pred _ _ -> isEmptyPredicate (And pred (Not (IngressPort pt)))


callbackInterval :: Callback -> Maybe Int
callbackInterval (CallbackByteCounter n _) = Just n
callbackInterval (CallbackPktCounter n _) = Just n
callbackInterval (CallbackGetPkt _) = Nothing
callbackInterval (CallbackMonSwitch _) = Nothing

insDelayStream :: Int
               -> a
               -> [Either a Int]
               -> [Either a Int]
insDelayStream delay evt [] = [Right delay, Left evt] 
insDelayStream delay evt (Right delay' : rest) =
  let delay'' = delay - delay'
    in if delay'' > 0 then
         (Right delay') : (insDelayStream delay'' evt rest)
       else if delay'' == 0 then
         (Right delay) : (Left evt) : rest
       else {- delay'' < 0 -}
         (Right delay) : (Left evt) : (Right (delay' - delay)) : rest
insDelayStream delay evt (Left evt' : rest) = 
  (Left evt') : (insDelayStream delay evt rest)


-- |Returns an infinite stream of callbacks and delays. The controller should
-- invoke callbacks with the current counter or pause for the delay.
--
-- The delay is in millseconds. (Note that 'threadDelay' takes a
-- nanosecond argument.)
callbackDelayStream :: Callbacks -> [Either (Id, Callback) Int]
callbackDelayStream callbacks = loop initial
  where loop :: [Either (Int, Id, Callback) Int] -> [Either (Id, Callback) Int]
        loop [] = []
        loop (Right delay : future) = (Right delay) : (loop future)
        loop (Left evt@(delay, x, cb) : future) =
          let future' = insDelayStream delay evt future
            in (Left (x, cb)) : (loop future')
        select :: (Id, Callback) -> Maybe (Int, Id, Callback)
        select (x, cb) = case callbackInterval cb of
          Nothing -> Nothing
          Just delay -> Just (delay, x, cb)
        initial :: [Either (Int, Id, Callback) Int]
        initial = foldr (\evt@(delay, _,_) acc -> insDelayStream delay evt acc)
                        []
                        (mapMaybe select (M.toList callbacks))

type DSState = (Id, Map Id Callback, [(Id, Chan (Loc, ByteString))])

type DS a = State DSState a

newCallback :: Callback -> DS Id
newCallback cb = do
  (x, cbs, gens) <- get
  put (x+1, M.insert x cb cbs, gens)
  return x
  
newGenPacket :: Chan (Loc, ByteString) -> DS Id
newGenPacket chan = do
  (x, cbs, gens) <- get
  put (x+1, cbs, (x,chan):gens)
  return x

dsPolicy ::  Policy -> DS Pol
dsPolicy PoBottom = return PolEmpty
dsPolicy (PoBasic pred actions) = do
  actions' <- mapM dsAction actions
  return (PolProcessIn pred actions')
dsPolicy (PoUnion pol1 pol2) = do
  pol1' <- dsPolicy pol1
  pol2' <- dsPolicy pol2
  return (PolUnion pol1' pol2')
dsPolicy (Restrict pol pred) = do
  pol' <- dsPolicy pol
  return (PolRestrict pol' pred)
dsPolicy (SendPackets chan) = do
  x <- newGenPacket chan
  return (PolGenPacket x)
dsPolicy (Sequence pol1 pol2) = do
  pol1' <- dsPolicy pol1
  pol2' <- dsPolicy pol2  
  return (PolSeq pol1' pol2')
  

dsAction :: Action -> DS Act
dsAction (Forward pt mods) =
  return (ActFwd pt mods)
dsAction (CountPackets _ interval cb) = do
  x <- newCallback (CallbackPktCounter interval cb)
  return (ActQueryPktCounter x)
dsAction (CountBytes _ interval cb) = do
  x <- newCallback (CallbackByteCounter interval cb)
  return (ActQueryByteCounter x)
dsAction (GetPacket _ cb) = do
  x <- newCallback (CallbackGetPkt cb)
  return (ActGetPkt x)
dsAction (MonitorSwitch cb) = do
  x <- newCallback (CallbackMonSwitch cb)
  return (ActMonSwitch x)

desugarPolicy :: Policy 
              -> (Callbacks, 
                  [(Id, Chan (Loc, ByteString))],
                  Pol)
desugarPolicy pol = (cbs, gens, pol')
  where (pol', (_, cbs, gens)) = runState (dsPolicy pol) (0, M.empty, [])

evalProgram :: Program
            -> (Map Switch [Queue], Policy)
evalProgram prog = (M.fromListWith (++) queues, pol)
  where (qMap, pol) = eval M.empty prog 
        queues = concatMap (\((sw, _), (_, qs)) -> zip [sw ..] (map (:[]) qs))
                           -- I assume nobody is reading this code.
                           (M.toList qMap)
        eval qMap prog = case prog of
          Policy pol -> (qMap, pol)
          ProgramUnion prog1 prog2 ->
            let (qMap', pol1) = eval qMap prog1
                (qMap'', pol2) = eval qMap' prog2
              in (qMap'', pol1 `mappend` pol2)
          WithQueue sw pt rate fn -> case M.lookup (sw,pt) qMap of
            Nothing -> 
              let q = Queue sw pt 1 rate
                in eval (M.insert (sw,pt) (1, [q]) qMap) (fn q)
            Just (n, qs) ->
              let q = Queue sw pt (n+1) rate
                in eval (M.insert (sw,pt) (n+1, q : qs) qMap) (fn q)

matchHdr :: Eq a => a -> Maybe a -> Bool
matchHdr _ Nothing = True
matchHdr v (Just v') = v == v'

matchNwHdr :: Eq a => Maybe a -> Maybe a -> Bool
matchNwHdr _ Nothing = True
matchNwHdr Nothing (Just _) = False
matchNwHdr (Just v) (Just v') = v == v'

matchPkt :: Match -> Port -> Packet -> Bool
matchPkt (Match {..}) pktInPort (Packet{..}) =
  pktInPort `matchHdr` inPort &&
  pktDlSrc `matchHdr` srcEthAddress &&
  pktDlDst `matchHdr` dstEthAddress &&
  pktDlVlan' `matchHdr` vLANID &&
  pktDlVlanPcp `matchHdr` vLANPriority &&
  pktDlTyp `matchHdr` ethFrameType &&
  pktNwProto `matchHdr` matchIPProtocol &&
  pktNwTos `matchHdr` ipTypeOfService &&
  pktNwSrc `matchIP` srcIPAddress &&
  pktNwDst `matchIP` dstIPAddress &&
  pktTpSrc `matchNwHdr` srcTransportPort &&
  pktTpDst `matchNwHdr` dstTransportPort
    where pktDlVlan' = case pktDlVlan of
            Just v -> v
            Nothing -> 0xffff
          matchIP Nothing (IPAddressPrefix _ 0) = True
          matchIP Nothing (IPAddressPrefix _ _) = False
          matchIP (Just ip) prefix = elemOfPrefix ip prefix
