{-# LANGUAGE
    TemplateHaskell,
    FlexibleContexts
 #-}

module Tests.Frenetic.NetCore.TestCompiler where

import Nettle.IPv4.IPAddress
import Nettle.OpenFlow.Packet
import Frenetic.NetCore.Semantics
import qualified Data.MultiSet as MS
import qualified Data.Set as Set
import Data.Word
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Property (Property, morallyDubiousIOProperty)
import Test.QuickCheck.Text
import Test.HUnit
import Test.Framework.Providers.HUnit
import Frenetic.NetCore.Types
import Frenetic.Compat
import Tests.Frenetic.ArbitraryCompat
import Frenetic.Pattern
import Frenetic.NetCore
import Tests.Frenetic.NetCore.ArbitraryTypes
import System.IO
import Frenetic.NetCore.Compiler
import Frenetic.Switches.OpenFlow
import Tests.Frenetic.Switches.ArbitraryOpenFlow

import qualified Nettle.OpenFlow.Action as OFAction
import qualified Nettle.OpenFlow.Match as OFMatch

import System.IO

compilerTests = $(testGroupGenerator)

case_test_query_1 = do
  (_, act) <- countPkts 1000
  let policy = PoBasic (Switch 0) act
  let tbl = compile 0 policy
  case tbl of
    [(_, act)] -> assertEqual
      "query should create an empty action in the flow table"
      (fromOFAct act) []
    otherwise -> assertFailure
      "query should create one entry in the flow table"

test_query_2 = do
  (_, act) <- countPkts 1000
  let policy =
        PoUnion (PoBasic Any (allPorts unmodified))
                (PoBasic (DlDst (EthernetAddress 1)) act)
  let tbl = compile 0 policy
  case tbl of
    [(_, act1), (_, act2)] -> do
      assertEqual "allPorts unmodified should come first in the flow-table"
        (fromOFAct act1) [OFAction.SendOutPort OFAction.Flood]
      assertEqual "empty action (for query) should come second in table"
        (fromOFAct act2) []
    otherwise -> assertFailure
      "query should create two entries in the flow table"

{-
xcase_regress_1 = do
  let pred = Not (Not (Switch 0))
  let policy = PoBasic pred (allPorts unmodified)
  let pkt = Packet {pktDlSrc = EthernetAddress 4410948387332,
              pktDlDst = EthernetAddress 6609988486150, pktDlTyp = 1,
              pktDlVlan = Just 4,
              pktDlVlanPcp = 0, pktNwSrc = Just 6, pktNwDst = Just 5,
              pktNwProto = 5,
              pktNwTos = 7, pktTpSrc = Just 5, pktTpDst = Just 0,
              pktInPort = 6}
  let polAct = interpretPolicy policy (Transmission top 0 pkt)
  let classAct = classify 0 pkt (compile 0 policy)
  assertEqual "classifier should produce the same action"
    (Just polAct) classAct

-}

{-
xnegation_regress_maker pred = do
  let act = forward [2]
  let act' = forward [90]
  let pol = PoUnion (PoBasic pred act) (PoBasic (Not pred) act')
  let pkt = Packet {pktDlSrc = EthernetAddress 200,
              pktDlDst = EthernetAddress 500, pktDlTyp = 1, pktDlVlan = Just 4,
              pktDlVlanPcp = 0, pktNwSrc = Just 6, pktNwDst = Just 5,
              pktNwProto = 5,
              pktNwTos = 7, pktTpSrc = Just 5, pktTpDst = Just 0, pktInPort = 6}
  let polAct = interpretPolicy pol (Transmission top 0 pkt)
  assertEqual "policy should fwd 2" act polAct
  let classAct = classify 0 pkt (compile 0 pol)
  assertEqual "flow table should fwd 2"
    (Just act) classAct
  let pkt' = pkt { pktDlDst = EthernetAddress 501 }
  let polAct' = interpretPolicy pol (Transmission top 0 pkt')
  assertEqual "policy should fwd 90"  act' polAct'
  let classAct' = classify 0 pkt' (compile 0 pol)
  assertEqual "flow table should fwd 90"
    (Just act') classAct'

xcase_regress_neg_1 = do
  negation_regress_maker (DlDst (EthernetAddress 500) <&&> Switch 0)

xcase_regress_neg_1_ok = do
  negation_regress_maker (DlDst (EthernetAddress 500))

xcase_regress_neg_1_2 = do
  negation_regress_maker (DlDst (EthernetAddress 500) <&&>
                          DlSrc (EthernetAddress 200))

-}

-- TODO(arjun): this really is a test for nettle
prop_ipAddr_ok :: IPAddress -> Bool
prop_ipAddr_ok addr = 
  addr == ipAddress b3 b2 b1 b0
    where (b3, b2, b1, b0) = addressToOctets addr

{-

-- Invariant: given an arbitrary classifier c, minimze c yields an
--            equivalent classifier.
-- Test with OpenFlow patterns and actions.
xprop_semantic_Equivalence_minimize :: Switch -> PacketInfo
  -> Classifier ActionImpl -> Bool
xprop_semantic_Equivalence_minimize sw pk classifier = res == minimizedRes
  where
    res = classify sw pk classifier
    minimizedRes = classify sw pk minimizedClassifier
    minimizedClassifier = minimizeClassifier classifier


-- Invariant: a policy and the classifier resulting from its compilation
--            should yield an equivalent action for all switches and
--            packets, modulo reactive specialization.
xprop_semanticEquivalence_compile_pattern :: Switch -> Policy
  -> PacketInfo -> Bool
xprop_semanticEquivalence_compile_pattern sw po pk =
  case fmap fromOFAct classActs of
     Nothing -> polActs == dropPkt
     Just [] -> polActs == dropPkt
     Just as -> as == fromOFAct (actnTranslate polActs)
  where topP = top
        polActs = interpretPolicy po $ Transmission {trPattern=topP, trSwitch=sw, trPkt=pk'}
        classifier = compile sw po
        classActs = classify sw pk classifier
        pk' = case toPacket pk of
          Just v -> v
          Nothing -> error "fail"

-- Invariant: a policy and the classifier resulting from its compilation
--            should yield an equivalent action for all switches and
--            packets, modulo reactive specialization.
xprop_semanticEquivalence_compile_OFpattern :: Switch -> Policy
  -> PacketInfo -> Bool
xprop_semanticEquivalence_compile_OFpattern sw po pk =
  case fmap fromOFAct classActs of
    Nothing -> polActs == dropPkt
    Just [] -> polActs == dropPkt
    Just as -> as == fromOFAct (actnTranslate polActs)
  where polActs = interpretPolicy po $ Transmission top sw pk'
        classifier = compile sw po
        classActs = classify sw pk classifier
        pk' = case toPacket pk of
          Just v -> v
          Nothing -> error "fail"

-}

pkx = Packet {pktDlSrc = EthernetAddress 0x00000000006e, 
       pktDlDst = EthernetAddress 0x0000000002ca, pktDlTyp = 55,
       pktDlVlan = Just 25, pktDlVlanPcp = 11, pktNwSrc = Just (IPAddress 9),
       pktNwDst = Just (IPAddress 10), pktNwProto = 44, pktNwTos = 59, pktTpSrc = Just 52, 
       pktTpDst = Nothing, pktInPort = 8}

{-
xcase_sem_bug_1 = do
  let po = PoBasic (NwSrc (IPAddressPrefix (IPAddress 4) 0)) (forward [999])
  let pk = pkx { pktNwSrc = Nothing }
  let sw = 0
  let polActs = interpretPolicy po $ Transmission top sw pk
      classifier = compile sw po
      classActs = classify sw pk classifier
  let ret = case fmap fromOFAct classActs of
        Nothing -> polActs == dropPkt
        Just [] -> polActs == dropPkt
        Just as -> as == fromOFAct (actnTranslate polActs)
  assertEqual "sem_bug_1" ret True

xcase_sem_bug_2 = do
  let po = PoBasic (NwSrc (IPAddressPrefix (IPAddress 0) 0)) (forward [999])
  let pk = Packet {pktDlSrc = EthernetAddress 0x00000000006e, pktDlDst = EthernetAddress 0x0000000002ca, pktDlTyp = 55, pktDlVlan = Just 25, pktDlVlanPcp = 11, pktNwSrc = Just 1574, pktNwDst = Just 10, pktNwProto = 44, pktNwTos = 59, pktTpSrc = Just 52, pktTpDst = Nothing, pktInPort = 8}
  let sw = 0
  let polActs = interpretPolicy po $ Transmission top sw pk
      classifier = compile sw po
      classActs = classify sw pk classifier
  let ret = case fmap fromOFAct classActs of
        Nothing -> polActs == dropPkt
        Just [] -> polActs == dropPkt
        Just as -> as == fromOFAct (actnTranslate polActs)
  assertEqual "sem_bug_2" ret True

xcase_sem_bug_3 = do
  let po = PoBasic (NwSrc (IPAddressPrefix (IPAddress 5) 1)) (forward [999])
  let pk = Packet {pktDlSrc = EthernetAddress 0x00000000006e, pktDlDst = EthernetAddress 0x0000000002ca, pktDlTyp = 55, pktDlVlan = Just 25, pktDlVlanPcp = 11, pktNwSrc = Just 9, pktNwDst = Just 10, pktNwProto = 44, pktNwTos = 59, pktTpSrc = Just 52, pktTpDst = Nothing, pktInPort = 8}
  let sw = 0
  let polActs = interpretPolicy po $ Transmission top sw pk
      classifier = compile sw po
      classActs = classify sw pk classifier
  let ret = case fmap fromOFAct classActs of
        Nothing -> polActs == dropPkt
        Just [] -> polActs == dropPkt
        Just as -> as == fromOFAct (actnTranslate polActs)
  assertEqual "sem_bug_3" ret True

xcase_sem_bug_4 = do
  let po = PoBasic (NwDst (IPAddressPrefix (IPAddress 12) 27)) (forward [999])
  let pk = pkx { pktNwDst = Nothing }
  let sw = 0
  let polActs = interpretPolicy po $ Transmission top sw pk
      classifier = compile sw po
      classActs = classify sw pk classifier
  let ret = case fmap fromOFAct classActs of
        Nothing -> polActs == dropPkt
        Just [] -> polActs == dropPkt
        Just as -> as == fromOFAct (actnTranslate polActs)
  assertEqual "sem_bug_4" ret True



xcase_quiescence_bug_1 = do
  assertEqual "policy -> allPorts unmodified" polActs (allPorts unmodified)
  assertEqual "classifier -> allPorts unmodified"
    (Just [OFAction.SendOutPort OFAction.Flood])
     (fmap (fromOFAct.actnTranslate) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $
      Transmission { trPattern=topP, trSwitch=0, trPkt= packet }
    classifier = compile 0 policy
    classActs = classify 0 packet classifier
    policy = PoBasic (NwSrc (IPAddressPrefix (IPAddress 0x5000001) 32)) (allPorts unmodified)
    packet = Packet {
        pktDlSrc = EthernetAddress 0
      , pktDlDst = EthernetAddress 0
      , pktDlTyp = 0
      , pktDlVlan = Nothing
      , pktDlVlanPcp = 0
      , pktNwSrc = Just 0x5000001
      , pktNwDst = Just 0
      , pktNwProto = 0
      , pktNwTos = 0
      , pktTpSrc = Just 0
      , pktTpDst = Just 0
      , pktInPort = 0
      }

p1 = PoBasic (NwSrc (IPAddressPrefix (IPAddress 0x5000001) 32)) (allPorts unmodified)
p2 = PoBasic (NwSrc (IPAddressPrefix (IPAddress 0x5000002) 32)) (allPorts unmodified)

xcase_quiescence_bug_2 = do
  assertEqual "policy -> allPorts unmodified" polActs (allPorts unmodified)
  assertEqual "classifier -> allPorts unmodified"
    (Just [OFAction.SendOutPort OFAction.Flood])
    (fmap (fromOFAct.actnTranslate) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $ Transmission {trPattern=topP, trSwitch=0, trPkt=packet}
    classifier = compile 0 policy
    classActs = classify 0 packet classifier
    policy = PoUnion p1 p2
    packet = Packet {
        pktDlSrc = EthernetAddress 0
      , pktDlDst = EthernetAddress 0
      , pktDlTyp = 0
      , pktDlVlan = Nothing
      , pktDlVlanPcp = 0
      , pktNwSrc = Just 0x5000001
      , pktNwDst = Just 0
      , pktNwProto = 0
      , pktNwTos = 0
      , pktTpSrc = Just 0
      , pktTpDst = Just 0
      , pktInPort = 0
      }

xcase_quiescence_bug_3 = do
  assertEqual "policy -> allPorts unmodified" polActs (allPorts unmodified)
  assertEqual "classifier -> allPorts unmodified"
    (Just [OFAction.SendOutPort OFAction.Flood])
    (fmap (fromOFAct.actnTranslate) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $ Transmission {trPattern=topP, trSwitch=0, trPkt=packet}
    classifier = compile 0 policy
    classActs = classify 0 packet classifier
    policy = PoUnion p1 p2
    packet = Packet {
        pktDlSrc = EthernetAddress 0
      , pktDlDst = EthernetAddress 0
      , pktDlTyp = 0
      , pktDlVlan = Nothing
      , pktDlVlanPcp = 0
      , pktNwSrc = Just 0x5000001
      , pktNwDst = Just 0
      , pktNwProto = 0
      , pktNwTos = 0
      , pktTpSrc = Just 0
      , pktTpDst = Just 0
      , pktInPort = 0
      }

xcase_quiescence_bug_4 = do
  assertEqual "policy -> allPorts unmodified" polActs (allPorts unmodified)
  assertEqual "classifier -> allPorts unmodified"
    (Just [OFAction.SendOutPort OFAction.Flood])
    (fmap (fromOFAct.actnTranslate) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $ Transmission {trPattern=topP, trSwitch=0, trPkt=packet}
    classifier = compile 0 policy
    classActs = classify 0 packet classifier
    policy = PoBasic (Or pattern (Switch switch)) (allPorts unmodified)
    switch = 0
    pattern = prAnd
      [ DlSrc (EthernetAddress 0)
      , DlDst (EthernetAddress 0)
      , DlTyp 0
      , DlVlan Nothing
      , DlVlanPcp 0
      , NwSrc (IPAddressPrefix (IPAddress 0) 32)
      , NwDst (IPAddressPrefix (IPAddress 0) 32)
      , NwProto 0
      , NwTos 0
      , TpSrcPort 0
      , TpDstPort 0
      ]
    packet = Packet {
        pktDlSrc = EthernetAddress 0
      , pktDlDst = EthernetAddress 0
      , pktDlTyp = 0
      , pktDlVlan = Nothing
      , pktDlVlanPcp = 0
      , pktNwSrc = Just 0
      , pktNwDst = Just 0
      , pktNwProto = 0
      , pktNwTos = 0
      , pktTpSrc = Just 0
      , pktTpDst = Just 0
      , pktInPort = 0
      }

xcase_quiescence_bug_5 = do
  assertEqual "policy -> allPorts unmodified" polActs (forward [1])
  assertEqual "classifier -> allPorts unmodified"
    (Just [OFAction.SendOutPort $ OFAction.PhysicalPort 1])
    (fmap (fromOFAct.actnTranslate) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $ Transmission topP 0 packet
    classifier = compile switch policy
    classActs = classify switch packet classifier
    policy = Switch switch ==> forward [1]
    switch = 0
    packet = Packet {
        pktDlSrc = EthernetAddress 1
      , pktDlDst = EthernetAddress 1
      , pktDlTyp = 1
      , pktDlVlan = Just 1
      , pktDlVlanPcp = 1
      , pktNwSrc = Just 1
      , pktNwDst = Just 0
      , pktNwProto = 1
      , pktNwTos = 1
      , pktTpSrc = Just 1
      , pktTpDst = Just 1
      , pktInPort = 1
      }

-}