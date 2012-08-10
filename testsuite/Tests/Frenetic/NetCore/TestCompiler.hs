{-# LANGUAGE
    TemplateHaskell,
    FlexibleContexts
 #-}

module Tests.Frenetic.NetCore.TestCompiler where

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
import Tests.Frenetic.ArbitraryPattern
import Frenetic.NetCore
import Tests.Frenetic.NetCore.ArbitraryTypes

import Frenetic.NetCore.Compiler
import Frenetic.Switches.OpenFlow
import Tests.Frenetic.Switches.ArbitraryOpenFlow

import qualified Nettle.OpenFlow.Action as OFAction
import qualified Nettle.OpenFlow.Match as OFMatch

import System.IO

compilerTests = $(testGroupGenerator)

-- |It's easier to write tests with Nettle's actions.
freneticToOFAct :: ActionImpl () -> OFAction.ActionSequence
freneticToOFAct = fromOFAct.actnTranslate.fromFreneticAct

case_test_query_1 = do
  (_, act) <- countPkts 1000
  let policy = PoBasic (PrTo 0) act
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
        PoUnion (PoBasic (PrPattern top) (allPorts unmodified))
                (PoBasic (PrPattern $ top { ptrnDlDst = Exact $ ethernetAddress64 1 }) act)
  let tbl = compile 0 policy
  case tbl of
    [(_, act1), (_, act2)] -> do
      assertEqual "allPorts unmodified should come first in the flow-table"
        (fromOFAct act1) [OFAction.SendOutPort OFAction.Flood]
      assertEqual "empty action (for query) should come second in table"
        (fromOFAct act2) []
    otherwise -> assertFailure
      "query should create two entries in the flow table"

case_regress_1 = do
  let pred = PrNegate (PrNegate (PrTo 0))
  let policy = PoBasic pred (allPorts unmodified)
  let pkt = FreneticPkt (Packet {pktDlSrc = ethernetAddress64 4410948387332,
              pktDlDst = ethernetAddress64 6609988486150, pktDlTyp = 1,
              pktDlVlan = Just 4,
              pktDlVlanPcp = 0, pktNwSrc = Just 6, pktNwDst = Just 5,
              pktNwProto = 5,
              pktNwTos = 7, pktTpSrc = Just 5, pktTpDst = Just 0,
              pktInPort = 6})
  let polAct = interpretPolicy policy (Transmission top 0 pkt)
  let classAct = classify 0 pkt (compile 0 policy)
  assertEqual "classifier should produce the same action"
    (Just (FreneticAct polAct)) classAct

negation_regress_maker pred = do
  let act = forward [2]
  let act' = forward [90]
  let pol = PoUnion (PoBasic pred act) (PoBasic (PrNegate pred) act')
  let pkt = Packet {pktDlSrc = ethernetAddress64 200,
              pktDlDst = ethernetAddress64 500, pktDlTyp = 1, pktDlVlan = Just 4,
              pktDlVlanPcp = 0, pktNwSrc = Just 6, pktNwDst = Just 5,
              pktNwProto = 5,
              pktNwTos = 7, pktTpSrc = Just 5, pktTpDst = Just 0, pktInPort = 6}
  let polAct = interpretPolicy pol (Transmission top 0 (FreneticPkt pkt))
  assertEqual "policy should fwd 2" act polAct
  let classAct = classify 0 (FreneticPkt pkt) (compile 0 pol)
  assertEqual "flow table should fwd 2"
    (Just (FreneticAct act)) classAct
  let pkt' = pkt { pktDlDst = ethernetAddress64 501 }
  let polAct' = interpretPolicy pol (Transmission top 0 (FreneticPkt pkt'))
  assertEqual "policy should fwd 90"  act' polAct'
  let classAct' = classify 0 (FreneticPkt pkt') (compile 0 pol)
  assertEqual "flow table should fwd 90"
    (Just (FreneticAct act')) classAct'

case_regress_neg_1 = do
  negation_regress_maker (dlDst (ethernetAddress64 500) <&&> PrTo 0)

case_regress_neg_1_ok = do
  negation_regress_maker (dlDst (ethernetAddress64 500))

case_regress_neg_1_2 = do
  negation_regress_maker (dlDst (ethernetAddress64 500) <&&>
                          dlSrc (ethernetAddress64 200))

-- Invariant: given an arbitrary classifier c, minimze c yields an
--            equivalent classifier.
-- Test with OpenFlow patterns and actions.
prop_semantic_Equivalence_minimize :: Switch -> PacketImpl ()
  -> Classifier (PatternImpl ()) (ActionImpl ()) -> Bool
prop_semantic_Equivalence_minimize sw pk classifier = res == minimizedRes
  where
    res = classify sw pk classifier
    minimizedRes = classify sw pk minimizedClassifier
    minimizedClassifier = minimizeClassifier classifier


-- Invariant: a policy and the classifier resulting from its compilation
--            should yield an equivalent action for all switches and
--            packets, modulo reactive specialization.
prop_semanticEquivalence_compile_pattern :: Switch -> Policy
  -> PacketImpl () -> Bool
prop_semanticEquivalence_compile_pattern sw po pk =
  case fmap freneticToOFAct classActs of
     Nothing -> polActs == dropPkt
     Just [] -> polActs == dropPkt
     Just as -> as == fromOFAct (actnTranslate polActs)
  where topP = top
        polActs = interpretPolicy po $ Transmission {trPattern=topP, trSwitch=sw, trPkt=pk}
        classifier = compile sw po
        classActs = classify sw pk classifier

-- Invariant: a policy and the classifier resulting from its compilation
--            should yield an equivalent action for all switches and
--            packets, modulo reactive specialization.
prop_semanticEquivalence_compile_OFpattern :: Switch -> Policy
  -> PacketImpl () -> Bool
prop_semanticEquivalence_compile_OFpattern sw po pk =
  case fmap freneticToOFAct classActs of
    Nothing -> polActs == dropPkt
    Just [] -> polActs == dropPkt
    Just as -> as == fromOFAct (actnTranslate polActs)
  where topP = top
        polActs = interpretPolicy po $ Transmission {trPattern=topP, trSwitch=sw, trPkt=pk}
        classifier = compile sw po
        classActs = classify sw pk classifier


case_quiescence_bug_1 = do
  assertEqual "policy -> allPorts unmodified" polActs (allPorts unmodified)
  assertEqual "classifier -> allPorts unmodified"
    (Just [OFAction.SendOutPort OFAction.Flood])
     (fmap (fromOFAct.actnTranslate.fromFreneticAct) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $
      Transmission { trPattern=topP, trSwitch=0, trPkt= packet }
    classifier = compile 0 policy
    classActs = classify 0 packet classifier
    policy = PoBasic (PrPattern top{ptrnNwSrc = Prefix 0x5000001 32}) (allPorts unmodified)
    packet = FreneticPkt $ Packet {
        pktDlSrc = ethernetAddress64 0
      , pktDlDst = ethernetAddress64 0
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

case_quiescence_bug_2 = do
  assertEqual "policy -> allPorts unmodified" polActs (allPorts unmodified)
  assertEqual "classifier -> allPorts unmodified"
    (Just [OFAction.SendOutPort OFAction.Flood])
    (fmap (fromOFAct.actnTranslate.fromFreneticAct) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $ Transmission {trPattern=topP, trSwitch=0, trPkt=packet}
    classifier = compile 0 policy
    classActs = classify 0 packet classifier
    policy = PoUnion p1 p2
    p1 = PoBasic (PrPattern top{ptrnNwSrc = Prefix 0x5000001 32}) (allPorts unmodified)
    p2 = PoBasic (PrPattern top{ptrnNwSrc = Prefix  0x5000002 32}) (allPorts unmodified)
    packet = FreneticPkt $ Packet {
        pktDlSrc = ethernetAddress64 0
      , pktDlDst = ethernetAddress64 0
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

case_quiescence_bug_3 = do
  assertEqual "policy -> allPorts unmodified" polActs (allPorts unmodified)
  assertEqual "classifier -> allPorts unmodified"
    (Just [OFAction.SendOutPort OFAction.Flood])
    (fmap (fromOFAct.actnTranslate.fromFreneticAct) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $ Transmission {trPattern=topP, trSwitch=0, trPkt=packet}
    classifier = compile 0 policy
    classActs = classify 0 packet classifier
    policy = PoUnion p1 p2
    p1 = PoBasic (PrPattern top{ptrnNwSrc = Prefix 0x5000001 32}) (allPorts unmodified)
    p2 = PoBasic (PrPattern top{ptrnNwSrc = Prefix 0x5000002 32}) (allPorts unmodified)
    packet = FreneticPkt $ Packet {
        pktDlSrc = ethernetAddress64 0
      , pktDlDst = ethernetAddress64 0
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

case_quiescence_bug_4 = do
  assertEqual "policy -> allPorts unmodified" polActs (allPorts unmodified)
  assertEqual "classifier -> allPorts unmodified"
    (Just [OFAction.SendOutPort OFAction.Flood])
    (fmap (fromOFAct.actnTranslate.fromFreneticAct) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $ Transmission {trPattern=topP, trSwitch=0, trPkt=packet}
    classifier = compile 0 policy
    classActs = classify 0 packet classifier
    policy = PoBasic (PrUnion (PrPattern pattern) (PrTo switch)) (allPorts unmodified)
    switch = 0
    pattern = Pattern {
        ptrnDlSrc = Exact (ethernetAddress64 0)
      , ptrnDlDst = Exact (ethernetAddress64 0)
      , ptrnDlTyp = Exact 0
      , ptrnDlVlan = Exact Nothing
      , ptrnDlVlanPcp = Exact 0
      , ptrnNwSrc = Prefix 0 32
      , ptrnNwDst = Prefix 0 32
      , ptrnNwProto = Exact 0
      , ptrnNwTos = Exact 0
      , ptrnTpSrc = Exact 0
      , ptrnTpDst = Exact 0
      , ptrnInPort = Wildcard
      }
    packet = FreneticPkt $ Packet {
        pktDlSrc = ethernetAddress64 0
      , pktDlDst = ethernetAddress64 0
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

case_quiescence_bug_5 = do
  assertEqual "policy -> allPorts unmodified" polActs (forward [1])
  assertEqual "classifier -> allPorts unmodified"
    (Just [OFAction.SendOutPort $ OFAction.PhysicalPort 1])
    (fmap (fromOFAct.actnTranslate.fromFreneticAct) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $ Transmission topP 0 packet
    classifier = compile switch policy
    classActs = classify switch packet classifier
    policy = PrTo switch ==> forward [1]
    switch = 0
    packet = FreneticPkt $ Packet {
        pktDlSrc = ethernetAddress64 1
      , pktDlDst = ethernetAddress64 1
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

