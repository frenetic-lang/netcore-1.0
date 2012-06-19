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
-- * Redistributions of source code must retain the above copyright           --
--   notice, this list of conditions and the following disclaimer.            --
-- * Redistributions of binaries must reproduce the above copyright           --
--   notice, this list of conditions and the following disclaimer in          --
--   the documentation or other materials provided with the distribution.     --
-- * The names of the copyright holds and contributors may not be used to     --
--   endorse or promote products derived from this work without specific      --
--   prior written permission.                                                --
--                                                                            --
-- Unless required by applicable law or agreed to in writing, software        --
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  --
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   --
-- LICENSE file distributed with this work for specific language governing    --
-- permissions and limitations under the License.                             --
--------------------------------------------------------------------------------
-- /testsuite/Frenetic/TestCompiler                                           --
--                                                                            --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE
    TemplateHaskell,
    FlexibleContexts
 #-}

module Tests.Frenetic.NetCore.TestCompiler where

import qualified Data.Set as Set
import Data.Word
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Property (Property, morallyDubiousIOProperty)
import Test.QuickCheck.Text
import Test.HUnit
import Test.Framework.Providers.HUnit

import Frenetic.Compat
import Tests.Frenetic.ArbitraryCompat
import Frenetic.Pattern
import Tests.Frenetic.ArbitraryPattern
import Frenetic.NetCore.API
import Frenetic.NetCore.Action
import Tests.Frenetic.NetCore.ArbitraryAPI

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

case_regress_1 = do
  let pred = PrNegate (PrNegate (PrTo 0))
  let policy = PoBasic pred flood
  let pkt = FreneticPkt (Packet {pktDlSrc = 4410948387332, 
              pktDlDst = 6609988486150, pktDlTyp = 1, pktDlVlan = 4, 
              pktDlVlanPcp = 0, pktNwSrc = 6, pktNwDst = 5, pktNwProto = 5, 
              pktNwTos = 7, pktTpSrc = 5, pktTpDst = 0, pktInPort = 6})
  let (polAct, _) = interpretPolicy policy (Transmission top 0 pkt)
  let classAct = classify 0 pkt (compile 0 policy)
  assertEqual "classifier should produce the same action" 
    (Just (FreneticAct polAct)) classAct


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
  case (polActs, fmap freneticToOFAct classActs) of
  ((s1, s2), Nothing)
    | s1 == emptyAction && s2 == emptyAction    -> True
  ((s1, s2), Just []) -- the fromOFAct calls are only needed because of this []
    | s1 == emptyAction && s2 == emptyAction    -> True
  ((s1, s2), Just as)
    | as == fromOFAct actnController                  -> True -- TODO
    | as /= fromOFAct actnController && s1 /= s2      -> False
    | otherwise -> as == fromOFAct (actnTranslate s1)
  _                                         -> False
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
  case (polActs,fmap freneticToOFAct classActs) of
  ((s1, s2), Nothing)
    | s1 == emptyAction && s2 == emptyAction    -> True
  ((s1, s2), Just [])
    | s1 == emptyAction && s2 == emptyAction    -> True
  ((s1, s2), Just as)
    | as == fromOFAct actnController                  -> True -- TODO
    | as /= fromOFAct actnController && s1 /= s2      -> False
    | otherwise ->  as == fromOFAct (actnTranslate s1)
  _                                         -> False
  where topP = top
        polActs = interpretPolicy po $ Transmission {trPattern=topP, trSwitch=sw, trPkt=pk}
        classifier = compile sw po
        classActs = classify sw pk classifier


case_quiescence_bug_1 = do
  assertEqual "policy -> flood" polActs (flood, flood)
  assertEqual "classifier -> flood" 
    (Just [OFAction.SendOutPort OFAction.Flood]) 
     (fmap (fromOFAct.actnTranslate.fromFreneticAct) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $ 
      Transmission { trPattern=topP, trSwitch=0, trPkt= packet }
    classifier = compile 0 policy
    classActs = classify 0 packet classifier
    policy = PoBasic (PrPattern top{ptrnNwSrc = Wildcard 0x5000001 0}) flood
    packet = FreneticPkt $ Packet {
        pktDlSrc = 0
      , pktDlDst = 0
      , pktDlTyp = 0
      , pktDlVlan = 0
      , pktDlVlanPcp = 0
      , pktNwSrc = 0x5000001
      , pktNwDst = 0
      , pktNwProto = 0
      , pktNwTos = 0
      , pktTpSrc = 0
      , pktTpDst = 0
      , pktInPort = 0
      }
 
case_quiescence_bug_2 = do
  assertEqual "policy -> flood" polActs (flood, flood)
  assertEqual "classifier -> flood" 
    (Just [OFAction.SendOutPort OFAction.Flood])
    (fmap (fromOFAct.actnTranslate.fromFreneticAct) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $ Transmission {trPattern=topP, trSwitch=0, trPkt=packet}
    classifier = compile 0 policy
    classActs = classify 0 packet classifier
    policy = PoUnion p1 p2
    p1 = PoBasic (PrPattern top{ptrnNwSrc = Wildcard 0x5000001 0}) flood
    p2 = PoBasic (PrPattern top{ptrnNwSrc = Wildcard 0x5000002 0}) flood
    packet = FreneticPkt $ Packet {
        pktDlSrc = 0
      , pktDlDst = 0
      , pktDlTyp = 0
      , pktDlVlan = 0
      , pktDlVlanPcp = 0
      , pktNwSrc = 0x5000001
      , pktNwDst = 0
      , pktNwProto = 0
      , pktNwTos = 0
      , pktTpSrc = 0
      , pktTpDst = 0
      , pktInPort = 0
      }
    
case_quiescence_bug_3 = do
  assertEqual "policy -> flood" polActs (flood, flood)
  assertEqual "classifier -> flood" 
    (Just [OFAction.SendOutPort OFAction.Flood])
    (fmap (fromOFAct.actnTranslate.fromFreneticAct) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $ Transmission {trPattern=topP, trSwitch=0, trPkt=packet}
    classifier = compile 0 policy
    classActs = classify 0 packet classifier
    policy = PoUnion p1 p2
    p1 = PoBasic (PrPattern top{ptrnNwSrc = Wildcard 0x5000001 0}) flood
    p2 = PoBasic (PrPattern top{ptrnNwSrc = Wildcard 0x5000002 0}) flood
    packet = FreneticPkt $ Packet {
        pktDlSrc = 0
      , pktDlDst = 0
      , pktDlTyp = 0
      , pktDlVlan = 0
      , pktDlVlanPcp = 0
      , pktNwSrc = 0x5000001
      , pktNwDst = 0
      , pktNwProto = 0
      , pktNwTos = 0
      , pktTpSrc = 0
      , pktTpDst = 0
      , pktInPort = 0
      }

case_quiescence_bug_4 = do
  assertEqual "policy -> flood" polActs (flood, flood)
  assertEqual "classifier -> flood" 
    (Just [OFAction.SendOutPort OFAction.Flood])
    (fmap (fromOFAct.actnTranslate.fromFreneticAct) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $ Transmission {trPattern=topP, trSwitch=0, trPkt=packet}
    classifier = compile 0 policy
    classActs = classify 0 packet classifier
    policy = PoBasic (PrUnion (PrPattern pattern) (PrTo switch)) flood
    switch = 0
    pattern = Pattern {
        ptrnDlSrc = Wildcard 0 0
      , ptrnDlDst = Wildcard 0 0
      , ptrnDlTyp = Wildcard 0 0
      , ptrnDlVlan = Wildcard 0 0
      , ptrnDlVlanPcp = Wildcard 0 0
      , ptrnNwSrc = Wildcard 0 0
      , ptrnNwDst = Wildcard 0 0
      , ptrnNwProto = Wildcard 0 0
      , ptrnNwTos = Wildcard 0 0
      , ptrnTpSrc = Wildcard 0 0
      , ptrnTpDst = Wildcard 0 0
      , ptrnInPort = Nothing
      }
    packet = FreneticPkt $ Packet {
        pktDlSrc = 0
      , pktDlDst = 0
      , pktDlTyp = 0
      , pktDlVlan = 0
      , pktDlVlanPcp = 0
      , pktNwSrc = 0
      , pktNwDst = 0
      , pktNwProto = 0
      , pktNwTos = 0
      , pktTpSrc = 0
      , pktTpDst = 0
      , pktInPort = 0
      }

case_quiescence_bug_5 = do
  hPutStrLn stderr ("bug_5: policy: " ++ show policy)
  hPutStrLn stderr ("bug_5: classifier: " ++ show classifier)
  assertEqual "policy -> flood" polActs (forward 1, forward 1)
  assertEqual "classifier -> flood" 
    (Just [OFAction.SendOutPort $ OFAction.PhysicalPort 1])
    (fmap (fromOFAct.actnTranslate.fromFreneticAct) classActs)
  where
    topP = top
    polActs = interpretPolicy policy $ Transmission {trPattern=topP, trSwitch=0, trPkt=packet}
    classifier = compile switch policy
    classActs = classify switch packet classifier
    policy = PoBasic (PrTo switch) $ (forward 1)
    switch = 0
    packet = FreneticPkt $ Packet {
        pktDlSrc = 1
      , pktDlDst = 1
      , pktDlTyp = 1
      , pktDlVlan = 1
      , pktDlVlanPcp = 1
      , pktNwSrc = 1
      , pktNwDst = 0
      , pktNwProto = 1
      , pktNwTos = 1
      , pktTpSrc = 1
      , pktTpDst = 1
      , pktInPort = 1
      }

