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

import qualified Data.Set as Set
import Data.Word
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Property (Property, morallyDubiousIOProperty)
import Test.QuickCheck.Text

import Frenetic.Compat
import Tests.Frenetic.ArbitraryCompat
import Frenetic.Pattern
import Tests.Frenetic.ArbitraryPattern
import Frenetic.NetCore.API
import Tests.Frenetic.NetCore.ArbitraryAPI

import Frenetic.NetCore.Compiler
import Frenetic.Switches.OpenFlow

import qualified Nettle.OpenFlow.Action as OFAction

-- main = $(defaultMainGenerator)

-- prop_semanticEquivalence :: Switch -> Policy -> Packet -> Bool
-- prop_semanticEquivalence sw po pk = case (polActs,classActs) of
--   ((s1, s2), Just [])
--     | s1 == Set.empty && s2 == Set.empty    -> True
--   ((s1, s2), Just as)
--     | as == actnController                  -> True -- TODO
--     | as /= actnController && s1 /= s2      -> False
--     | otherwise                             -> as == actnTranslate s1
--   _                                         -> False
--   where polActs = interpretPolicy po $ Transmission {trPattern=pTop, trSwitch=sw, trPkt=pk}
--         classifier :: Classifier Pattern OFAction.ActionSequence
--         classifier = compile sw po
--         classActs :: Maybe OFAction.ActionSequence
--         classActs = classify sw pk classifier

main = defaultMain tests

tests = [
          testGroup "NetCore Compiler" [
            testProperty "semantic equivalence" prop_semanticEquivalenceIO
          ]
        ]

prop_semanticEquivalenceIO :: Switch -> Policy -> Packet -> Property
prop_semanticEquivalenceIO sw po pk = morallyDubiousIOProperty $ semanticEquivalenceIO sw po pk

semanticEquivalenceIO :: Switch -> Policy -> Packet -> IO Bool
semanticEquivalenceIO sw po pk = case (polActs,classActs) of
  ((s1, s2), Nothing)
    | s1 == Set.empty && s2 == Set.empty    -> return True
  ((s1, s2), Just [])
    | s1 == Set.empty && s2 == Set.empty    -> return True
  ((s1, s2), Just as)
    | as == actnController                  -> return True -- TODO
    | as /= actnController && s1 /= s2      -> do
        term <- newStdioTerminal
        putLine term ("fail (1): " ++ show s1 ++ " /= " ++ show s2)
        return False
    | otherwise                             -> 
        let rv = as == actnTranslate s1 in
          if rv
            then return True
          else do
            term <- newStdioTerminal
            putLine term ("fail (2): " ++ show s1 ++ " /= " ++ show as)
            putLine term ("classifier: " ++ ruleStrings)
            return False
  _                                         -> 
      do
        term <- newStdioTerminal
        putLine term "other fail"
        return False
  where polActs = interpretPolicy po $ Transmission {trPattern=pTop, trSwitch=sw, trPkt=pk}
        classifier :: Classifier Pattern OFAction.ActionSequence
        classifier = compile sw po
        ruleStrings = show classifier
        classActs :: Maybe OFAction.ActionSequence
        classActs = classify sw pk classifier

{- Pattern that matches anything. -}
pTop :: Pattern
pTop = Pattern {
  ptrnDlSrc=star ()
  , ptrnDlDst=star ()
  , ptrnDlTyp=star ()
  , ptrnDlVlan=star ()
  , ptrnDlVlanPcp=star ()
  , ptrnNwSrc=star ()
  , ptrnNwDst=star ()
  , ptrnNwProto=star ()
  , ptrnNwTos=star ()
  , ptrnTpSrc=star ()
  , ptrnTpDst=star ()
  , ptrnInPort=Nothing }
 
