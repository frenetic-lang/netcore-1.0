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
-- /testsuite/Frenetic/ArbitraryCompat                                        --
--                                                                            --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE
    TypeSynonymInstances
 #-}

module Tests.Frenetic.ArbitraryCompat where
import Frenetic.Compat
import Frenetic.LargeWord
import Test.QuickCheck

buildWord48 w1 w2 w3 w4 w5 w6 = 
  LargeKey 
    w1 
    (LargeKey w2 (LargeKey w3 (LargeKey w4 (LargeKey w5 w6))))

instance Arbitrary Word48 where
  arbitrary = do
    w1 <- arbitrary
    w2 <- arbitrary
    w3 <- arbitrary
    w4 <- arbitrary
    w5 <- arbitrary
    w6 <- arbitrary
    return $ buildWord48 w1 w2 w3 w4 w5 w6

  shrink (LargeKey 
           w1 
           (LargeKey w2 (LargeKey w3 (LargeKey w4 (LargeKey w5 w6))))) =
    [buildWord48 s w2 w3 w4 w5 w6 | s <- shrink w1] ++
    [buildWord48 w1 s w3 w4 w5 w6 | s <- shrink w2] ++
    [buildWord48 w1 w2 s w4 w5 w6 | s <- shrink w3] ++
    [buildWord48 w1 w2 w3 s w5 w6 | s <- shrink w4] ++
    [buildWord48 w1 w2 w3 w4 s w6 | s <- shrink w5] ++
    [buildWord48 w1 w2 w3 w4 w5 s | s <- shrink w6]


instance Arbitrary Packet where
  arbitrary = do
    dlsrc       <- arbitrary
    dldst       <- arbitrary
    dltyp       <- arbitrary
    dlvlan      <- arbitrary
    dlvlanpcp   <- arbitrary
    nwsrc       <- arbitrary
    nwdst       <- arbitrary
    nwproto     <- arbitrary
    nwtos       <- arbitrary
    tpsrc       <- arbitrary
    tpdst       <- arbitrary
    inport      <- arbitrary
    return $ Packet 
               dlsrc dldst dltyp dlvlan dlvlanpcp nwsrc nwdst nwproto nwtos 
               tpsrc tpdst inport

  shrink p = 
    [p {pktDlSrc = s}       | s <- shrink (pktDlSrc p)] ++
    [p {pktDlDst = s}       | s <- shrink (pktDlDst p)] ++
    [p {pktDlTyp = s}       | s <- shrink (pktDlTyp p)] ++
    [p {pktDlVlan = s}      | s <- shrink (pktDlVlan p)] ++
    [p {pktDlVlanPcp = s}   | s <- shrink (pktDlVlanPcp p)] ++
    [p {pktNwSrc = s}       | s <- shrink (pktNwSrc p)] ++
    [p {pktNwDst = s}       | s <- shrink (pktNwDst p)] ++
    [p {pktNwProto = s}     | s <- shrink (pktNwProto p)] ++
    [p {pktNwTos = s}       | s <- shrink (pktNwTos p)] ++
    [p {pktTpSrc = s}       | s <- shrink (pktTpSrc p)] ++
    [p {pktTpDst = s}       | s <- shrink (pktTpDst p)] ++
    [p {pktInPort = s}      | s <- shrink (pktInPort p)]

