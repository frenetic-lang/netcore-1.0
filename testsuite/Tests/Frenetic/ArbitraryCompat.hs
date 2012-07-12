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
    TypeSynonymInstances,
    TemplateHaskell,
    MultiParamTypeClasses,
    FlexibleInstances
 #-}

module Tests.Frenetic.ArbitraryCompat where
import Frenetic.NetCore.Semantics
import qualified Data.Set as Set
import Data.Word
import Data.Bits
import Frenetic.Compat
import Frenetic.LargeWord
import Frenetic.Pattern
import Test.QuickCheck
import Tests.Frenetic.ArbitraryPattern
import Control.Newtype.TH
import Control.Newtype
import Frenetic.Switches.OpenFlow
import Frenetic.NetCore.API

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

instance Arbitrary (PacketImpl ()) where
  arbitrary = do
    v <- arbitrary
    return (FreneticPkt v)


instance Arbitrary Pattern where
  arbitrary = do
    ptrnDlSrc       <- arbitrary
    ptrnDlDst       <- arbitrary
    ptrnDlTyp       <- arbitrary
    ptrnDlVlan      <- arbitrary
    ptrnDlVlanPcp   <- arbitrary
    ptrnNwSrc       <- arbitrary
    ptrnNwDst       <- arbitrary
    ptrnNwProto     <- arbitrary
    ptrnNwTos       <- arbitrary
    ptrnTpSrc       <- arbitrary
    ptrnTpDst       <- arbitrary
    ptrnInPort      <- arbitrary
    return $ Pattern ptrnDlSrc ptrnDlDst ptrnDlTyp ptrnDlVlan ptrnDlVlanPcp
                     ptrnNwSrc ptrnNwDst ptrnNwProto ptrnNwTos ptrnTpSrc
                     ptrnTpDst ptrnInPort

  shrink p =
    [p {ptrnDlSrc = s}	    | s <- shrink (ptrnDlSrc p)] ++
    [p {ptrnDlDst = s}	    | s <- shrink (ptrnDlDst p)] ++
    [p {ptrnDlTyp = s}	    | s <- shrink (ptrnDlTyp p)] ++
    [p {ptrnDlVlan = s}	    | s <- shrink (ptrnDlVlan p)] ++
    [p {ptrnDlVlanPcp = s}	| s <- shrink (ptrnDlVlanPcp p)] ++
    [p {ptrnNwSrc = s}	    | s <- shrink (ptrnNwSrc p)] ++
    [p {ptrnNwDst = s}	    | s <- shrink (ptrnNwDst p)] ++
    [p {ptrnNwProto = s}	| s <- shrink (ptrnNwProto p)] ++
    [p {ptrnNwTos = s}	    | s <- shrink (ptrnNwTos p)] ++
    [p {ptrnTpSrc = s}	    | s <- shrink (ptrnTpSrc p)] ++
    [p {ptrnTpDst = s}	    | s <- shrink (ptrnTpDst p)] ++
    [p {ptrnInPort = s}	    | s <- shrink (ptrnInPort p)]

newtype ExactishPattern = ExactishPattern Pattern
    deriving (Show, Eq)
$(mkNewTypes[''ExactishPattern])

instance Arbitrary ExactishPattern where
  arbitrary = do
    ptrnDlSrc_in      <- arbitrary
    ptrnDlDst_in      <- arbitrary
    ptrnDlTyp_in      <- arbitrary
    ptrnDlVlan_in     <- arbitrary
    ptrnDlVlanPcp_in  <- arbitrary
    ptrnNwSrc_in      <- arbitrary
    ptrnNwDst_in      <- arbitrary
    ptrnNwProto_in    <- arbitrary
    ptrnNwTos_in      <- arbitrary
    ptrnTpSrc_in      <- arbitrary
    ptrnTpDst_in      <- arbitrary
    ptrnInPort        <- arbitrary
    let ptrnDlSrc :: Wildcard Word48
        ptrnDlDst :: Wildcard Word48
        ptrnDlTyp :: Wildcard Word16
        ptrnDlVlan :: Wildcard Word16
        ptrnDlVlanPcp :: Wildcard Word8
        ptrnNwSrc :: Wildcard Word32
        ptrnNwDst :: Wildcard Word32
        ptrnNwProto :: Wildcard Word8
        ptrnNwTos :: Wildcard Word8
        ptrnTpSrc :: Wildcard Word16
        ptrnTpDst :: Wildcard Word16
        ptrnDlSrc = unW ptrnDlSrc_in
        ptrnDlDst = unW ptrnDlDst_in
        ptrnDlTyp = unW ptrnDlTyp_in
        ptrnDlVlan = unW ptrnDlVlan_in
        ptrnDlVlanPcp = unW ptrnDlVlanPcp_in
        ptrnNwSrc = unW ptrnNwSrc_in
        ptrnNwDst = unW ptrnNwDst_in
        ptrnNwProto = unW ptrnNwProto_in
        ptrnNwTos = unW ptrnNwTos_in
        ptrnTpSrc = unW ptrnTpSrc_in
        ptrnTpDst = unW ptrnTpDst_in
    return $ pack $ Pattern ptrnDlSrc ptrnDlDst ptrnDlTyp ptrnDlVlan ptrnDlVlanPcp
                            ptrnNwSrc ptrnNwDst ptrnNwProto ptrnNwTos ptrnTpSrc
                            ptrnTpDst ptrnInPort

  shrink p_in =
    let p = unpack p_in
        r = [p {ptrnDlSrc = s}	    | s <- shrink (ptrnDlSrc p)] ++
            [p {ptrnDlDst = s}	    | s <- shrink (ptrnDlDst p)] ++
            [p {ptrnDlTyp = s}	    | s <- shrink (ptrnDlTyp p)] ++
            [p {ptrnDlVlan = s}	    | s <- shrink (ptrnDlVlan p)] ++
            [p {ptrnDlVlanPcp = s}	| s <- shrink (ptrnDlVlanPcp p)] ++
            [p {ptrnNwSrc = s}	    | s <- shrink (ptrnNwSrc p)] ++
            [p {ptrnNwDst = s}	    | s <- shrink (ptrnNwDst p)] ++
            [p {ptrnNwProto = s}	| s <- shrink (ptrnNwProto p)] ++
            [p {ptrnNwTos = s}	    | s <- shrink (ptrnNwTos p)] ++
            [p {ptrnTpSrc = s}	    | s <- shrink (ptrnTpSrc p)] ++
            [p {ptrnTpDst = s}	    | s <- shrink (ptrnTpDst p)] ++
            [p {ptrnInPort = s}	    | s <- shrink (ptrnInPort p)]
    in Prelude.map pack r


instance (Arbitrary a, Ord a) => Arbitrary (Set.Set a) where
  arbitrary = do
    l <- listOf arbitrary
    return $ Set.fromList l

  shrink s =
    let f a b = let shrunkElems = shrink a
                in case shrunkElems of
                     [] -> b
                     _  -> Set.union (Set.fromList shrunkElems) b
    in [Set.fold f Set.empty s]

instance (Arbitrary ptrn, Arbitrary pkt) => Arbitrary (Transmission ptrn pkt) where
  arbitrary = do
    pt <- arbitrary
    sw <- arbitrary
    pk <- arbitrary
    return $ Transmission pt sw pk

  shrink t =
    [t {trPattern = s} | s <- shrink (trPattern t)] ++
    [t {trSwitch = s} | s <- shrink (trSwitch t)] ++
    [t {trPkt = s} | s <- shrink (trPkt t)]

instance Arbitrary Action where
  arbitrary = do
    -- TODO(arjun): queries
    oneof [ return flood,
            do ports <- listOf arbitrary
               return $ foldr unionAction dropPkt (map forward ports)
          ]

instance Arbitrary (PatternImpl ()) where
  arbitrary = do
    v <- arbitrary
    return (FreneticPat v)

instance Arbitrary (ActionImpl ()) where
  arbitrary = do
    v <- arbitrary
    return (FreneticAct v)

