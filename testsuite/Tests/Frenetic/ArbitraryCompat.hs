{-# LANGUAGE
    TypeSynonymInstances,
    TemplateHaskell,
    MultiParamTypeClasses,
    FlexibleInstances
 #-}

module Tests.Frenetic.ArbitraryCompat where

import Frenetic.NetCore.Semantics
import Frenetic.NetCore.Types
import qualified Data.Set as Set
import Data.Word
import Data.Bits
import Frenetic.Compat
import Frenetic.LargeWord
import Frenetic.Pattern
import Test.QuickCheck
import Tests.Frenetic.ArbitraryPattern
import Frenetic.Switches.OpenFlow
import Frenetic.NetCore

buildWord48 w1 w2 w3 w4 w5 w6 =
  LargeKey
    w1
    (LargeKey w2 (LargeKey w3 (LargeKey w4 (LargeKey w5 w6))))

instance Arbitrary EthernetAddress where
  arbitrary = do
    w64 <- arbitrary
    return (ethernetAddress64 w64)

  shrink eth = map ethernetAddress64 (shrink (unpackEth64 eth))

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
    oneof [ return $ allPorts unmodified,
            do ports <- listOf arbitrary
               return $ foldr (<+>) dropPkt (map forward ports)
          ]

instance Arbitrary (PatternImpl ()) where
  arbitrary = do
    v <- arbitrary
    return (FreneticPat v)

instance Arbitrary (ActionImpl ()) where
  arbitrary = do
    v <- arbitrary
    return (FreneticAct v)

