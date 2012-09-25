module Arbitrary where

import Test.QuickCheck
import qualified Data.ByteString.Lazy as BS
import Frenetic.Common
import Control.Monad
import Nettle.Arbitrary
import Frenetic.NetCore.Types
import Frenetic.NetCore.Semantics

arbPort :: Gen Port
arbPort = oneof [ return n | n <- [1 .. 5] ]

arbSwitch :: Gen Switch
arbSwitch = oneof [ return n | n <- [1 .. 10] ]

arbGenPktId :: Gen Id
arbGenPktId = oneof [ return 900, return 901, return 902 ]

arbCountersId :: Gen Id
arbCountersId = oneof [ return 500, return 501, return 502 ]

instance Arbitrary Predicate where
  arbitrary = sized $ \s -> 
    let small = 
          [ liftM DlSrc arbitrary
          , liftM DlDst arbitrary
          , liftM DlTyp arbitrary
          , liftM DlVlan arbitrary
          , liftM DlVlanPcp arbitrary
          , liftM NwSrc arbitrary
          , liftM NwDst arbitrary
          , liftM NwProto arbitrary
          , liftM NwTos arbitrary
          , liftM TpSrcPort arbitrary
          , liftM TpDstPort arbitrary 
          , liftM IngressPort arbPort
          , liftM Switch arbSwitch
          , return None
          , return Any
          ]
        large =
          [ liftM2 Or (resize (s-1) arbitrary) (resize (s-1) arbitrary)
          , liftM2 And (resize (s-1) arbitrary) (resize (s-1) arbitrary)
          , liftM Not (resize (s-1) arbitrary)
          ]
      in if s == 0 then oneof small else oneof (small ++ large)

instance Arbitrary Loc where
  arbitrary = liftM2 Loc arbSwitch arbPort

expectsNwFields :: Word16 -> Bool
expectsNwFields 0x800 = True
expectsNwFields 0x806 = True
expectsNwFields _ = False

-- Does not generate ToQueue
instance Arbitrary PseudoPort where
  arbitrary = frequency [ (5, liftM Physical arbPort)
                        , (1, return AllPorts)
                        ]

instance Arbitrary ByteString where
  arbitrary = return BS.empty

-- This isn't perfect, but we attempt to build a reasonable set of headers.
-- We favors building IP and ARP packets and fill out the Maybe-typed nw* 
-- fields for IP and ARP packets.
instance Arbitrary Packet where
  arbitrary = do
    dlSrc <- arbitrary
    dlDst <- arbitrary
    -- generate IP and ARP packets most of the time
    dlTyp <- frequency [(10, return 0x800), (5, return 0x806), (1, arbitrary)]
    dlVlan <- arbitrary
    dlVlanPcp <- arbitrary
    let nwArbitrary :: Arbitrary a => Gen (Maybe a)
        -- do not ever use NoMonmorphismRestriction
        nwArbitrary = if expectsNwFields dlTyp then 
                        liftM Just arbitrary 
                      else 
                        return Nothing
    pktNwSrc <- nwArbitrary
    pktNwDst <- nwArbitrary
    pktNwProto <- arbitrary
    pktNwTos <- arbitrary
    pktTpSrc <- nwArbitrary
    pktTpDst <- nwArbitrary
    return (Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp pktNwSrc pktNwDst
                   pktNwProto pktNwTos pktTpSrc pktTpDst)

arbInPkt = liftM3 InPkt arbitrary arbitrary (liftM Just arbitrary)

arbInGenPkt = liftM5 InGenPkt arbGenPktId arbSwitch arbitrary arbitrary
                              arbitrary

arbInCounters = liftM5 InCounters arbCountersId arbSwitch arbitrary
                                  arbitrary arbitrary

-- TODO(arjun): arbInSwitchEvt

instance Arbitrary In where
  arbitrary = oneof [ arbInPkt, arbInGenPkt, arbInCounters ]