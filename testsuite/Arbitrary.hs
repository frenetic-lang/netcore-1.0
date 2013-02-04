module Arbitrary where

import Test.QuickCheck
import qualified Data.ByteString.Lazy as BS
import Frenetic.Common
import Control.Monad
import Nettle.Arbitrary
import Frenetic.NetCore.Types hiding (Switch)
import qualified Frenetic.NetCore.Types as NetCore
import Frenetic.NetCore.Semantics
import Frenetic.Topo

arbPort :: Gen Port
arbPort = oneof [ return n | n <- [1 .. 5] ]

arbSwitch :: Gen Switch
arbSwitch = oneof [ return n | n <- [1 .. 10] ]

arbGenPktId :: Gen Id
arbGenPktId = oneof [ return 900, return 901, return 902 ]

arbCountersId :: Gen Id
arbCountersId = oneof [ return 500, return 501, return 502 ]

arbGetPktId :: Gen Id
arbGetPktId = oneof [ return 600, return 601, return 602 ]

arbMonSwitchId :: Gen Id
arbMonSwitchId = oneof [ return 700, return 701, return 702 ]

arbFwd :: Gen Act
arbFwd = liftM2 ActFwd arbitrary arbitrary

arbFwds :: Gen [Act]
arbFwds = oneof [ liftM2 (:) arbFwd arbFwds, return [] ]

arbInterval :: Gen Int
arbInterval = oneof [ return (-1), return 0, return 1, return 30 ]

instance Arbitrary Act where
  arbitrary = oneof
    [ arbFwd
    , liftM2 ActQueryPktCounter arbCountersId arbInterval
    , liftM2 ActQueryByteCounter arbCountersId arbInterval
    , liftM ActGetPkt arbGetPktId
    , liftM ActMonSwitch arbMonSwitchId
    ]


instance Arbitrary Modification where
  arbitrary = sized $ \s -> frequency [ (2, return unmodified), (1, mod s) ]
    where mod s = do
            a1 <- if s < 1 then return Nothing else arbitrary
            a2 <- if s < 2 then return Nothing else arbitrary
            a3 <- if s < 3 then return Nothing else arbitrary
            a4 <- if s < 4 then return Nothing else arbitrary
            a5 <- if s < 5 then return Nothing else arbitrary
            a6 <- if s < 6 then return Nothing else arbitrary
            a7 <- if s < 7 then return Nothing else arbitrary
            a8 <- if s < 8 then return Nothing else arbitrary
            a9 <- if s < 9 then return Nothing else arbitrary
            return (Modification a1 a2 a3 a4 a5 a6 a7 a8 a9)

instance Arbitrary Pol where
  arbitrary = sized $ \s -> case s of
    0 -> return PolEmpty
    otherwise -> oneof 
      [ liftM2 PolProcessIn (resize (s-1) arbitrary) (resize (s-1) arbitrary)
      , liftM2 PolUnion (resize (s-1) arbitrary) (resize (s-1) arbitrary)
      -- , liftM2 PolSeq (resize (s-1) arbitrary) (resize (s-1) arbitrary)
      , liftM2 PolRestrict (resize (s-1) arbitrary) (resize (s-1) arbitrary)
      , liftM PolGenPacket (resize (s-1) arbGenPktId)
      ]

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
          , liftM NetCore.Switch arbSwitch
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
