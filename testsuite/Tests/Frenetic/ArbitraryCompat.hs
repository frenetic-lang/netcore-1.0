{-# LANGUAGE
    TypeSynonymInstances,
    TemplateHaskell,
    MultiParamTypeClasses,
    FlexibleInstances
 #-}

module Tests.Frenetic.ArbitraryCompat where

import Control.Monad
import Frenetic.NetCore.Semantics
import Frenetic.NetCore.Types
import qualified Data.Set as Set
import Data.Word
import Data.Bits
import Frenetic.Compat
import Frenetic.Pattern
import Test.QuickCheck
import qualified Data.MultiSet as MS
import Frenetic.Switches.OpenFlow
import Frenetic.NetCore

buildWord48 = ethernetAddress

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

instance Arbitrary PseudoPort where
  arbitrary = do
    oneof [ return AllPorts
          ,  do port <- arbitrary
                return (Physical port)
          ]

instance Arbitrary Action where
  arbitrary = do
    -- TODO(arjun): queries and modifications
    p <- arbitrary
    return (Forward p unmodified)

instance Arbitrary ActionImpl where
  arbitrary = do
    acts <- arbitrary
    return (actnTranslate (MS.fromList acts))
