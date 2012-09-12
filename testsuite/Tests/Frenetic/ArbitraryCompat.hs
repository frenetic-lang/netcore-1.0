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
import Frenetic.Pattern
import Test.QuickCheck
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

instance Arbitrary Loc where
  arbitrary = 
    liftM2 Loc arbitrary arbitrary

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
  arbitrary = liftM actnTranslate arbitrary
