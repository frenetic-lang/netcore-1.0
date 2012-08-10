{-# LANGUAGE
    TypeSynonymInstances
 #-}

module Tests.Frenetic.NetCore.ArbitraryTypes where
import Data.Set                                 as Set

import Frenetic.NetCore.Types
import Frenetic.Compat
import Tests.Frenetic.ArbitraryCompat

import Test.QuickCheck

instance Arbitrary IPAddr where
  arbitrary = do
    w <- arbitrary
    return (IPAddr w)

instance Arbitrary Predicate where
  -- Bound the size of the predicate
  arbitrary = sized $ \s -> if s > 0
    then do
      pat <- arbitrary
      sw  <- arbitrary
      p1  <- resize (s-1) arbitrary
      p2  <- resize (s-1) arbitrary
      oneof [ return $ PrPattern pat,
            return $ PrTo sw,
            return $ PrUnion p1 p2,
            return $ PrIntersect p1 p2,
            return $ PrNegate $ p1 ]
    else do
      pat <- arbitrary
      sw  <- arbitrary
      oneof [ return $ PrPattern pat,
            return $ PrTo sw ]

  shrink (PrPattern p)         = [PrPattern p' | p' <- shrink p]
  shrink (PrTo s)              = []
  shrink (PrUnion p1 p2)       = [p1, p2]
  shrink (PrIntersect p1 p2)   = [p1, p2]
  shrink (PrNegate p)          = [PrNegate p' | p' <- shrink p]

instance Arbitrary Policy where
  -- Bound the size of the policy
  arbitrary = sized $ \s -> let depth = 10 in
    if s > depth
      then resize depth arbitrary
      else if s > 0
        then do
          pred <- resize s arbitrary
          acts <- resize s arbitrary
          --p1   <- resize (s-1) arbitrary
          --p2   <- resize (s-1) arbitrary
          oneof [ return $ PoBasic pred acts
                --, return $ PoUnion p1 p2
                  -- TODO(arjun): test putting union into here and un-comment
                  -- code
                ]
        else do
          pred <- resize s arbitrary
          acts <- resize s arbitrary
          return $ PoBasic pred acts
  shrink (PoBottom)             = [PoBottom]
  shrink (PoBasic pr as)        = [PoBasic pr' as | pr' <- shrink pr]
  shrink (PoUnion p1 p2)        = [p1, p2]
