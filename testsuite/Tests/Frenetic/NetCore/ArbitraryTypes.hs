{-# LANGUAGE
    TypeSynonymInstances
 #-}

module Tests.Frenetic.NetCore.ArbitraryTypes where

import Data.Set                                 as Set
import Control.Monad
import Frenetic.NetCore.Types
import Frenetic.Compat
import Tests.Frenetic.ArbitraryCompat
import Tests.Frenetic.Switches.ArbitraryOpenFlow
import Test.QuickCheck

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
          , liftM IngressPort arbitrary
          , liftM Switch arbitrary
          , return None
          , return Any
          ]
        large =
          [ liftM2 Or (resize (s-1) arbitrary) (resize (s-1) arbitrary)
          , liftM2 And (resize (s-1) arbitrary) (resize (s-1) arbitrary)
          , liftM Not (resize (s-1) arbitrary)
          ]
      in if s == 0 then oneof small else oneof (small ++ large)

{-
  shrink (PrPattern p)         = [PrPattern p' | p' <- shrink p]
  shrink (Switch s)              = []
  shrink (Or p1 p2)       = [p1, p2]
  shrink (And p1 p2)   = [p1, p2]
  shrink (Not p)          = [Not p' | p' <- shrink p]
-}

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
