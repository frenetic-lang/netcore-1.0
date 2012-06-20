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
-- /testsuite/Frenetic/ArbitraryAPI                                           --
--                                                                            --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE
    TypeSynonymInstances
 #-}

module Tests.Frenetic.NetCore.ArbitraryAPI where
import Data.Set                                 as Set

import Frenetic.NetCore.API
import Frenetic.Compat
import Tests.Frenetic.ArbitraryCompat

import Frenetic.LargeWord
import Test.QuickCheck

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
            -- TODO: return $ PrDifference p1 p2,
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
  shrink (PrDifference p1 p2)  = [p1, p2]
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
          p1   <- resize (s-1) arbitrary
          p2   <- resize (s-1) arbitrary
          oneof [ return $ PoBasic pred acts
                  , return $ PoUnion p1 p2
                  , return $ PoIntersect p1 p2
                  -- TODO: NYI 
                  -- , return PoUnknown
                  -- , return $ PoDifference p1 p2
                 ]
        else do
          pred <- resize s arbitrary
          acts <- resize s arbitrary
          oneof [ return $ PoBasic pred acts
                  -- TODO: NYI: 
                  -- , return PoUnknown 
                  ]

  shrink (PoBasic pr as)        = [PoBasic pr' as | pr' <- shrink pr]
  shrink (PoUnknown)            = []
  shrink (PoUnion p1 p2)        = [p1, p2]
  shrink (PoIntersect p1 p2)    = [p1, p2]
  shrink (PoDifference p1 p2)   = [p1, p2]

