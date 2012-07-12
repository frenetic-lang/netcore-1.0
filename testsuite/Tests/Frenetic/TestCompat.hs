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
-- /testsuite/Frenetic/TestCompat                                             --
--                                                                            --
-- $Id$ --
--------------------------------------------------------------------------------

module Tests.Frenetic.TestCompat where

import Data.Word
import Data.Bits
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Control.Newtype.TH
import Control.Newtype
import Frenetic.Compat
import Frenetic.Pattern
import Tests.Frenetic.ArbitraryCompat
import Tests.Frenetic.ArbitraryPattern
import Frenetic.LargeWord
import Frenetic.NetCore.API

compatTests = $(testGroupGenerator)

prop_ExactishWildcard_is_exactish :: ExactishWildcard Word8 -> Bool
prop_ExactishWildcard_is_exactish w_in = let w@(Wildcard x m) = unW w_in in
    w == top || m == (complement 0)

prop_ExactishPattern_is_exactish :: ExactishPattern -> Bool
prop_ExactishPattern_is_exactish p_in =
  let p = unpack p_in
      fields = [
          f $ ptrnDlSrc p
        , f $ ptrnDlDst p
        , f $ ptrnDlTyp p
        , f $ ptrnDlVlan p
        , f $ ptrnDlVlanPcp p
        , f $ ptrnNwSrc p
        , f $ ptrnNwDst p
        , f $ ptrnNwProto p
        , f $ ptrnNwTos p
        , f $ ptrnTpSrc p
        , f $ ptrnTpDst p
        ]
  in foldl (&&) True fields
    where
      f :: (Eq a, Bits a, Num a) => Wildcard a -> Bool
      f w@(Wildcard x m) = (w == top || m == (complement 0))

