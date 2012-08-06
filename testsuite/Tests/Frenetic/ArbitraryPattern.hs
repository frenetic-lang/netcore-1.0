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
-- /testsuite/Frenetic/ArbitraryPattern                                       --
--                                                                            --
-- $Id$ --
--------------------------------------------------------------------------------

module Tests.Frenetic.ArbitraryPattern where
import Data.Bits
import Frenetic.Pattern
import Test.QuickCheck

instance (Arbitrary a) => Arbitrary (Wildcard a) where
  arbitrary = oneof [ do { v <- arbitrary; return (Exact v) }
                    , return Wildcard
                    ]

  shrink (Exact v) = [Exact v' | v' <- shrink v]
  shrink Wildcard  = [Wildcard]

instance (Num a, Bits a, Arbitrary a) => Arbitrary (Prefix a) where
  arbitrary = do
    val <- arbitrary 
    len <- oneof (map return [ 0 .. bitSize val ])
    return (Prefix val len)

  shrink v = [v]
