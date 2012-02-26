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
-- /src/Frenetic/NetStream/Compiler.hs                                        --
--                                                                            --
-- $Id$ --
--------------------------------------------------------------------------------


module Frenetic.NetStream.Compiler where

import Frenetic.NetStream.API

{-| Add additional structural information to a predicate. -}
refinePredicate :: forall ptrn pkt. (ValidTransmission ptrn pkt) =>
                   Transmission ptrn pkt -> Predicate -> Predicate
refinePredicate tr (PrPattern ptrn) =
  case (fromPatternUnderapprox  (toPacket $ trPkt tr)  ptrn :: Maybe ptrn) of
      Just pat -> PrUnion (PrPattern ptrn) (PrSwitchPattern (show pat) (toDyn pat))
      Nothing -> PrPattern ptrn
refinePredicate tr (PrSwitchPattern s dyn) = PrSwitchPattern s dyn
refinePredicate tr (PrHint (Tag i)) = undefined -- FIX need more info
refinePredicate tr (PrTo s) = PrTo s
refinePredicate tr (PrUnion pr1 pr2) =
    PrUnion (refinePredicate tr pr1) (refinePredicate tr pr2)
refinePredicate tr (PrIntersect pr1 pr2) =
    PrIntersect (refinePredicate tr pr1) (refinePredicate tr pr2)
refinePredicate tr (PrDifference pr1 pr2) =
    PrDifference (refinePredicate tr pr1) (refinePredicate tr pr2)
refinePredicate tr (PrNegate pr) = PrNegate (refinePredicate tr pr)

{-| Add additioanl structural information to a policy. -}
refinePolicy :: (ValidTransmission ptrn pkt) =>
                Transmission ptrn pkt -> Policy -> Policy
refinePolicy tr (PoBasic pr as) = PoBasic (refinePredicate tr pr) as
refinePolicy tr (PrHint (Tag i)) = undefined -- FIX need more info
refinePolicy tr (PoUnion po1 po2) = PoUnion (refinePolicy tr po1) (refinePolicy tr po2)
refinePolicy tr (PoIntersect po1 po2) = PoIntersect (refinePolicy tr po1) (refinePolicy tr po2)
refinePolicy tr (PoDifference po1 po2) = PoDifference (refinePolicy tr po1) (refinePolicy tr po2)
