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
-- /src/Frenetic/NetStream/FRP.hs                                             --
--                                                                            --
-- $Id$ --
--------------------------------------------------------------------------------


{-# LANGUAGE Arrows #-}

module Frenetic.NetStream.FRP where

import FRP.Animas
import Frenetic.NetStream.API

type FreneticA a b = SF a b

-- TODO: fill in
type Headers = Int

query :: (ValidTransmission ptrn pkt) => Headers -> Predicate -> FreneticA () (Event (Transmission ptrn pkt))
query = undefined

-- Just trying to get some type signatures for now. You can feel free to add more combinators you feel should be in the language. Read the Animas docs!