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
-- /src/Patterns.hs                                                            --
-- Patterns                                                           --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE 
    NoMonomorphismRestriction, 
    ExistentialQuantification,
    ImpredicativeTypes,
    ScopedTypeVariables,
    FlexibleInstances,
    TypeSynonymInstances, 
    GADTs
 #-}

module Frenetic.Patterns where

import System.IO.Unsafe
import System.IO
import Data.Bits
import Data.Map as Map
import Data.Set as Set
import Data.Word
import Data.List as List

import Frenetic.Network 
import Frenetic.Language 
import Nettle.OpenFlow.Match as OFMatch
import Nettle.OpenFlow.Action as OFAction
import Nettle.IPv4.IPAddress as IPAddress 
import Nettle.Ethernet.EthernetAddress 

-- Patterns
--

class MatchElement a where
  mTop :: a
  mMeet :: a -> a -> Maybe a

instance (Eq a) => MatchElement (Maybe a) where
  mTop = Nothing
  mMeet x Nothing = Just x
  mMeet Nothing x = Just x
  mMeet m1 m2 | m1 == m2 = Just m1
              | otherwise = Nothing

instance MatchElement IPAddressPrefix where
  mTop = defaultIPPrefix
  mMeet = IPAddress.intersect

