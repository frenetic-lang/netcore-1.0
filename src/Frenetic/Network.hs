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
-- /src/Network.hs                                                            --
-- Frenetic network types                                                     --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE 
    NoMonomorphismRestriction,
    ExistentialQuantification,
    MultiParamTypeClasses,
    StandaloneDeriving,
    GADTs
 #-}

module Frenetic.Network where

import Numeric
import Data.Char 
import Data.Word
import Data.LargeWord

--
-- Basic types
-- 
type Switch = Word64
type Port = Word16

type Word48 = LargeKey Word8 (LargeKey Word8 (LargeKey Word8 (LargeKey Word8 (LargeKey Word8 Word8)))) 
    
-- instance Show HardwareAddress where
--   show (HardwareAddress (EthernetAddress w1 w2 w3 w4 w5 w6)) = 
--     let hex x = (if x < 10 then "0" else "") ++ showHex x "" in 
--     (hex w1) ++ ":" ++ (hex w2) ++ ":" ++ (hex w3) ++ ":" ++ 
--     (hex w4) ++ ":" ++ (hex w5) ++ ":" ++ (hex w6)

-- 
-- Headers
-- 
data Header b where
  Dl_src :: Header Word48
  Dl_dst :: Header Word48
  Dl_typ :: Header Word16
  Dl_vlan :: Header Word16
  Dl_vlan_pcp :: Header Word8
  Nw_src :: Header Word32
  Nw_dst :: Header Word32
  Nw_proto :: Header Word8
  Nw_tos :: Header Word8
  Tp_src :: Header Word16
  Tp_dst :: Header Word16

deriving instance Eq b => Eq (Header b)
deriving instance Show b => Show (Header b)
deriving instance Ord b => Ord (Header b)

--
-- Packets
--
class (Eq p) => Packet p where
  getHeader :: p -> Header b -> b
  setHeader :: p -> Header b -> b -> p

--
-- Transmissions
--
data Transmission p = forall p. (Packet p) => Transmission Switch Port p
