-- |Everything necessary to build a controller atop NetCore, using Nettle as
-- a backend.
module Frenetic.NetCore
  ( -- * OpenFlow Controllers
    controller
  , dynController
  -- * Policies
  , Policy (..)
  , (==>)
  , (<%>)
  , (<+>)
  -- * Predicates
  , Predicate
  , exactMatch
  , inport
  , (<||>)
  , (<&&>)
  , matchAll
  , matchNone
  , neg
  , prSubtract
  , prOr
  , prAnd
  -- ** Exact match predicate constructors
  , onSwitch
  , dlSrc
  , dlDst
  , dlTyp
  , dlVlan
  , dlNoVlan
  , dlVlanPcp
  , nwSrc
  , nwDst
  , nwSrcPrefix
  , nwDstPrefix
  , nwProto
  , nwTos
  , tpSrc
  , tpDst
  , inPort
  -- * Actions
  , Action
  -- ** Constructors
  , dropPkt
  , forward
  , allPorts
  , modify
  , countBytes
  , countPkts
  , getPkts
  -- ** Modifications
  , Modification
  , unmodified
  -- * Network Elements
  , Switch
  , Port
  , Vlan
  , Loc (..)
  , Word48
  , broadcastAddress
  , EthernetAddress
  , IPAddr
  , ipAddr
  , unpackIPAddr
  -- * Packets
  , Packet (..)
  -- * Packet modifications
  , modDlSrc
  , modDlDst
  , modDlVlan
  , modDlVlanPcp
  , modNwSrc
  , modNwDst
  , modNwTos
  , modTpSrc
  , modTpDst
  -- * Channels
  , select
  , both
  -- * Slices
  , Slice(..)
  -- ** Topology constructors
  , Topo
  , buildGraph
  -- ** Slice constructors
  , internalSlice
  , simpleSlice
  -- ** Compilation
  , transform
  , transformEdge
  , dynTransform
  ) where

import Frenetic.Common
import Frenetic.NetCore.Types
import Frenetic.NetCore.Short
import Frenetic.Pattern
import Frenetic.Server
import Frenetic.Slices.Compile
import Frenetic.Slices.Slice
import Frenetic.Topo
