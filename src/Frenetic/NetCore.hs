-- |Everything necessary to build a controller atop NetCore, using Nettle as
-- a backend.
module Frenetic.NetCore
  ( module Frenetic.Server
  -- * Policies
  , Policy (..)
  , (==>)
  , (<%>)
  , (<+>)
  -- * Predicates
  , Predicate (..)
  , Pattern (..)
  , Wildcard (..)
  , Prefix (..)
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
  , Action (..)
  , Query (..)
  , Modification (..)
  , unmodified
  , query
  , pktQuery
  , isPktQuery
  , dropPkt
  , allPorts
  , forward
  , modify
  , actionForwardsTo
  , unionAction
  -- * Network Elements
  , Switch
  , Port
  , Vlan
  , Loc (..)
  , PseudoPort (..)
  , Word48
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
  ) where

import Frenetic.Server
import Frenetic.NetCore.Types
import Frenetic.NetCore.Short
import Frenetic.Pattern
