module Frenetic.NetCore.Short
  ( -- * Exact match pattern constructors
    dlSrc     
  , dlDst     
  , dlTyp     
  , dlVlan    
  , dlVlanPcp 
  , nwSrc     
  , nwDst     
  , nwProto   
  , nwTos     
  , tpSrc     
  , tpDst     
  , inPort    
  ) where

import Data.Word
import Frenetic.Pattern
import Frenetic.NetCore.API

dlSrc     :: Word48     -> Pattern
dlDst     :: Word48     -> Pattern
dlTyp     :: Word16     -> Pattern
dlVlan    :: Word16     -> Pattern
dlVlanPcp :: Word8      -> Pattern
nwSrc     :: Word32     -> Pattern
nwDst     :: Word32     -> Pattern
nwProto   :: Word8      -> Pattern
nwTos     :: Word8      -> Pattern
tpSrc     :: Word16     -> Pattern
tpDst     :: Word16     -> Pattern
inPort    :: Port -> Pattern

dlSrc     value = top {ptrnDlSrc = exact value}
dlDst     value = top {ptrnDlDst = exact value}
dlTyp     value = top {ptrnDlTyp = exact value}
dlVlan    value = top {ptrnDlVlan = exact value}
dlVlanPcp value = top {ptrnDlVlanPcp = exact value}
nwSrc     value = top {ptrnNwSrc = Prefix value 32}
nwDst     value = top {ptrnNwDst = Prefix value 32}
nwProto   value = top {ptrnNwProto = exact value}
nwTos     value = top {ptrnNwTos = exact value}
tpSrc     value = top {ptrnTpSrc = exact value}
tpDst     value = top {ptrnTpDst = exact value}
inPort    value = top {ptrnInPort = Exact value}
