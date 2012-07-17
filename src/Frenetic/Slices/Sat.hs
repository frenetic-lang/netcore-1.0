module Frenetic.Slices.Sat
  ( simulatesForwards
  ) where

import Frenetic.Z3
import Frenetic.Sat
import Frenetic.Slices.Slice
import Frenetic.NetCore.API

simulatesForwards :: Slice -> Policy -> Policy -> IO (Maybe String)
simulatesForwards slice a b = check $ Input setUp consts assertions
  where
    p = Z3Packet "p"
    p' = Z3Packet "pp"
    v = Z3Int "v"
    v' = Z3Int "vv"

    consts = [ DeclConst (ConstPacket p)
             , DeclConst (ConstPacket p')
             , DeclConst (ConstInt v)
             , DeclConst (ConstInt v')
             ]

    assertions = [ inSlice slice p
                 , inSlice slice p'
                 , forwards a p p'
                 , ForAll [ConstInt v, ConstInt v']
                          (Not (forwardsWith b (p, Just v) (p', Just v')))
                 ]
