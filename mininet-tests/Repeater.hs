module Repeater where

import Frenetic.NetCore

policy = PoBasic (PrPattern top) flood

main = freneticServer policy  
