module Repeater where

import Frenetic.NetCore

policy = Any ==> allPorts unmodified

main = controller policy
