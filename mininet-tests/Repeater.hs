module Repeater where

import Frenetic.NetCore

policy = matchAll ==> allPorts unmodified

main = controller policy
