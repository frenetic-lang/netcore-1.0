
-- Eventually take this stuff and put it into test-framework

module Frenetic.Playground where

import Frenetic.Switches.OpenFlow

import Frenetic.Pattern
import Frenetic.Language
import Frenetic.Compiler

import qualified Data.Set as Set

test1_pol = PrPattern (top { ptrnInPort = Just 1 }) `PoBasic` Set.singleton 1

test1_cls :: OFClassifier
test1_cls = compile 1 test1_pol

test2_pol = (PrPattern (top { ptrnDlSrc = wMake "1?" }) `PoBasic` Set.singleton 2)

test2_cls :: OFClassifier
test2_cls = compile 1 test2_pol

test3_pat =  (top { ptrnNwSrc = wMake "1?" })

test3_pol = (PrPattern (top { ptrnNwSrc = wMake "1?" }) `PoBasic` Set.singleton 3)

test3_cls :: OFClassifier
test3_cls = compile 1 test3_pol

