module Main where

import Tests.Frenetic.NetCore.TestCompiler
import Tests.Frenetic.NetCore.TestNetCore
import Tests.Frenetic.Switches.TestSwitches
import Tests.Frenetic.Slices.TestCompile
import Tests.Frenetic.Slices.TestSlice
import Tests.Frenetic.TestCompat
import Test.HUnit
import Test.Framework

main = defaultMain
  [ compilerTests
  , netCoreTests
  , switchTests
  , compatTests
  , sliceCompileTests
  , sliceTests
  ]
