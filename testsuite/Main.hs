module Main where

import Tests.Frenetic.NetCore.TestCompiler
import Tests.Frenetic.NetCore.TestNetCore
import Tests.Frenetic.Slices.TestCompile
import Tests.Frenetic.Slices.TestSlice
import Tests.Frenetic.Slices.TestVerification
import Tests.Frenetic.Switches.TestSwitches
import Tests.Frenetic.TestCompat
import Tests.Frenetic.TestSat
import Test.HUnit
import Test.Framework

main = defaultMain
  [ compilerTests
  , netCoreTests
  , switchTests
  , compatTests
  , satTests
  , sliceCompileTests
  , sliceTests
  , sliceVerificationTests
  ]
