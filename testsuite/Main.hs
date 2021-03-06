module Main where

import Data.List
import System.Environment
import qualified Semantics
import Tests.Frenetic.Slices.TestCompile
import Tests.Frenetic.Slices.TestEndToEnd
import Tests.Frenetic.Slices.TestSlice
import Tests.Frenetic.Slices.TestVerification
import Tests.Frenetic.Slices.TestVlanAssignment
import Tests.Frenetic.TestSat
import Test.HUnit
import Test.Framework

main = do
  args <- getArgs
  let sat = elem "sat" args
  let ourTests = if sat then satTestGroup : mainTests else mainTests
  let args' = if sat then delete "sat" args else args
  defaultMainWithArgs ourTests args'

mainTests =
  [ Semantics.tests
  , testGroup "Slice tests" [ sliceCompileTests
                            , sliceTests
                            , sliceVerificationTests
                            , vlanAssignmentTests
                            ]
  ]

satTestGroup = testGroup "SAT tests" [
                                       satTests
                                     , endToEndTests
                                     ]
