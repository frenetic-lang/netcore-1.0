module Common
  ( module Test.QuickCheck
  , module Test.HUnit
  , module Arbitrary
  , module Text.Printf
  , module Test.Framework
  , module Test.Framework.TH
  , module Test.Framework.Providers.QuickCheck2
  , module Test.Framework.Providers.HUnit
  , assertQuickCheck
  ) where

import Text.Printf
import Test.QuickCheck hiding (classify)
import Test.HUnit hiding (Testable, Test)
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Arbitrary

assertQuickCheck :: Property -> Assertion
assertQuickCheck body = do
  result <- quickCheckResult body
  case result of
    Success _ _ _ -> return ()
    otherwise -> assertFailure ""
