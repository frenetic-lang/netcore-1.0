{-# LANGUAGE
    TemplateHaskell
 #-}

import Test.Framework
import Test.Framework.Providers.QuickCheck2

main = defaultMain tests

tests = [ testGroup "Simple Properties" [
            testProperty "reflInt" prop_reflInt ]]

prop_reflInt :: Int -> Bool
prop_reflInt i = i == i

