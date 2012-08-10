module Tests.Frenetic.ArbitraryPattern where
import Data.Bits
import Frenetic.Pattern
import Test.QuickCheck

instance (Arbitrary a) => Arbitrary (Wildcard a) where
  arbitrary = oneof [ do { v <- arbitrary; return (Exact v) }
                    , return Wildcard
                    ]

  shrink (Exact v) = [Exact v' | v' <- shrink v]
  shrink Wildcard  = [Wildcard]

instance (Num a, Bits a, Arbitrary a) => Arbitrary (Prefix a) where
  arbitrary = do
    val <- arbitrary
    len <- oneof (map return [ 0 .. bitSize val ])
    return (Prefix val len)

  shrink v = [v]
