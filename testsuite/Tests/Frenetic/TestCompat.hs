module Tests.Frenetic.TestCompat where

import Data.Word
import Data.Bits
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Frenetic.Compat
import Frenetic.Pattern
import Tests.Frenetic.ArbitraryCompat
import Tests.Frenetic.ArbitraryPattern
import Frenetic.NetCore.Types

compatTests = $(testGroupGenerator)

