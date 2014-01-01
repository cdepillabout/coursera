
module QuickCheckTests ( quickCheckTests
                       )
                       where

import EightPuzzle

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property)

someTest :: Bool
someTest = 1 == 2

quickCheckTests :: [Test]
quickCheckTests =
    [ testProperty "need to create a QuickCheck test" someTest
    ]

