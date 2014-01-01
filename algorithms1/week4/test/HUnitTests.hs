
module HUnitTests ( hUnitTests
                  )
                  where

import EightPuzzle

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=), Assertion)

someTest :: Assertion
someTest = do
        let x = 1 
        x @?= 2

hUnitTests :: [Test]
hUnitTests =
    [ testCase "need to create an HUnit test" someTest
    ]


