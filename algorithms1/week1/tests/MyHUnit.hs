module MyHUnit ( frameworkUnitTests
               )
               where

import Percolation
import PercolationStats
import WeightedQuickUnion

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))

n1percolationsPercolate :: Assertion
n1percolationsPercolate = let perc = newPercolation 1
                              perc' = open perc (toXY 1 1)
                          in doesPercolate perc' @?= True


n2percolationsPercolate :: Assertion
n2percolationsPercolate = do
        let perc = newPercolation 2
            perc' = open perc (toXY 1 1)
            perc'' = open perc' (toXY 2 1)
        doesPercolate perc'' @?= True

frameworkUnitTests :: [Test]
frameworkUnitTests =
    [ testCase "a 1-size percolation with the only cell opened does percolate"
        n1percolationsPercolate
    , testCase "a 2-size percolation with the cell opened on top and bottom does percolate"
        n2percolationsPercolate
    --, FrameworkQC2.testProperty "find and union work correctly" findShowsCorrectUnionAfterBeingUnioned
    ]
