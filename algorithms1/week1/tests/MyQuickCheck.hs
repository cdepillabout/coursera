{-# Language FlexibleInstances, OverlappingInstances #-}

module MyQuickCheck ( tastyproperties
                    , frameworkproperties
                    , wquCorrectSize
                    , findShowsCorrectUnionAfterBeingUnioned
                    , findShowsCorrectUnionAfterBeingUnionedBad
                    )
                    where

import Percolation
import PercolationStats
import WeightedQuickUnion

import qualified Test.Tasty as Tasty
import Test.Tasty.QuickCheck (Arbitrary(..), elements, Positive, Property, testProperty, (==>))
import qualified Test.Framework as Framework
import qualified Test.Framework.Providers.QuickCheck2 as FrameworkQC2

--qcProps :: TestTree
--qcProps = testGroup "(checked by QuickCheck)"
--  [ testProperty "sort == sort . reverse" $
--      \list -> sort (list :: [Int]) == sort (reverse list) ]

test_list = [1..50]

instance Arbitrary WeightedQuickUnion where
        arbitrary = do
            n <- elements test_list
            return $ newWeightedQuickUnion n

instance Arbitrary (WeightedQuickUnion, Index, Index) where
        arbitrary = do
            n <- elements test_list
            let wqu = newWeightedQuickUnion n
            indexA <- elements [1..n]
            --indexA <- elements [1..100]
            indexB <- elements [1..n]
            return (wqu, indexA, indexB)

findShowsCorrectUnionAfterBeingUnioned :: WeightedQuickUnion -> Int -> Int -> Property
findShowsCorrectUnionAfterBeingUnioned wqu intA intB =
        -- \wqu intA intB -> let count = countWqu wqu in
        let count = countWqu wqu in
            ((intA :: Int) /= (intB :: Int)) ==>
            if (intA >= 1) && (intA < count) && (intB >= 1) && (intB < count)
                then let wquAfterUnion = union wqu intA intB
                     in isConnected wquAfterUnion intA intB
                else True

findShowsCorrectUnionAfterBeingUnionedBad :: (WeightedQuickUnion, Index, Index) -> Bool
findShowsCorrectUnionAfterBeingUnionedBad (wqu, indexA, indexB) =
        --(indexA /= indexB) ==>
        let wquAfterUnion = union wqu indexA indexB
        in isConnected wquAfterUnion indexA indexB

wquCorrectSize :: Int -> Property
wquCorrectSize int = not (int < 1) ==> countWqu (newWeightedQuickUnion int) == int

tastyproperties :: Tasty.TestTree
tastyproperties = Tasty.testGroup "Properties"
    [ testProperty "create a WeightedQuickUnion of the right size" wquCorrectSize
    , testProperty "find and union work correctly" findShowsCorrectUnionAfterBeingUnioned
    , testProperty "find and union work correctly bad" findShowsCorrectUnionAfterBeingUnionedBad
    ]

frameworkproperties :: [Framework.Test]
frameworkproperties = 
    [ FrameworkQC2.testProperty "create a WeightedQuickUnion of the right size" wquCorrectSize
    , FrameworkQC2.testProperty "find and union work correctly" findShowsCorrectUnionAfterBeingUnioned
    , FrameworkQC2.testProperty "find and union work correctly bad" findShowsCorrectUnionAfterBeingUnionedBad
    ]
