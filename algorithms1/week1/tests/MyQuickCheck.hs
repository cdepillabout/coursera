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
import Test.Tasty.QuickCheck (Arbitrary(..), elements, Property, testProperty, (==>))
import qualified Test.Framework as Framework
import qualified Test.Framework.Providers.QuickCheck2 as FrameworkQC2

import Test.QuickCheck (Gen)

--qcProps :: TestTree
--qcProps = testGroup "(checked by QuickCheck)"
--  [ testProperty "sort == sort . reverse" $
--      \list -> sort (list :: [Int]) == sort (reverse list) ]

wquElementsList :: [Int]
wquElementsList = [1..50]

percolationElementsList :: [Int]
percolationElementsList = [1..100]

instance Arbitrary WeightedQuickUnion where
        arbitrary = do
            n <- elements wquElementsList
            return $ newWeightedQuickUnion n

instance Arbitrary (WeightedQuickUnion, Index, Index) where
        arbitrary = do
            n <- elements wquElementsList
            let wqu = newWeightedQuickUnion n
            indexA <- elements [1..n]
            --indexA <- elements [1..100]
            indexB <- elements [1..n]
            return (wqu, indexA, indexB)

instance Arbitrary (WeightedQuickUnion, Index) where
        arbitrary = do
            n <- elements wquElementsList
            let wqu = newWeightedQuickUnion n
            index <- elements [1..n]
            return (wqu, index)

instance Arbitrary (WeightedQuickUnion, Index, Index, Index) where
        arbitrary = do
            n <- elements wquElementsList
            let wqu = newWeightedQuickUnion n
            indexA <- elements [1..n]
            indexB <- elements [1..n]
            indexC <- elements [1..n]
            return (wqu, indexA, indexB, indexC)

instance Arbitrary Percolation where
        arbitrary = do
            n <- elements percolationElementsList
            return $ newPercolation n

instance Arbitrary (Percolation, [(X, Y)]) where
        arbitrary = do
            n <- elements percolationElementsList
            --return $ newPercolation n
            undefined
          where
            allXYs :: Int -> Gen (X, Y)
            allXYs n = elements $ map (\(x:y:[])-> toXY x y) $ sequence $ [[1..n], [1..n]]

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

noTwoPointsAreConnectedBeforeUnion :: (WeightedQuickUnion, Index, Index) -> Property
noTwoPointsAreConnectedBeforeUnion (wqu, indexA, indexB) =
        (indexA /= indexB) ==> not $ isConnected wqu indexA indexB

everyPointIsConnectedToItself :: (WeightedQuickUnion, Index) -> Bool
everyPointIsConnectedToItself (wqu, index) = isConnected wqu index index

wquCorrectSize :: Int -> Property
wquCorrectSize int = not (int < 1) ==> countWqu (newWeightedQuickUnion int) == int

connectedAfterTwoUnions :: (WeightedQuickUnion, Index, Index, Index) -> Bool
connectedAfterTwoUnions (wqu, indexA, indexB, indexC) =
        let wqu' = union wqu indexA indexB
            wqu'' = union wqu' indexB indexC
        in isConnected wqu'' indexA indexC

newPercolationsNeverPercolate :: Percolation -> Bool
newPercolationsNeverPercolate perc = not $ doesPercolate perc

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
    , FrameworkQC2.testProperty "no two points are connected before union" noTwoPointsAreConnectedBeforeUnion
    , FrameworkQC2.testProperty "every point is connected to itself" everyPointIsConnectedToItself
    , FrameworkQC2.testProperty "two things unioned together twice are connected" connectedAfterTwoUnions
    , FrameworkQC2.testProperty "new percolations never percolate" connectedAfterTwoUnions
    ]
