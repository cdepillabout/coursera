
module WeightedQuickUnion ( union
                          , isConnected
                          , find
                          , countWqu
                          , newWeightedQuickUnion
                          , WeightedQuickUnion
                          , Index
                          )
                          where

import Control.Monad.ST (ST)
import Data.Array.ST (newArray, newListArray, readArray, runSTUArray, STUArray, writeArray)
import Data.Array.Unboxed (UArray, (!))
import Data.Array.Unsafe (unsafeThaw)

type Index = Int

type ParentId = Index

type Size = Int

data WeightedQuickUnion = WeightedQuickUnion
                        { wquIds :: UArray Index ParentId
                        , wquSize :: UArray Index Size
                        , wquCount :: Int
                        } deriving (Show)

union :: WeightedQuickUnion -> Int -> Int -> WeightedQuickUnion
union wqu@(WeightedQuickUnion _ sizes n) p q =
    let foundP = find wqu p
        foundQ = find wqu q
    in if foundP == foundQ
           then wqu
           else unionArrayModifier wqu foundP foundQ
  where
    unionArrayModifier :: WeightedQuickUnion -> Int -> Int -> WeightedQuickUnion
    unionArrayModifier (WeightedQuickUnion _ids _sizes _n) foundP foundQ =
        let foundPSize = sizes ! foundP
            foundQSize = sizes ! foundQ
            newSize = foundPSize + foundQSize
            newIds = runSTUArray $ do
                thawedArray <- unsafeThaw _ids :: ST s (STUArray s Index ParentId)
                if foundPSize < foundQSize
                    then writeArray thawedArray foundP foundQ
                    else writeArray thawedArray foundQ foundP
                return thawedArray
            newSizes = runSTUArray $ do
                thawedArray <- unsafeThaw _sizes :: ST s (STUArray s Index Size)
                if foundPSize < foundQSize
                    then writeArray thawedArray foundQ newSize
                    else writeArray thawedArray foundP newSize
                return thawedArray
        in WeightedQuickUnion newIds newSizes (n - 1)

isConnected :: WeightedQuickUnion -> Int -> Int -> Bool
isConnected wqu p q = (find wqu p) == (find wqu q)

find :: WeightedQuickUnion -> Int -> Int
find wqu@(WeightedQuickUnion ids _ _) p = let indexVal = ids ! p in
    if p == indexVal
        then p
        else find wqu indexVal

countWqu :: WeightedQuickUnion -> Int
countWqu wqu = wquCount wqu

newWeightedQuickUnion :: Int -> WeightedQuickUnion
newWeightedQuickUnion n =
        let ids = runSTUArray $ do
                    arr <- newListArray (1, n) [1..] :: ST s (STUArray s Index ParentId)
                    return arr
            sizes = runSTUArray $ do
                    arr <- newArray (1, n) 1 :: ST s (STUArray s Index Size)
                    return arr
        in WeightedQuickUnion ids sizes n
