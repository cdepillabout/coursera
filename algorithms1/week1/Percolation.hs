
module Percolation ( Percolation(..)
                   , X(..)
                   , Y(..)
                   , toXY
                   , xyToActualIndex
                   , actualIndexToXy
                   , open
                   , isOpen
                   , isFull
                   , doesPercolate
                   , newPercolation
                   )
                   where


import Control.Monad.ST (ST)
import Data.Array.ST (newArray, newListArray, readArray, runSTUArray, STUArray, writeArray)
import Data.Array.Unboxed (UArray, (!))
import Data.Array.Unsafe (unsafeThaw)

import WeightedQuickUnion

newtype X = X Int deriving (Show)

newtype Y = Y Int deriving (Show)

data Direction = PercUp
               | PercDown
               | PercLeft
               | PercRight deriving (Show, Eq)

data Percolation = Percolation
                 { percolationWqu :: WeightedQuickUnion
                 , percolationOpenState :: UArray Int Bool
                 , percolationSideLength :: Int -- this starts at 1
                 , percolationTotalLength :: Int -- this starts from 1 and is equal to
                                                 -- sideLength * 2 + 1
                 } deriving (Show)

toXY :: Int -> Int -> (X, Y)
toXY x y = (X x, Y y)

xyLowLevel :: Int -> (X, Y) -> Int
xyLowLevel sideLength (X x, Y y)
    | x < 1 || x > sideLength = error ("X out of range: " ++ show x)
    | y < 1 || y > sideLength = error ("Y out of range: " ++ show y)
    | otherwise = sideLength * (x - 1) + (y - 1) + 3

xyToActualIndex :: Percolation -> (X, Y) -> Int
xyToActualIndex (Percolation _ _ sideLength _) xy =
        xyLowLevel sideLength xy

actualIndexLowLevel :: Int -> Int -> Int -> (X, Y)
actualIndexLowLevel sideLength totalLength actualIndex
    | actualIndex < 3 || actualIndex > totalLength =
        error ("ActualIndex out of range: " ++ show actualIndex)
    | otherwise = let actualIndexFromZero = actualIndex - 3
                      x = (actualIndexFromZero `div` sideLength) + 1
                      y = (actualIndexFromZero `mod` sideLength) + 1
                  in (X x, Y y)

actualIndexToXy :: Percolation -> Int -> (X, Y)
actualIndexToXy (Percolation _ _ sideLength totalLength) actualIndex =
        actualIndexLowLevel sideLength totalLength actualIndex

openActual :: Int -> Percolation -> Percolation
openActual actual' perc' =
        -- first check if it is already open, and if so, don't do anything
        if isOpenActual actual' perc'
            then perc'
            else openActualHelper actual' perc'
  where
    openActualHelper :: Int -> Percolation -> Percolation
    openActualHelper actual (Percolation wqu openStates sideLength totalLength) =
        -- first set it to open
        let newOpenStates = runSTUArray $ do
                thawedOpenStates <- unsafeThaw openStates :: ST s (STUArray s Int Bool)
                writeArray thawedOpenStates actual True
                return thawedOpenStates
            newWqu = foldl (tryConnectTo openStates sideLength totalLength actual)
                           wqu
                           [PercUp, PercDown, PercLeft, PercRight]
        in Percolation newWqu newOpenStates sideLength totalLength

    tryConnectTo :: UArray Int Bool -> Int -> Int -> Int -> WeightedQuickUnion -> Direction -> WeightedQuickUnion
    tryConnectTo openStates sideLength totalLength actual wqu direction =
        maybe wqu (union wqu actual) $
            unitFrom openStates sideLength (actualIndexLowLevel sideLength totalLength actual) direction

    dumbUnitFrom :: (X, Y) -> Direction -> (X, Y)
    dumbUnitFrom (X x, Y y) PercUp    = (X (x - 1), Y y)
    dumbUnitFrom (X x, Y y) PercDown  = (X (x + 1), Y y)
    dumbUnitFrom (X x, Y y) PercLeft  = (X x,       Y (y - 1))
    dumbUnitFrom (X x, Y y) PercRight = (X x,       Y (y + 1))

    -- Calculate the unit that is in the direction from this unit.  If
    -- it is not a valid point, then return Nothing, else return Just the
    -- other unit actual value.
    unitFrom :: UArray Int Bool -> Int -> (X, Y) -> Direction -> Maybe Int
    unitFrom openStates sideLength xy@(X x, _) direction
        -- hard code for when we are connecting to the top point
        | x == 1 && direction == PercUp = Just 1
        -- and the bottom point
        | x == sideLength && direction == PercDown = Just 2
        | otherwise = otherUnitFrom openStates sideLength $ dumbUnitFrom xy direction

    -- Make sure the other unit we are looking at falls into the allowable
    -- range.
    otherUnitFrom :: UArray Int Bool -> Int -> (X, Y) -> Maybe Int
    otherUnitFrom openStates sideLength otherXy@(X otherX, Y otherY)
        | otherX < 1 || otherX > sideLength || otherY < 1 || otherY > sideLength = Nothing
        | otherwise = checkIfOpen openStates $ xyLowLevel sideLength otherXy

    -- Make sure that the other site we are trying to link to is actually
    -- open.
    checkIfOpen :: UArray Int Bool -> Int -> Maybe Int
    checkIfOpen openStates otherActual
        | isOpenLowLevel otherActual openStates = Just otherActual
        | otherwise = Nothing

open :: Percolation -> (X, Y) -> Percolation
open perc xy = openActual (xyToActualIndex perc xy) perc

isOpenLowLevel :: Int -> UArray Int Bool -> Bool
isOpenLowLevel actual openStates = openStates ! actual

isOpenActual :: Int -> Percolation -> Bool
isOpenActual actual perc = percolationOpenState perc ! actual

isOpen :: Percolation -> (X, Y) -> Bool
isOpen perc xy = isOpenActual (xyToActualIndex perc xy) perc

isFull :: Percolation -> (X, Y) -> Bool
isFull perc xy = let wqu = percolationWqu perc
                     actualIndex = xyToActualIndex perc xy
                 in isConnected wqu 1 actualIndex

doesPercolate :: Percolation -> Bool
doesPercolate (Percolation wqu _ _ _) = isConnected wqu 1 2

newPercolation :: Int -> Percolation
newPercolation sideLength =
    let totalLength = sideLength * sideLength + 2
        wqu = (newWeightedQuickUnion totalLength)
        openStates = runSTUArray $ do
            -- open up the first two sites (these are are first and second
            -- things that connect to two the top row and bottom row
            -- respectively).
            let firstTwoOpen = True : True : repeat False
            arr <- newListArray (1, totalLength) firstTwoOpen :: ST s (STUArray s Int Bool)
            return arr
    in Percolation wqu openStates sideLength totalLength
