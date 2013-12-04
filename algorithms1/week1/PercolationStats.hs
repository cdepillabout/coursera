
module PercolationStats ( runPercolationStats
                        , PercolationStats(..)
                        )
                        where

import Control.Monad (forM)
import System.Random (randomRIO)

import Percolation

data PercolationStats = PercolationStats
                 { percStatsMean :: Double
                 , percStatsStdDev :: Double
                 , percStatsConfidenceLow :: Double
                 , percStatsConfidenceHigh :: Double
                 , percStatsSideLength :: Int
                 , percStatsTimes :: Int
                 } deriving (Show)


mean :: [Int] -> Int -> Double
mean results len = (fromIntegral $ sum results) / fromIntegral len

stddev :: undefiend
stddev = undefined

confidenceLo :: undefiend
confidenceLo = undefined

confidenceHi :: undefiend
confidenceHi = undefined

openRandom :: Percolation -> IO Percolation
openRandom perc = do
        let sideLen = percolationSideLength perc
        randomA <- randomRIO (1, sideLen)
        randomB <- randomRIO (1, sideLen)
        let xy = toXY randomA randomB
        if isOpen perc xy
            then openRandom perc
            else do
                --putStrLn $ "Opening " ++ show xy
                return $ open perc xy

findWhenPercolates :: Int -> IO Int
findWhenPercolates sideLen = do
        let perc = newPercolation sideLen
        allOpens <- openUntilPercolates perc
        return $ length allOpens
  where
    openUntilPercolatesHelper :: Percolation -> IO [Bool]
    openUntilPercolatesHelper perc = do
            newPerc <- openRandom perc
            if doesPercolate newPerc == True
                then return [True]
                else do
                    l <- openUntilPercolatesHelper newPerc
                    return $ False : l

    openUntilPercolates :: Percolation -> IO [Bool]
    openUntilPercolates perc
        | doesPercolate perc = error "It should not percolate from the very beginning!"
        | otherwise = openUntilPercolatesHelper perc

runPercolationStats :: Int -> Int -> IO PercolationStats
runPercolationStats n t = do
        resultLengths <- forM (replicate t n) findWhenPercolates
        let myMean = mean resultLengths t / fromIntegral (n * n)
            varianceList = map (\len -> (fromIntegral len) - myMean) resultLengths
            variance = sum (map (^2) varianceList) / fromIntegral t
            stdDev = sqrt variance
            confidenceLow = myMean - (1.96) * stdDev
            confidenceHigh = myMean + (1.96) * stdDev
        return $ PercolationStats myMean stdDev confidenceLow confidenceHigh n t
                        --(mean resultLengths t)
                        --(stddev resultLengths)
                        --(confidenceLo resultLengths)
                        --(confidenceHi resultLengths)
                        --n
                        --t

