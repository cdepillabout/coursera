
module PercolationStats ( runPercolationStats
                        , PercolationStats(..)
                        )
                        where

import Control.Monad.Trans.Class

import Percolation

data PercolationInfo = PercolationInfo
                 { percolation :: Percolation
                 , sideLength :: Int
                 , times :: Int
                 } deriving (Show)

data PercolationStats = PercolationStats
                 { percStatsMean :: Double
                 , percStatsStdDev :: Double
                 , percStatsConfidenceLow :: Double
                 , percStatsConfidenceHigh :: Double
                 , percStatsSideLength :: Int
                 , percStatsTimes :: Int
                 } deriving (Show)


mean :: undefiend
mean = undefined

stddev :: undefiend
stddev = undefined

confidenceLo :: undefiend
confidenceLo = undefined

confidenceHi :: undefiend
confidenceHi = undefined

openRandom :: Percolation -> IO Percolation
openRandom perc = undefined

findWhenPercolates :: Int -> IO Int
findWhenPercolates sideLength = do
        let perc = newPercolation sideLength
        allOpens <- openUntilPercolates perc
        return $ length allOpens
  where
    tryPercs :: (Bool, Percolation) -> IO (Bool, Percolation)
    tryPercs (doesPerc, perc)
        | doesPerc == True = error "This should also never percolate!!!"
        | otherwise = do
            newPerc <- openRandom perc
            return (doesPercolate newPerc, newPerc)

    openUntilPercolatesHelper :: (Bool, Percolation) -> [IO (Bool, Percolation)]
    openUntilPercolatesHelper (doesPerc, perc) = do
            (newDoesPerc, newPerc) <- (lift $ tryPercs (doesPerc, perc)) :: [IO (Bool, Percolation)]
            if newDoesPerc == True
                then [return (True, newPerc)]
                else return (False, newPerc) : openUntilPercolatesHelper (False, newPerc)

    openUntilPercolates :: Percolation -> IO [(Bool, Percolation)]
    openUntilPercolates perc
        | doesPercolate perc = error "It should not percolate from the very beginning!"
        | otherwise = sequence $ openUntilPercolatesHelper (False, perc)

runPercolationStats :: Int -> Int -> IO PercolationStats
runPercolationStats n t = do undefined

--        PercolationStats
 --                       (mean)
  --                      (stddev)
   --                     (confidenceLo)
    --                    (confidenceHi)

