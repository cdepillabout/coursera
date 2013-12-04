
module Main ( main
            )
            where

import Control.Monad (ap, forever, when)
import Control.Monad.ST (runST, ST)
import Data.Array.IO (freeze, newArray, readArray, IOArray, writeArray)
import Data.Array.ST (freeze, thaw, newArray, newListArray, readArray, runSTArray, runSTUArray, STArray, STUArray, writeArray)
import Data.Array (Array)
import Data.Array.Unboxed (UArray, (!))
import Data.Array.Unsafe (unsafeThaw)
import Data.Ix (Ix)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.IO (getLine)
import Text.Read (readMaybe)
import Text.Printf (printf)

import Percolation
import PercolationStats
import WeightedQuickUnion

die :: String -> IO a
die err = do putStrLn $ "ERROR: " ++ err
             exitWith (ExitFailure 1)

parsingFailure :: String -> IO a
parsingFailure val = die $ printf "Couldn't parse \"%s\" as an integer" val

parsingFailureWithName :: String -> String -> IO a
parsingFailureWithName name val =
        die $ printf "Couldn't parse %s \"%s\" as an integer" name val

main :: IO ()
main = do
        --[n, times] <- getArgs
        args <- getArgs
        progName <- getProgName
        case args of
            [strNums, strTimes] -> do
                let maybeNums = readMaybe strNums :: Maybe Int
                    maybeTimes = readMaybe strTimes :: Maybe Int
                checkMaybeArgs strNums maybeNums strTimes maybeTimes
            _ -> die ("Must run like `" ++ progName ++ " SIZE TIMES`.")
    where
        checkMaybeArgs :: String -> Maybe Int -> String -> Maybe Int -> IO ()
        checkMaybeArgs badStrNums Nothing _ _ =
            die ("Could not parse NUMS \"" ++ badStrNums ++ "\" as integer")
        checkMaybeArgs _ _ badStrTimes Nothing =
            die ("Could not parse TIMES \"" ++ badStrTimes ++ "\" as integer")
        checkMaybeArgs _ (Just nums) _ (Just times) = runWithNumsAndTimes nums times

        runWithNumsAndTimes :: Int -> Int -> IO ()
        runWithNumsAndTimes nums times = do
            stats <- runPercolationStats nums times
            print stats
            exitWith ExitSuccess
