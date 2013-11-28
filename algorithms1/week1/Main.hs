
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
            let perc = newPercolation nums
            _ <- operateOnForever perc helper
            exitWith ExitSuccess

        operateOnForever :: Monad m => t -> (t -> m t) -> m b
        operateOnForever value function = do
            returnValue <- function value
            operateOnForever returnValue function

        helper :: Percolation -> IO (Percolation)
        helper perc = do
                print perc
                (p, q) <- readTwoInts
                let pointA = toXY p q
                newPerc <- if isOpen perc pointA
                                   then putStrLn "Already open!" >> return perc
                                   else return $ open perc pointA
                if isFull newPerc pointA
                    then putStrLn "Filled with water!!"
                    else putStrLn "No water yet..."
                when (doesPercolate perc) (putStrLn "Percolates!!!!!!")
                return newPerc

        readTwoInts :: IO (Int, Int)
        readTwoInts = do
            line <- getLine
            when (line == "") $ exitWith ExitSuccess
            case words line of
                [strA, strB] -> return (,)
                    `ap` (maybe (parsingFailure strA) return $ readMaybe strA)
                    `ap` (maybe (parsingFailure strB) return $ readMaybe strB)
                _ -> die $ "Couldn't parse two integers from line: " ++ line

