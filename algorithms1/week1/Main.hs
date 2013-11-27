
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
            let wqu = newWeightedQuickUnion nums
            _ <- operateOnForever wqu helper
            exitWith ExitSuccess

        operateOnForever :: Monad m => t -> (t -> m t) -> m b
        operateOnForever value function = do
            returnValue <- function value
            operateOnForever returnValue function

        helper :: WeightedQuickUnion -> IO (WeightedQuickUnion)
        helper wqu = do
                print wqu
                (p, q) <- readTwoInts
                --words line
                let connected = isConnected wqu p q
                putStrLn $ "Is connected? " ++ show connected
                return $ union wqu p q
                --return array

        readTwoInts :: IO (Int, Int)
        readTwoInts = do
            line <- getLine
            when (line == "") $ exitWith ExitSuccess
            case words line of
                [strA, strB] -> return (,)
                    `ap` (maybe (parsingFailure strA) return $ readMaybe strA)
                    `ap` (maybe (parsingFailure strB) return $ readMaybe strB)
                _ -> die $ "Couldn't parse two integers from line: " ++ line



--    * Reads in a sequence of pairs of integers (between 0 and N-1) from standard input,
--    * where each integer represents some object;
--    * if the objects are in different components, merge the two components
--    * and print the pair to standard output.
--    */
--public static void main(String[] args) {
--    int N = StdIn.readInt();
--    WeightedQuickUnionUF uf = new WeightedQuickUnionUF(N);
--    while (!StdIn.isEmpty()) {
--        int p = StdIn.readInt();
--        int q = StdIn.readInt();
--        if (uf.connected(p, q)) continue;
--        uf.union(p, q);
--        StdOut.println(p + " " + q);
--    }
--    StdOut.println(uf.count() + " components");
--}

