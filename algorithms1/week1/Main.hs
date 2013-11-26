
module Main ( main
            )
            where

import Control.Monad.ST (runST, ST)
import Data.Array.IO (freeze, newArray, readArray, IOArray, writeArray)
import Data.Array.ST (freeze, thaw, newArray, readArray, runSTArray, STArray, writeArray)
import Data.Array (Array)
import Data.Array.Unsafe (unsafeThaw)
import System.Environment (getArgs)

import Percolation
import PercolationStats
import WeightedQuickUnion

main :: IO ()
main = do
        [n, times] <- getArgs
        let nums = read n :: Int
        finalArray <- buildArray nums
        f <- freeze finalArray :: IO (Array Int Int)
        print f
        let g = runSTArray $ buildArrayST nums
        print g
        print $ runSTArray $ doWhat g
        print g
        print $ runSTArray $ doWhat g
        print g
        print $ runSTArray $ doWhat g
        print g
        print $ runSTArray $ doWhat g
        print g
        print $ runSTArray $ doWhat g
        print g


buildArray :: Int -> IO (IOArray Int Int)
buildArray n = do
        arr <- newArray (0, n) 999999 :: IO (IOArray Int Int)
        a <- readArray arr 1
        writeArray arr 1 64
        b <- readArray arr 1
        return arr

buildArrayST :: Int -> ST s (STArray s Int Int)
buildArrayST n = do
        arr <- newArray (0, n) 999999 :: ST s (STArray s Int Int)
        a <- readArray arr 1
        writeArray arr 1 64
        b <- readArray arr 1
        writeArray arr 5 5
        return arr

doWhat :: Array Int Int -> ST s (STArray s Int Int)
doWhat array = do
        f <- unsafeThaw array :: ST s (STArray s Int Int)
        a <- readArray f 5
        writeArray f 5 (a+1)
        return f

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

