
        --let nums = read n :: Int
        --finalArray <- buildArray nums
        --f <- freeze finalArray :: IO (Array Int Int)
        --print f
        --let g = runSTArray $ buildArrayST nums
        --print g
        --print $ runSTArray $ doWhat g
        --print g
        --print $ runSTArray $ doWhat g
        --print g
        --print $ runSTArray $ doWhat g
        --print g
        --print $ runSTArray $ doWhat g
        --print g
        --print $ runSTArray $ doWhat g
        --print g


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



-- Weighted Quick Union MAIN!!!!
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
