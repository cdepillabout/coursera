
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
