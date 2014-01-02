
module Main ( main
            )
            where

import EightPuzzle
import EightPuzzle.PriorityQueue
import Control.Monad.ST (runST, ST)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer (runWriter, tell, runWriterT, Writer, writer, WriterT, execWriterT)
import Control.Monad.Trans.Class (lift)
import Control.Monad (forM_)

main :: IO ()
main = do
        let result = runST $ execWriterT $ run1
        mapM_ putStrLn result

run2 :: WriterT [String] (ST s) ()
run2 = do
        pq <- lift $ newPQ
        forM_ [1..75000] $ \num -> do
            lift $ insert pq num
        forM_ [1..75000] $ \_ -> do
            lift $ delMin pq

run1 :: WriterT [String] (ST s) ()
run1 = do
        logMsg "starting..."
        pq <- lift $ newPQ
        logSTOp "new pq, initial size" $ size pq
        lift $ insert pq (3 :: Int)
        logSTOp "after insert 3, size" $ size pq
        logSTOp "after insert 3, min" $ minOfPQ pq
        lift $ delMin pq
        logSTOp "after delete 3, size" $ size pq
        logSTOp "after delete 3, min" $ minOfPQ pq
        lift $ insert pq 4
        logSTOp "after insert 4, size" $ size pq
        logSTOp "after insert 4, min" $ minOfPQ pq
        lift $ insert pq 2
        logSTOp "after insert 2, size" $ size pq
        logSTOp "after insert 2, min" $ minOfPQ pq
        lift $ insert pq 5
        logSTOp "after insert 5, size" $ size pq
        logSTOp "after insert 5, min" $ minOfPQ pq
        lift $ delMin pq
        logSTOp "after delete 2, size" $ size pq
        logSTOp "after delete 2, min" $ minOfPQ pq
        lift $ delMin pq
        logSTOp "after delete 4, size" $ size pq
        logSTOp "after delete 4, min" $ minOfPQ pq
        lift $ delMin pq
        logSTOp "after delete 5, size" $ size pq
        logSTOp "after delete 5, min" $ minOfPQ pq
        lift $ delMin pq
        logSTOp "after delete ?, size" $ size pq
        logSTOp "after delete ?, min" $ minOfPQ pq
        lift $ insert pq 9
        logSTOp "after insert 9, size" $ size pq
        logSTOp "after insert 9, min" $ minOfPQ pq
        lift $ insert pq 8
        logSTOp "after insert 8, size" $ size pq
        logSTOp "after insert 8, min" $ minOfPQ pq
        lift $ insert pq 7
        logSTOp "after insert 7, size" $ size pq
        logSTOp "after insert 7, min" $ minOfPQ pq
        lift $ delMin pq
        logSTOp "after delete 7, size" $ size pq
        logSTOp "after delete 7, min" $ minOfPQ pq
        lift $ delMin pq
        logSTOp "after delete 8, size" $ size pq
        logSTOp "after delete 8, min" $ minOfPQ pq
        lift $ delMin pq
        logSTOp "after delete 9, size" $ size pq
        logSTOp "after delete 9, min" $ minOfPQ pq
        {-
        lift $ insert pq 1
        logSTOp "after insert 1, size" $ size pq
        logSTOp "after insert 1, min" $ minOfPQ pq
        lift $ insert pq 5
        logSTOp "after insert 5, size" $ size pq
        logSTOp "after insert 5, min" $ minOfPQ pq
        -}
        return ()
  where
      --logSTOp :: (Monad m, Show a) => String -> ST s a -> WriterT [String] (ST s) ()
      logSTOp :: (Monad m, Show a) => String -> m a -> WriterT [String] m ()
      logSTOp message stOperation = do
          result <- lift stOperation
          logMsg (message ++ ": " ++ show result)

      logMsg :: (Monad m) => String -> WriterT [String] m ()
      logMsg message = tell [message]
