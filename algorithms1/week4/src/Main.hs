
module Main ( main
            )
            where

import EightPuzzle
import EightPuzzle.PriorityQueue
import Control.Monad.ST (runST, ST)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer (runWriter, tell, runWriterT, Writer, writer, WriterT, execWriterT)
import Control.Monad.Trans.Class (lift)

main :: IO ()
main = do
        let result = runST $ execWriterT $ run
        mapM_ putStrLn result

run :: WriterT [String] (ST s) ()
run = do
        logMsg "starting..."
        pq <- lift $ newPQ
        logSTOp "new pq, initial size" $ size pq
        lift $ insert pq (3 :: Int)
        logSTOp "after insert 3, size" $ size pq
        logSTOp "after insert 3, min" $ minOfPQ pq
        lift $ insert pq 4
        logSTOp "after insert 4, size" $ size pq
        logSTOp "after insert 4, min" $ minOfPQ pq
        lift $ insert pq 2
        logSTOp "after insert 2, size" $ size pq
        logSTOp "after insert 2, min" $ minOfPQ pq
        return ()
  where
      --logSTOp :: (Monad m, Show a) => String -> ST s a -> WriterT [String] (ST s) ()
      logSTOp :: (Monad m, Show a) => String -> m a -> WriterT [String] m ()
      logSTOp message stOperation = do
          result <- lift stOperation
          logMsg (message ++ ": " ++ show result)

      logMsg :: (Monad m) => String -> WriterT [String] m ()
      logMsg message = tell [message]
