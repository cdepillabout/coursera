{-# LANGUAGE ScopedTypeVariables #-}

module EightPuzzle.PriorityQueue
    ( PriorityQueue
    , delMin
    , insert
    , isEmpty
    , minOfPQ
    , newPQ
    , size
    ) where

import Control.Monad (guard, MonadPlus, mzero, when)
import Control.Monad.ST (ST, unsafeSTToIO, unsafeIOToST)
import Control.Monad.Trans.Class (lift)
import Data.STRef
       (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef)
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as Vector
import qualified Data.Vector.Generic.Mutable as GenericVector
import Data.Vector.Generic ((!))
import Data.Vector (freeze)

import EightPuzzle.PriorityQueue.Internal

newtype PriorityQueue s k = PQ (STRef s (PriorityQueue_ s k))

data PriorityQueue_ s k = PriorityQueue
    { vector :: MVector s k
    , vectorSize :: Int
    , itemCount :: STRef s Int
    }

---------------
-- DEBUGGING --
---------------
showPQ_ :: PriorityQueue_ s a -> ST s [Char]
showPQ_ (PriorityQueue vec vecSize countRef) = do
        count <- readSTRef countRef
        --frozenVec <- freeze vec
        return $ "<PQ"
              ++ " vectorSize: "
              ++ show vecSize
              ++ " count: "
              ++ show count
              ++ ">"

debugPrintPQ_ :: PriorityQueue_ s a -> ST s ()
debugPrintPQ_ pq = showPQ_ pq >>= debug

debugPrintPQMsg_ :: String -> PriorityQueue_ s a -> ST s ()
debugPrintPQMsg_ message pq = showPQ_ pq >>= debug . (message ++)

debug :: String -> ST s ()
debug message = unsafeIOToST $ putStrLn ("\t** " ++ message)


newPQ :: ST s (PriorityQueue s k)
newPQ = newSizedPQReal 1 >>= newRef

newSizedPQReal :: Int -> ST s (PriorityQueue_ s k)
newSizedPQReal pqSize = do
        vec <- Vector.replicate pqSize undefined
        countRef <- newSTRef 0
        return $! PriorityQueue vec pqSize countRef


isEmpty :: PriorityQueue s k -> ST s (Bool)
isEmpty pqRef = do
        fmap (== 0) (size pqRef)

size :: PriorityQueue s k -> ST s Int
size (PQ ref) = do
        (PriorityQueue _ _ countRef) <- readSTRef ref
        readSTRef countRef

minOfPQ :: PriorityQueue s k -> ST s (Maybe k)
minOfPQ pqRef = safeReadVec pqRef 0

insert :: forall s k. PriorityQueue s k -> k -> ST s ()
insert (PQ ref) item = do
        pq <- readSTRef ref
        debugPrintPQMsg_ "in insert, before inserting and checkResize, " pq
        pq' <- checkSize =<< insert' pq
        debugPrintPQMsg_ "in insert, after after inserting and checkResize, " pq'
        writeSTRef ref pq'
        debug "done insert"
  where
    insert' :: PriorityQueue_ s k -> ST s (PriorityQueue_ s k)
    insert' pq@(PriorityQueue vec _ countRef) = do
        --when (vecSize == count) (resize pq (vecSize * 2))
        count <- readSTRef countRef
        Vector.write vec count item
        modifySTRef countRef (1+)
        return $ pq

delMin = undefined

newRef :: PriorityQueue_ s k -> ST s (PriorityQueue s k)
newRef pq = fmap PQ $ newSTRef pq

guardM :: (Monad m, MonadPlus n) => m (Bool) -> m (n a) -> m (n a)
guardM wrappedBool elseM = do
        unwrappedBool <- wrappedBool
        if unwrappedBool then return mzero else elseM

safeReadVec :: PriorityQueue s k -> Int -> ST s (Maybe k)
safeReadVec pqRef idx = do
        guardM (isEmpty pqRef) $ fmap Just $ unsafeReadVec pqRef idx

unsafeReadVec :: PriorityQueue s k -> Int -> ST s k
unsafeReadVec (PQ ref) idx = do
        pq <- readSTRef ref
        let vec = vector pq
        Vector.read vec idx

checkSize :: forall s k. PriorityQueue_ s k -> ST s (PriorityQueue_ s k)
checkSize pq@(PriorityQueue _ vecSize countRef) = do
        count <- readSTRef countRef
        debugPrintPQMsg_ "in checkSize (after inserting), " pq
        go count
  where
      go :: Int -> ST s (PriorityQueue_ s k)
      go count
            | count == vecSize = do
                debug "EXPANDING!!!"
                resize pq (vecSize*2)
            | count <= vecSize `div` 4 = do
                debug "SHRINKING!!!"
                resize pq (vecSize `div` 2)
            | otherwise = do
                debug "NOT RESIZING!!!"
                return pq

resize :: PriorityQueue_ s k -> Int -> ST s (PriorityQueue_ s k)
resize (PriorityQueue vec vecSize countRef) newSize = do
        newPriorityQueue <- newSizedPQReal newSize
        let (PriorityQueue newVec _ newCountRef) = newPriorityQueue
        readSTRef countRef >>= writeSTRef newCountRef
        copyArrayTo newVec
        return newPriorityQueue
  where
    copyArrayTo newVec = go 0
      where
        go idx
            | idx >= vecSize = return ()
            | otherwise = do
                Vector.read vec idx >>= Vector.write newVec idx
                go (idx+1)


swim = undefined
