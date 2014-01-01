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
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as Vector
import qualified Data.Vector.Generic.Mutable as GenericVector
import Data.Vector.Generic ((!))

import EightPuzzle.PriorityQueue.Internal

newtype PriorityQueue s k = PQ (STRef s (PriorityQueue_ s k))

data PriorityQueue_ s k = PriorityQueue
    { vector :: MVector s k
    , vectorSize :: Int
    , itemCount :: Int
    }


newPQ :: ST s (PriorityQueue s k)
newPQ = newSizedPQReal 10 >>= newRef

newSizedPQReal :: Int -> ST s (PriorityQueue_ s k)
newSizedPQReal size = do
        vec <- Vector.replicate size undefined
        return $! PriorityQueue vec size 0


isEmpty :: PriorityQueue s k -> ST s (Bool)
isEmpty pqRef = do
        fmap (== 0) (size pqRef)

size :: PriorityQueue s k -> ST s Int
size (PQ ref) = do
        pq <- readSTRef ref
        return $ size' pq
  where
    size' :: PriorityQueue_ s k -> Int
    size' (PriorityQueue _ _ count) = count

minOfPQ :: PriorityQueue s k -> ST s (Maybe k)
minOfPQ pqRef = safeReadVec pqRef 0

insert :: forall s k. PriorityQueue s k -> k -> ST s ()
insert (PQ ref) item = do
        pq <- readSTRef ref
        pq' <- insert' pq
        writeSTRef ref pq'
  where
    insert' :: PriorityQueue_ s k -> ST s (PriorityQueue_ s k)
    insert' (PriorityQueue vec vecSize count) = do
        --when (vecSize == count) (resize pq (vecSize * 2))
        Vector.write vec count item
        return $ PriorityQueue vec vecSize (count + 1)

delMin = undefined

newRef :: PriorityQueue_ s k -> ST s (PriorityQueue s k)
newRef pq = fmap PQ $ newSTRef pq

guardM :: (Monad m, MonadPlus n) => m (Bool) -> m (n a) -> m (n a)
guardM wrappedBool elseM = do
        unwrappedBool <- wrappedBool
        if unwrappedBool then return mzero else elseM

unsafeReadVec :: PriorityQueue s k -> Int -> ST s k
unsafeReadVec (PQ ref) idx = do
        pq <- readSTRef ref
        let vec = vector pq
        Vector.read vec idx

safeReadVec :: PriorityQueue s k -> Int -> ST s (Maybe k)
safeReadVec pqRef idx = do
        guardM (isEmpty pqRef) $ fmap Just $ unsafeReadVec pqRef idx


resize :: PriorityQueue_ s k -> Int -> ST s (PriorityQueue_ s k)
resize pq newSize = undefined
