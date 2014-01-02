{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module EightPuzzle.PriorityQueue
    ( PriorityQueue
    , delMin
    , insert
    , isEmpty
    , minOfPQ
    , newPQ
    , size
    ) where

import Control.Monad (guard, MonadPlus, mzero, when, liftM2)
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

insert :: forall s k. (Ord k, Show k) => PriorityQueue s k -> k -> ST s ()
insert (PQ ref) item = do
        pq <- readSTRef ref
        --debugPrintPQMsg_ "in insert, before inserting and checkResize, " pq
        pq' <- checkSize =<< insert' pq
        --debugPrintPQMsg_ "in insert, after after inserting and checkResize, " pq'
        writeSTRef ref pq'
        --debug "done insert"
  where
    insert' :: PriorityQueue_ s k -> ST s (PriorityQueue_ s k)
    insert' pq@(PriorityQueue vec _ countRef) = do
        count <- readSTRef countRef
        Vector.write vec count item
        modifySTRef countRef (1+)
        --debugPrintPQMsg_ "in insert', before swimming" pq
        pq' <- swim pq count
        --debugPrintPQMsg_ "in insert', after swimming" pq'
        return pq'

delMin :: (Show k, Ord k) => PriorityQueue s k -> ST s (Maybe k)
delMin pqRef@(PQ ref) = do
        empty <- isEmpty pqRef
        if empty
            then return Nothing
            else do
                minFromQueue <- minOfPQ pqRef
                pq <- readSTRef ref
                let (PriorityQueue vec _ countRef) = pq
                count <- readSTRef countRef

                -- exchange the first and last elements
                exchange pq 0 (count - 1)
                -- write bottom to the previous last element
                Vector.write vec (count - 1) undefined
                -- update the count
                writeSTRef countRef (count -  1)
                -- sink the new top element to where it should be
                pq' <- sink pq 0 (count - 1)

                -- possible resize the priority queue
                checkSize pq'

                return minFromQueue



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
        --debugPrintPQMsg_ "in checkSize (after inserting or deleting), " pq
        go count
  where
      go :: Int -> ST s (PriorityQueue_ s k)
      go count
            | count == vecSize = do
                --debug "EXPANDING!!!"
                resize pq (vecSize*2)
            | count <= vecSize `div` 4 = do
                --debug "SHRINKING!!!"
                resize pq (vecSize `div` 2)
            | otherwise = do
                --debug "NOT RESIZING!!!"
                return pq

resize :: PriorityQueue_ s k -> Int -> ST s (PriorityQueue_ s k)
resize pq@(PriorityQueue vec vecSize countRef) newSize = do
        newPriorityQueue <- newSizedPQReal newSize
        let (PriorityQueue newVec _ newCountRef) = newPriorityQueue
        count <- readSTRef countRef
        writeSTRef newCountRef count
        --debugPrintPQMsg_ "in resize old queue, " pq
        --debugPrintPQMsg_ "in resize new queue, " newPriorityQueue
        --copyArrayTo newVec count
        let oldVecSlice = Vector.unsafeSlice 0 count vec
        let newVecSlice = Vector.unsafeSlice 0 count newVec
        Vector.unsafeCopy newVecSlice oldVecSlice
        return newPriorityQueue
  where
    copyArrayTo newVec count = go 0
      where
        go !idx
            | idx >= count = return ()
            | otherwise = do
                Vector.read vec idx >>= Vector.write newVec idx
                go $ idx+1


swim :: (Ord k, Show k) => PriorityQueue_ s k -> Int -> ST s (PriorityQueue_ s k)
swim pq idx
        | idx < 1 = return pq
        | otherwise = do
            let parentIdx = ((idx + 1) `div` 2) - 1
            isGreater <- greater pq parentIdx idx
            --debug ("in swim, idx: " ++ show idx ++ ", parentIdx: " ++ show parentIdx)
            case isGreater of
                True -> do
                    exchange pq idx parentIdx
                    swim pq parentIdx
                False -> return pq

sink :: (Ord k, Show k) => PriorityQueue_ s k -> Int -> Int -> ST s (PriorityQueue_ s k)
sink !pq !idx !count
        | (2 * (idx+1)) - 1 >= count = return pq
        | otherwise = do
            let !j = 2 * idx
            --debug ("in sink, idx: " ++ show idx ++ ", j: " ++ show j ++ ", count: " ++ show count)
            !isGreater <- greater pq j (j+1)
            let !j' = if j < count && isGreater then (j+1) else j
            !isGreater' <- greater pq idx j'
            if not isGreater'
                then return pq
                else do
                    exchange pq idx j'
                    sink pq j' count


exchange :: (Show k) => PriorityQueue_ s k -> Int -> Int -> ST s ()
exchange (PriorityQueue !vec _ _) !idxA !idxB = do
        Vector.unsafeSwap vec idxA idxB

        --itemA <- Vector.read vec idxA
        --itemB <- Vector.read vec idxB
        --debug ("in exch, exching idxA (" ++ show idxA ++ ") with idxB(" ++ show idxB ++ ")")
        --debug ("in exch, exching itemA (" ++ show itemA ++ ") with idxB(" ++ show itemB ++ ")")
        --Vector.write vec idxA itemB
        --Vector.write vec idxB itemA
        --return pq

greater :: (Ord k) => PriorityQueue_ s k -> Int -> Int -> ST s (Bool)
greater (PriorityQueue !vec _ _) !idxA !idxB = do
        --itemA <- Vector.read vec idxA
        --itemB <- Vector.read vec idxB
        --return $ itemA > itemB
        liftM2 (>) (Vector.read vec idxA) (Vector.read vec idxB)
