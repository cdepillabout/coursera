{-# LANGUAGE RankNTypes #-}

module WeightedQuickUnion ( countWQU
                          , findWQU
                          )
                          where

import Control.Monad.ST (ST)
import Data.Array.ST (STUArray)

data Index = Index Int

data ParentId = ParentId Index

data Size = Size Int

data WeightedQuickUnion s = WeightedQuickUnion
                          { wQUIds :: ST s (STUArray s Index ParentId)
                          , wQUSize :: ST s (STUArray s Index Size)
                          , wQUCount :: Int
                          }

countWQU :: WeightedQuickUnion s -> Int
countWQU wqu = wQUCount wqu

findWQU :: WeightedQuickUnion s -> Index -> Int
findWQU wqu p = undefined


