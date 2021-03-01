{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

module Sort where

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.State
import           Data.Array
import           Data.Array.ST

import           Utilities

mergeSort :: forall a. (Ord a) => Vec1D a -> Vec1D a
mergeSort arr
  = runST $ do
    arrST <- thaw arr :: ST s (STVec1D s a)
    let (lb, ub) = bounds arr
    mergeSort' arrST lb (ub + 1)
    freeze arrST
  where
    mergeSort' arrST inf sup
      | inf + 1 >= sup = return ()
      | otherwise      = do
        let mid = (inf + sup) `div` 2
        arrST1 <- newArray_ (inf, mid - 1) :: ST s (STVec1D s a)
        arrST2 <- newArray_ (mid, sup - 1) :: ST s (STVec1D s a)
        forM_ [inf..(mid - 1)] $ \i -> do
          v <- readArray arrST i
          writeArray arrST1 i v
        forM_ [mid..(sup - 1)] $ \j -> do
          v <- readArray arrST j
          writeArray arrST2 j v
        mergeSort' arrST1 inf mid
        mergeSort' arrST2 mid sup
        (i, j, k) <- loop (inf, mid, inf) $ \(i, j, k) -> 
          breakWhen (i >= mid || j >= sup) (i, j, k) $ do
            vi <- readArray arrST1 i
            vj <- readArray arrST2 j
            if vi > vj
              then writeArray arrST k vj >> return (i, j + 1, k + 1)
              else writeArray arrST k vi >> return (i + 1, j, k + 1)
        forM_ (zip [i..(mid - 1)] [k..]) $ \(i, k) -> do
          vi <- readArray arrST1 i
          writeArray arrST k vi
        forM_ (zip [j..(sup - 1)] [k..]) $ \(j, k) -> do
          vj <- readArray arrST2 j
          writeArray arrST k vj
        return ()
