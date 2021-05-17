{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

module Sort where

import           Control.Monad (forM_, when)
import           Control.Monad.ST (ST, runST)
import           Data.Array (bounds, (!))
import Data.Array.ST
  (MArray(newArray_, getBounds), freeze, readArray, thaw, writeArray)
import           Data.Maybe (fromJust)

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
        forM_ [inf..(mid - 1)] $ \i -> readArray arrST i >>= writeArray arrST1 i
        forM_ [mid..(sup - 1)] $ \j -> readArray arrST j >>= writeArray arrST2 j
        mergeSort' arrST1 inf mid
        mergeSort' arrST2 mid sup
        (i, j, k) <- loop (inf, mid, inf) $ \(i, j, k) ->
          breakWhen (i >= mid || j >= sup) (i, j, k) $ do
            vi <- readArray arrST1 i
            vj <- readArray arrST2 j
            if vi > vj
              then writeArray arrST k vj >> return (i, j + 1, k + 1)
              else writeArray arrST k vi >> return (i + 1, j, k + 1)
        forM_ (zip [i..(mid - 1)] [k..]) $ \(i, k) ->
          readArray arrST1 i >>= writeArray arrST k
        forM_ (zip [j..(sup - 1)] [k..]) $ \(j, k) -> do
          readArray arrST2 j >>= writeArray arrST k

quickSort :: forall a. (Ord a) => Vec1D a -> Vec1D a
quickSort arr
  = runST $ do
    arrST <- thaw arr :: ST s (STVec1D s a)
    let (lb, ub) = bounds arr
    quickSort' arrST lb (ub + 1)
    freeze arrST
    where
      quickSort' arrST inf sup
        | inf + 1 >= sup = return ()
        | otherwise      = do
          pivot  <- readArray arrST inf
          (i, _) <- loop (inf, sup) $ \(i, j) ->
            breakWhen (i + 1 == j) (i, j) $ do
              vi <- readArray arrST (i + 1)
              if vi > pivot
                then do
                  vj <- readArray arrST (j - 1)
                  writeArray arrST (i + 1) vj
                  writeArray arrST (j - 1) vi
                  return (i, j - 1)
               else return (i + 1, j)
          vi <- readArray arrST i
          writeArray arrST i pivot
          writeArray arrST inf vi
          quickSort' arrST inf i
          quickSort' arrST (i + 1) sup

fixMaxHeap :: (Ord a) => STVec1D s a -> Int -> Int -> ST s ()
fixMaxHeap hST r lim = do
  breakWhen_ (2 * r > lim) $ do
    vr    <- readArray hST r
    vlc   <- readArray hST (2 * r)
    mbvrc <- readArrayMaybe hST (2 * r + 1)
    if (2 * r + 1 > lim) || (fromJust mbvrc <= vlc)
      then when (vr < vlc) $ do
        writeArray hST r vlc
        writeArray hST (2 * r) vr
        fixMaxHeap hST (2 * r) lim
      else when (vr < fromJust mbvrc) $ do
        writeArray hST r (fromJust mbvrc)
        writeArray hST (2 * r + 1) vr
        fixMaxHeap hST (2 * r + 1) lim
  return ()

toMaxHeap :: (Ord a) => STVec1D s a -> ST s ()
toMaxHeap hST
  = toMaxHeap' hST 1
  where
    toMaxHeap' hST r = do
      (_, sup) <- getBounds hST
      breakWhen_ (r > sup) $ do
        toMaxHeap' hST (2 * r)
        toMaxHeap' hST (2 * r + 1)
        fixMaxHeap hST r sup
      return ()

heapSort :: forall a. (Ord a) => Vec1D a -> Vec1D a
heapSort arr = runST $ do
  let (lb, ub) = bounds arr
  let len      = ub - lb + 1
  hST <- newHeapFromArray lb ub
  toMaxHeap hST
  heapSort' hST len
  freeze hST
  where
    newHeapFromArray :: Int -> Int -> ST s (STVec1D s a)
    newHeapFromArray lb ub = do
      hST <- newArray_ (1, ub - lb + 1)
      forM_ [1..(ub - lb + 1)] $ \i -> writeArray hST i $ arr ! (i - 1 + lb)
      return hST
    heapSort' hST 1
      = return ()
    heapSort' hST k = do
      v1 <- readArray hST 1
      vk <- readArray hST k
      writeArray hST 1 vk
      writeArray hST k v1
      fixMaxHeap hST 1 (k - 1)
      heapSort' hST (k - 1)
