-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

module HamiltonCircuit where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Array (array, (!))
import           Data.Bits
import           Data.Foldable

-- Requires installation
import           Data.Map as M (Map(..), empty, insert, (!?))
import           Data.Sequence as S (Seq(..), fromList, (><))

import           Graph
import           Utilities

-- Find the hamilton circuit of a graph with the shortest total distance.
-- Nothing if does not exist.
-- The complexity is worse than exponential, thus of no use for large graphs.
hamiltonCircuit :: Graph a => a -> Maybe (Int, [Int])
hamiltonCircuit graph = do
  raw <- fst $ execState (bellmanHeldKarpS starter f fin graph) (Nothing, empty)
  return (fst raw, toList $ snd raw)
  where
    starter n i = do
      (_, dict) <- get
      let val = liftM2 (,) (weight (n, i) graph) (Just $ fromList [n, i])
      put (Nothing, insert (0, i) val dict)
    f s s' i j  = do
      (_, dict) <- get
      let cur = join $ dict !? (s, i)
      let sub = join $ dict !? (s', j)
      let dis = liftM2 (+) (fst <$> sub) (weight (j, i) graph)
      let nxt = liftM2 (,) dis $ liftM2 (><) (snd <$> sub) (Just $ fromList [i])
      put (Nothing, insert (s, i) (minMaybeOn fst cur nxt) dict)
      return ()
    fin s' n i  = do
      (res, dict) <- get
      let sub = join $ dict !? (s', i)
      let dis = liftM2 (+) (fst <$> sub) (weight (i, n) graph)
      let nxt = liftM2 (,) dis $ liftM2 (><) (snd <$> sub) (Just $ fromList [n])
      put (minMaybeOn id res (minMaybeOn fst res nxt), dict)

-- A State that simulates the bare-bones of Bellman-Held-Karp Algorithm
-- See full documentation in README.md.
bellmanHeldKarpS :: (Graph a) 
  => (Int -> Int -> State b ()) 
  -> (Integer -> Integer -> Int -> Int -> State b ()) 
  -> (Integer -> Int -> Int -> State b ()) 
  -> a 
  -> State b ()
bellmanHeldKarpS starter f finisher graph 
  | null $ nodes graph = return ()
  | otherwise          = do
    forM_ ns' $ \x -> starter n x
    forM_ ([1..sup]) $ \s -> forM_ range $ \i -> if testBit s i
      then return ()
      else forM_ range $ \j -> do
        let isIn = testBit s j
        if isIn && i /= j
          then f s (s - bit j) (ns ! i) (ns ! j)
          else return ()
    forM_ range $ \i -> finisher (sup - bit i) n (ns ! i)
  where
    (n : ns') = nodes graph
    sz        = length ns'
    ns        = array (0, sz - 1) $ zip range ns'
    range     = [0..(sz - 1)]
    sup       = bit sz - 1
