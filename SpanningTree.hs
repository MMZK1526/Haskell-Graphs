-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

module SpanningTree where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Array.Unboxed hiding ((!))
import           Data.List (maximum, minimum, minimumBy, sortBy)
import           Data.Maybe (fromJust)

-- May require installation
import           Data.IntMap.Lazy as IM 
  (IntMap(..), delete, empty, keys, insert, notMember, null, (!))
import           Data.Set as S 
  (Set(..), fromDescList, insert, notMember, member, size)

import           Graph
import           Utilities


--------------------------------------------------------------------------------
-- Prim's Algorithm (Classic)
--------------------------------------------------------------------------------

-- Returns the Minimum Spanning Tree via Prim's Algorithm.
-- Pre: The graph is undirected.
primMST :: Graph a => a -> Maybe a
primMST graph
  | sz == 0      = Just emptyGraph
  | isBreaking b = Nothing
  | otherwise    = Just t
  where
    sz     = numNodes graph
    (b, t) = runState (primS graph $ \s e w -> do
      tree <- get
      put $ addUWArcs [((s, e), w)] tree
      ) (initUGraph (nodes graph) [])

-- Returns the total weight of the Minimum Spanning Tree via Prim's Algorithm.
-- Pre: The graph is undirected.
primMSTWeights :: Graph a => a -> Maybe Int
primMSTWeights graph
  | sz == 0      = Just 0
  | isBreaking b = Nothing
  | otherwise    = Just t
  where
    sz = numNodes graph
    (b, t) 
      = runState (primS graph $ const (const ((get >>=) . (put .) . (+)))) 0

-- A State that simulates Prim's Algorithm.
-- This function is convoluted and is not necessary unless you need to do custom
-- actions during the formation of the spanning tree.
-- See full documentation in README.md. (TODO)
-- Pre: The graph is undirected.
primS :: (Graph a, Flaggable l) 
  => a 
  -> (Int -> Int -> Int -> State b l) 
  -> State b (Terminate ())
primS graph fun 
  | sz == 0   = continueLoop
  | otherwise = do
    t <- get
    let ((k, _), res) = execState prim' ((fromDescList [root], initAdj), t)
    put res
    breakUpon (sz > size k)
  where
    sz      = numNodes graph
    root    = head $ nodes graph
    initAdj = execState (forM_ (neighbours root graph) $ \s -> do
      fringe <- get
      put $ IM.insert s (fromJust (weight (root, s) graph), root) fringe
      ) empty
    prim'   = forMBreak_ [2..sz] $ \_ -> do
    ((k, f), t) <- get
    breakWhen (IM.null f) $ do
      let minN     = minimumBy ((. (f !)) . compare . (f !)) (keys f)
      let (w, n)   = f ! minN
      let adj      = neighbours minN graph
      let k'       = S.insert minN k
      let (b', t') = runState (fun n minN w) t
      runUnlessBreak b' $ put ((k', execState (forM_ adj $ \s -> do
        fringe <- get
        let newW = fromJust $ weight (minN, s) graph
        continueWhen (S.member s k') $ if IM.notMember s fringe
          then put $ IM.insert s (newW, minN) fringe
          else put $ IM.insert s (min (fringe ! s) (newW, minN)) fringe
        ) $ delete minN f), t')


--------------------------------------------------------------------------------
-- Kruskal's Algorithm
--------------------------------------------------------------------------------

-- Returns the Minimum Spanning Tree via Kruskal's Algorithm.
-- Pre: The graph is undirected.
kruskalMST :: Graph a => a -> Maybe a
kruskalMST graph
  | isBreaking b = Nothing
  | otherwise    = Just t
  where
    (b, t) = runState (kruskalS graph $ \s e w -> do
      tree <- get
      put $ addUWArcs [((s, e), w)] tree
      ) (initUGraph (nodes graph) [])

-- Returns the total weight of the MST via Kruskal's Algorithm.
-- Pre: The graph is undirected.
kruskalMSTWeights :: Graph a => a -> Maybe Int
kruskalMSTWeights graph
  | sz == 0      = Just 0
  | isBreaking b = Nothing
  | otherwise    = Just t
  where
    sz = numNodes graph
    (b, t) 
      = runState (kruskalS graph $ const (const ((get >>=) . (put .) . (+)))) 0

-- A State that simulates Kruskals's Algorithm.
-- This function is convoluted and is not necessary unless you need to do custom
-- actions during the formation of the spanning tree.
-- See full documentation in README.md. (TODO)
-- Pre: The graph is undirected.
kruskalS :: (Graph a, Flaggable l) 
  => a 
  -> (Int -> Int -> Int -> State b l) 
  -> State b (Terminate ())
kruskalS graph fun = do
    t <- get
    let ((acc, _, _), res) = execState kruskal' ((0, arcs, uf), t)
    put res
    breakUpon $ sz > acc + 1
  where
    sz       = numNodes graph
    arcs     = sortBy ((. snd) . compare . snd) $ uArcs graph
    uf       = initUF $ nodes graph
    kruskal' = loop_ $ do
      ((acc, arcs, uf), t) <- get
      breakWhen (acc == sz - 1 || Prelude.null arcs) $ do
        let (((n, n'), w) : as) = arcs
        if getRep n uf == getRep n' uf
          then put ((acc, as, uf), t) >> continueLoop
          else do
            let (b, t') = runState (fun n n' w) t
            runUnlessBreak b $ put ((acc + 1, as, unionFind n n' uf), t')
