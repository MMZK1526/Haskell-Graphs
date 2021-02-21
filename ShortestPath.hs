{-# LANGUAGE TupleSections #-}

-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

module ShortestPath where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Either (Either(..))
import           Data.List (minimumBy)
import           Data.Maybe

-- Requires installation
import           Data.IntMap.Lazy as IM 
 (IntMap(..), notMember, delete, insert, empty, fromList, keys, null, (!), (!?))
import           Data.Map as M (Map(..), empty, insert, (!?))
import           Data.Set as S (fromList, insert, member)

import           Graph
import           Utilities

-- Returns an IntMap of nodes reachable from the given root and their distances.
-- Pre: The root is in the graph.
shortestDistances :: (Graph a) => Int -> a -> IntMap Int
shortestDistances n graph
  = execState (dijkstraS n graph sp) IM.empty
  where
    sp _ n d = get >>= put . IM.insert n d

-- Returns the shortest distance between two nodes as well the path,
-- or Nothing if disconnected.
-- Pre: The nodes are in the graph.
shortestDistance :: (Graph a) => Int -> Int -> a -> Maybe (Int, [(Int, Int)])
shortestDistance = shortestDistanceWithHeuristic $ const 0

-- Returns the shortest distance between two nodes as well the path,
-- or Nothing if disconnected, with the aid of an heuristic function.
-- Pre: The nodes are in the graph and the heuristic is consitent.
shortestDistanceWithHeuristic :: (Graph a) 
  => (Int -> Int) 
  -> Int 
  -> Int 
  -> a 
  -> Maybe (Int, [(Int, Int)])
shortestDistanceWithHeuristic heuristic n n' graph
  = liftM2 (,) (dMap IM.!? n') (reverse <$> process path)
  where
    (dMap, path) = execState (aStarS n graph sp heuristic) (IM.empty, IM.empty)
    sp n1 n2 d = do
      (dMap, path) <- get
      put (IM.insert n2 d dMap, IM.insert n2 n1 path)
      breakUpon $ n2 == n'
    processS   = do
      curN <- get
      if curN == n
        then return []
        else do
          let prevN = path ! curN
          put prevN
          rest <- processS
          return $ (prevN, curN) : rest
    process path 
      = path IM.!? n' >> Just (evalState processS n')

foo n n' graph = dMap
  where
    (dMap, path) = execState (dijkstraS n graph sp) (IM.empty, IM.empty)
    sp n1 n2 d = do
      (dMap, path) <- get
      put (IM.insert n2 d dMap, IM.insert n2 n1 path)
      breakUpon $ n2 == n'

-- Returns the directed unweighted shortest distance spanning tree from
-- a given root to all reachable nodes.
-- Pre: The root is in the graph.
shortestDistanceSpanningTree :: (Graph a) => Int -> a -> a
shortestDistanceSpanningTree n graph
  = execState (dijkstraS n graph sp) $ initGraph (nodes graph) []
  where
    sp n n' _ 
      | n == n'   = return ()
      | otherwise = get >>= put . addArcs [(n, n')]

-- A State that simulates Dijkstra's Algorithm.
-- This function is convoluted and is not necessary unless you need to do custom
-- actions during the formation of the shortest path spanning tree.
-- See full documentation in README.md. (TODO)
-- Pre: The graph contains no negative cycles.
dijkstraS :: (Graph a, Flaggable l) 
  => Int 
  -> a 
  -> (Int -> Int -> Int -> State b l) 
  -> State b (Terminate ())
dijkstraS = flip flip (const 0) . (flip .) . aStarS

-- A State that simulates A* Algorithm.
-- This function is convoluted and is not necessary unless you need to do custom
-- actions during the formation of the shortest path spanning tree.
-- See full documentation in README.md.
-- Pre: The graph contains no negative cycles.
aStarS :: (Graph a, Flaggable l) 
  => Int 
  -> a 
  -> (Int -> Int -> Int -> State b l) 
  -> (Int -> Int)
  -> State b (Terminate ())
aStarS root graph fun heuristic = do
  t <- get
  let (b, t') = runState (fun root root 0) t
  put t'
  runUnlessBreak b $ do
    t <- get
    let (_, res) = execState aStar' ((S.fromList [root], initAdj), t)
    put res
  return $ flag b
  where
    initAdj = execState (forM_ (neighbours root graph) $ \s -> 
      get >>= put . IM.insert s (fromJust (weight (root, s) graph), root)
      ) IM.empty
    aStar'  = loop_ $ do
      ((v, f), t) <- get
      breakWhen (IM.null f) $ do
        let minN     = minimumBy (comparator f) (keys f)
        let (d, n)   = f ! minN
        let adj      = neighbours minN graph
        let v'       = S.insert minN v
        let (b', t') = runState (fun n minN d) t
        put ((v', execState (forM_ adj $ \s -> do
          fringe <- get
          let newW = fromJust $ weight (minN, s) graph
          continueWhen (S.member s v') $ if IM.notMember s fringe
            then put $ IM.insert s (d + newW, minN) fringe
            else put $ IM.insert s (min (fringe ! s) (d + newW, minN)) fringe
          ) $ delete minN f), t')
        return b'
    comparator f n n'
      = compare (fst (f ! n) + heuristic n) (fst (f ! n') + heuristic n') 

-- Computing the shortest path between all pairs of nodes in a graph.
-- The result contains a map from tuples of nodes (start, end) to tuples of 
-- the distance from start to end, and the next node on this path since start. 
-- For the paths already reaching the destination, next is itself.
-- Pre: The graph contains no negative cycles.
shortestPathsFully :: (Graph a) => a -> Map (Int, Int) (Maybe (Int, Int))
shortestPathsFully graph
  = execState (floydWarshallS starter f graph) M.empty
  where
    starter i j
      | i /= j    = get >>= 
        put . M.insert (i, j) ((, j) <$> (weight (i, j) graph))
      | otherwise = get >>= put . M.insert (i, i) (Just (0, i))
    f i j k =
      continueWhen (i == k) $ do
        t <- get
        let cur = join $ t M.!? (i, j)
        let ik  = join $ t M.!? (i, k)
        let val = liftM2 (+) (fst <$> ik) (fst <$> join (t M.!? (k, j)))
        let new = liftM2 (,) val (snd <$> ik)
        put $ M.insert (i, j) (minMaybeOn fst cur new) t

-- Computing the max bandwith between all pairs of distinct nodes in a graph.
-- The result contains a map from tuples of nodes (start, end) to tuples of 
-- the bandwith from start to end, and the next node on this path since start. 
-- For the paths already reaching the destination, next is itself.
-- Pre: The graph contains no negative cycles.
bandwithFully :: (Graph a) => a -> Map (Int, Int) (Maybe (Int, Int))
bandwithFully graph
  = execState (floydWarshallS starter f graph) M.empty
  where
    starter i j
      = continueWhen (i == j) $ 
      get >>= put . M.insert (i, j) ((, j) <$> (weight (i, j) graph))
    f i j k 
      = continueWhen (i == j || i == k) $ do
        t <- get
        let cur = join $ t M.!? (i, j)
        let ik  = join $ t M.!? (i, k)
        let val = liftM2 min (fst <$> ik) (fst <$> join (t M.!? (k, j)))
        let new = liftM2 (,) val (snd <$> ik)
        put $ M.insert (i, j) (maxMaybeOn fst cur new) t

-- Computing the closure of a graph.
-- The result is unweighted.
graphClosure :: (Graph a) => a -> a
graphClosure graph
  = simplify $ execState (floydWarshallS starter f graph) graph
  where
    starter = const $ const $ return ()
    f i j k = do
      g <- get
      if isJust (weight (i, k) g) && isJust (weight (k, j) g)
        then put $ addArcs [(i, j)] g
        else return ()

-- A State that simulates the bare-bones of Floyd-Warshall Algorithm
-- See full documentation in README.md.
floydWarshallS :: (Graph a, Flaggable l1, Flaggable l2) 
  => (Int -> Int -> State b l1) 
  -> (Int -> Int -> Int -> State b l2) 
  -> a 
  -> State b (Terminate ())
floydWarshallS starter f graph = do
  t <- get
  let ns = nodes graph
  b <- forMBreak_ ns $ \x -> forM_ ns $ \y -> starter x y
  b <- runUnlessBreak b $
    forMBreak_ ns $ \z -> forM_ ns $ \x -> forM_ ns $ \y -> f x y z
  return $ flag b
