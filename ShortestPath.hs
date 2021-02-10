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
import           Data.Set as S (fromList, insert, member)

import           Graph
import           Utilities

infinity :: Int
infinity = maxBound

-- Returns an IntMap of nodes reachable from the given root and their distances.
-- Pre: The root is in the graph.
shortestDistances :: (Graph a) => Int -> a -> IntMap Int
shortestDistances n graph
  = execState (dijkstraS n graph sp) empty
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
  = liftM2 (,) (dMap !? n') (reverse <$> process path)
  where
    (dMap, path) = execState (aStarS n graph sp heuristic) (empty, empty)
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
      = path !? n' >> Just (evalState processS n')

foo n n' graph = dMap
  where
    (dMap, path) = execState (dijkstraS n graph sp) (empty, empty)
    sp n1 n2 d = do
      (dMap, path) <- get
      put (IM.insert n2 d dMap, IM.insert n2 n1 path)
      breakUpon $ n2 == n'

-- Returns the directed unweighted shortest distance spanning tree from
-- a given root to all reachable nodes
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
  -> State b ()
dijkstraS = flip flip (const 0) . (flip .) . aStarS

-- A State that simulates A* Algorithm.
-- This function is convoluted and is not necessary unless you need to do custom
-- actions during the formation of the shortest path spanning tree.
-- See full documentation in README.md. (TODO)
-- Pre: The graph contains no negative cycles.
aStarS :: (Graph a, Flaggable l) 
  => Int 
  -> a 
  -> (Int -> Int -> Int -> State b l) 
  -> (Int -> Int)
  -> State b ()
aStarS root graph fun heuristic = do
  t <- get
  let (b, t') = runState (fun root root 0) t
  put t'
  runUnlessBreak b $ do
    t <- get
    let (_, res) = execState aStar' ((S.fromList [root], initAdj), t)
    put res
  return ()
  where
    initAdj = execState (forM_ (neighbours root graph) $ \s -> 
      get >>= put . IM.insert s (fromJust (weight (root, s) graph), root)
      ) empty
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
