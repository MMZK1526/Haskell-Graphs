{-# LANGUAGE TupleSections #-}

-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

module Search where

-- Implements Depth-First Search and Breadth-First Search.
-- Checks connectivity of any given graph.
-- Implements topological sorting over any direct acyclic graph.

import           Control.Monad
import Control.Monad.Trans.State
    (State, evalState, execState, get, put, runState)
import           Data.Maybe (fromJust, isNothing)

-- Require installation
import           Data.IntMap.Lazy as IM
  (IntMap(..), empty, insert, member, (!), (!?))
import           Data.Sequence (Seq((:<|), Empty), fromList, (<|), (|>))
import           Data.Set as S (Set(..), empty, fromDescList, insert, member)

import           Graph
import           Utilities


--------------------------------------------------------------------------------
-- DFS, BFS & Their Applications
--------------------------------------------------------------------------------

-- A State that simulates Depth-First Search.
-- This function is convoluted and is not necessary unless you need to do custom
-- actions during the Depth-First Search.
-- See full documentation in README.md.
depthFirstS :: (Graph a, Flaggable l1, Flaggable l2)
  => Int 
  -> a 
  -> Bool 
  -> (Int -> State b l1) 
  -> (Int -> State b l2) 
  -> State ((Set Int, Set Int), b) (Terminate ())
depthFirstS x graph allowCycle fEnter fExit
  = dfs continueFlag x
  where
    dfs b x = runUnlessBreak b $ do
      ((nIn, nOut), t) <- get
      continueWhen_ (S.member x nIn) $ do
        let (b', res) = runState (fEnter x) t
        put ((S.insert x nIn, nOut), res)
        b' <- forMBreak_ (neighbours x graph) $ \y -> if S.member y nIn
          then breakUpon (not $ S.member y nOut || allowCycle)
          else dfs (flag b') y >>= return . flag
        ((nIn, nOut), t) <- get
        runUnlessBreak b' $ do 
          let (b', res) = runState (fExit x) t
          put ((nIn, S.insert x nOut), res)
          return $ flag b'

-- Traverses the graph using Depth-First Search from a given node
-- and returns the list of passed nodes and their depths.
-- Pre: The given node is in the graph.
depthFirstNodes :: Graph a => Int -> a -> IntMap Int
depthFirstNodes n graph
  = snd $ snd (execState (dfs n) initial)
  where
    -- Contains sets of visited nodes (first pass), exited nodes (last pass),
    -- and a list of nodes with their depth. Starts with all empty.
    initial = ((S.empty, S.empty), (0, IM.empty))
    dfs x   = depthFirstS x graph True (\n -> do
      (d, ns) <- get
      put (d + 1, IM.insert n d ns)
      ) $ \_ -> do
      (d, ns) <- get
      put (d - 1, ns)

-- Traverses the graph using Depth-First Search from a given node
-- and returns the corresponding spanning tree.
-- Pre: The given node is in the graph.
depthFirstTree :: Graph a => Int -> a -> a
depthFirstTree n graph
  = snd $ snd (execState (dfs n) initial)
  where
    -- Contains sets of visited nodes (first pass), exited nodes (last pass),
    -- and a graph that builds towards the spanning tree.
    initial    = ((S.empty, S.empty), ([], startGraph))
    startGraph = initGraph (nodes graph) []
    dfs x      = depthFirstS x graph True (\n -> do
      (st, g) <- get
      put $ if null st
        then (n : st, g)
        else (n : st, addUArcs [(head st, n)] g)
      ) $ \_ -> do
      (st, g) <- get
      put (tail st, g)

-- Topological sorting of a directed acyclic graph (DAG).
-- If the graph contains a cycle, will return Nothing.
topologicalSort :: Graph a => a -> Maybe [Int]
topologicalSort graph
  | isBreaking b = Nothing
  | otherwise    = Just $ snd res
  where
    (b, res) = runState (forMBreak_ (nodes graph) tSortS) initial
    initial  = ((S.empty, S.empty), [])
    -- Runs the Depth-First Search on each of the nodes.
    tSortS x = depthFirstS x graph False (const continueLoop) $ \n -> do
      ns <- get
      put (n : ns)

-- A State that simulates Breadth-First Search.
-- This function is convoluted and is not necessary unless you need to do custom
-- actions during the Breadth-First Search.
-- See full documentation in README.md.
breadthFirstS :: (Graph a, Flaggable l1, Flaggable l2) 
  => Int 
  -> a 
  -> (Int -> State b l1) 
  -> (Int -> State b l2) 
  -> State ((Set Int, Seq Int), b) (Terminate ())
breadthFirstS x graph fEnter fExit = do
  ((nIn, queue), t) <- get
  if S.member x nIn
    then continueLoop
    else do
      let (b', res) = runState (fEnter x) t
      put ((S.insert x nIn, x <| queue), res)
      bfs $ flag b'
  where
    bfs b = do
      ((nIn, queue), t) <- get
      runUnlessBreak b $ case queue of
        Empty      -> continueLoop
        (q :<| qs) -> do
          let (b', res) = runState (fExit q) t
          put ((nIn, qs), res)
          b' <- forMBreak_ (neighbours q graph) 
            $ \x -> runUnlessBreak b' $ continueWhen_ (S.member x nIn) $ do 
            ((nIn, qs), t) <- get
            let (b', res) = runState (fEnter x) t
            put ((S.insert x nIn, qs |> x), res)
            return $ flag b'
          bfs $ flag b'

-- Traverses the graph using Breadth-First Search from a given node
-- and returns the list of passed nodes and their depths.
-- Pre: The given node is in the graph.
breadthFirstNodes :: Graph a => Int -> a -> IntMap Int
breadthFirstNodes n graph
  = snd $ snd (execState (bfs n) initial)
  where
    initial = ((S.empty, fromList []), (0, IM.empty))
    bfs x   = breadthFirstS x graph (\n -> do
      (d, ns) <- get
      put (d, IM.insert n d ns)
      ) $ \n -> do
      (_, ns) <- get
      put (ns ! n + 1, ns)

-- Traverses the graph using Breadth-First Search from a given node
-- and returns the corresponding spanning tree.
-- Pre: The given node is in the graph.
breadthFirstTree :: Graph a => Int -> a -> a
breadthFirstTree n graph
  = snd $ snd (execState (bfs n) initial)
  where
    -- The information contains a Maybe Int that stores the potential parent,
    -- which is Nothing for the root, and a graph that builds towards the tree.
    initial    = ((S.empty, fromList []), (Nothing, startGraph))
    startGraph = initGraph (nodes graph) []
    bfs x      = breadthFirstS x graph (\n -> do
      (p, g) <- get
      runWhenJust p $ put (p, addUArcs [(fromJust p, n)] g)
      ) $ \n -> get >>= put . (Just n ,) . snd

-- Returns True if the undirected graph is connected.  
-- Pre: The graph is undirected.  
isConnected :: Graph a => a -> Bool
isConnected graph
  = (sz == 0) || (snd (execState (bfs node) initial) == sz)
  where
    sz      = numNodes graph
    node    = head $ nodes graph
    initial = ((S.empty, fromList []), 0)
    bfs n   = breadthFirstS n graph (\n -> do
      c <- get
      put $ c + 1
      continueLoop
      ) (const continueLoop)

-- Returns True if the directed graph is strongly connected.
-- Algorithm: First check if every non-root node can reach the root (randomly 
-- picked), then check if the all nodes are reachable from the root.
isStronglyConnected :: Graph a => a -> Bool
isStronglyConnected graph
  | null $ nodes graph = True
  | otherwise          = not (isBreaking traceBack) && isConnected graph
  where
    inner     = (S.empty, fromList [])
    -- h is the root while t contains all the non-root nodes.
    (h : t)   = nodes graph
    -- initial holds the information of all nodes that can reach to the root.
    initial   = (inner, fromDescList [h])
    -- If not strongly connected, traceBack returns Terminate True ().
    traceBack = evalState (forMBreak_ t $ \x -> do
      (_, ns) <- get
      -- If the search terminated early, it means this node reaches a node that
      -- is known to be connected to the root. Otherwise it is not connected.
      breakUpon (not $ isBreaking (evalState (bfs x) (inner, ns)))
      ) initial
    -- The search terminates when the frontier contains a node that is in the
    -- information, which means it can lead to the root.
    bfs x     = breadthFirstS x graph (\n -> do
      ns <- get
      breakWhen_ (S.member n ns) $ put (S.insert n ns) >> continueLoop
      ) (const continueLoop)
      
-- Returns the (unweighted) distance between two nodes;  
-- If unreachable returns Nothing.  
-- Pre: The given nodes are in the graph.  
distance :: Graph a => Int -> Int -> a -> Maybe Int
distance = flip . ((!?) .) . breadthFirstNodes
