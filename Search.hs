-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

module Search where

-- Implements Depth-First Search and Breadth-First Search.
-- Checks connectivity of any given graph.
-- Implements topological sorting over any direct acyclic graph.

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Maybe (fromJust, isNothing)

-- May require installation
import           Data.IntMap.Lazy as IM
  (IntMap(..), empty, insert, member, (!), (!?))
import           Data.Sequence hiding (length, null, (!?))
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
depthFirstS :: Graph a 
  => Int 
  -> a 
  -> Bool 
  -> (Int -> State (Terminate b) ()) 
  -> (Int -> State (Terminate b) ()) 
  -> State ((Set Int, Set Int), (Terminate b)) ()
depthFirstS x graph allowCycle fEnter fExit
  = dfs x
  where
    dfs x = do
    ((nIn, nOut), tb) <- get
    runUntilBreak tb $ if S.member x nIn
      then return ()
      else do
        put ((S.insert x nIn, nOut), execState (fEnter x) tb)
        forM_ (neighbours x graph) $ \y -> if S.member y nIn
          then if not $ S.member y nOut || allowCycle
            then put ((S.insert x nIn, nOut), terminate tb)
            else return ()
          else dfs y
        ((nIn, nOut), tb) <- get
        runUntilBreak tb $ put ((nIn, S.insert x nOut), execState (fExit x) tb)

-- Traverses the graph using Depth-First Search from a given node
-- and returns the list of passed nodes and their depths.
-- Pre: The given node is in the graph.
depthFirstNodes :: Graph a => Int -> a -> IntMap Int
depthFirstNodes n graph
  = snd $ information (snd (execState (dfs n) initial))
  where
    -- Contains sets of visited nodes (first pass), exited nodes (last pass),
    -- and a list of nodes with their depth. Starts with all empty.
    initial = ((S.empty, S.empty),  return (0, IM.empty))
    dfs x   = depthFirstS x graph True (\n -> do
      raw <- get
      let (d, ns) = information raw
      put $ return (d + 1, IM.insert n d ns)
      ) $ \_ -> do
      raw <- get
      let (d, ns) = information raw
      put $ return (d - 1, ns)

-- Traverses the graph using Depth-First Search from a given node
-- and returns the corresponding spanning tree.
-- Pre: The given node is in the graph.
depthFirstTree :: Graph a => Int -> a -> a
depthFirstTree n graph
  = snd $ information (snd (execState (dfs n) initial))
  where
    -- Contains sets of visited nodes (first pass), exited nodes (last pass),
    -- and a graph that builds towards the spanning tree.
    initial    = ((S.empty, S.empty), return ([], startGraph))
    startGraph = initGraph (nodes graph) []
    dfs x      = depthFirstS x graph True (\n -> do
      raw <- get
      let (st, g) = information raw
      put $ if null st
        then return (n : st, g)
        else return (n : st, addUArcs [(head st, n)] g)
      ) $ \_ -> do
      raw <- get
      let (st, g) = information raw
      put $ return (tail st, g)

-- Topological sorting of a directed acyclic graph (DAG).
-- If the graph contains a cycle, will return Nothing.
topologicalSort :: Graph a => a -> Maybe [Int]
topologicalSort graph
  | isBreaking result = Nothing
  | otherwise         = Just $ information result
  where
    result   = snd (execState (forM_ (nodes graph) tSortS) initial)
    initial  = ((S.empty, S.empty), return [])
    -- Runs the Depth-First Search on each of the nodes.
    tSortS x = depthFirstS x graph False (const $ return ()) $ \n -> do
      raw <- get
      put $ return (n : information raw)

-- A State that simulates Breadth-First Search.
-- This function is convoluted and is not necessary unless you need to do custom
-- actions during the Breadth-First Search.
-- See full documentation in README.md.
breadthFirstS :: Graph a 
  => Int 
  -> a 
  -> (Int -> State (Terminate b) ()) 
  -> (Int -> State (Terminate b) ()) 
  -> State ((Set Int, Seq Int), (Terminate b)) ()
breadthFirstS x graph fEnter fExit = do
  ((nIn, queue), tb) <- get
  if S.member x nIn
    then return ()
    else do
      put ((S.insert x nIn, x <| queue), execState (fEnter x) tb)
      bfs
  where
    bfs = do
      ((nIn, queue), tb) <- get
      case queue of
        Empty      -> return ()
        (q :<| qs) -> do
          put ((nIn, qs), tb)
          runUntilBreak tb $ put ((nIn, qs), execState (fExit q) tb)
          forM_ (neighbours q graph) $ \x -> do
            ((nIn, qs), tb) <- get
            runUntilBreak tb $ if S.member x nIn
              then return ()
              else put ((S.insert x nIn, qs |> x), execState (fEnter x) tb)
          bfs

-- Traverses the graph using Breadth-First Search from a given node
-- and returns the list of passed nodes and their depths
-- Pre: The given node is in the graph.
breadthFirstNodes :: Graph a => Int -> a -> IntMap Int
breadthFirstNodes n graph
  = snd $ information (snd (execState (bfs n) initial))
  where
    initial = ((S.empty, fromList []), return (0, IM.empty))
    bfs x   = breadthFirstS x graph (\n -> do
      raw <- get
      let (d, ns) = information raw
      put $ return (d, IM.insert n d ns)
      ) $ \n -> do
      raw <- get
      let (_, ns) = information raw
      put $ return (ns ! n + 1, ns)

-- Traverses the graph using Breadth-First Search from a given node
-- and returns the corresponding spanning tree.
-- Pre: The given node is in the graph.
breadthFirstTree :: Graph a => Int -> a -> a
breadthFirstTree n graph
  = snd $ information (snd (execState (bfs n) initial))
  where
    -- The information contains a Maybe Int that stores the potential parent,
    -- which is Nothing for the root, and a graph that builds towards the tree.
    initial    = ((S.empty, fromList []), return (Nothing, startGraph))
    startGraph = initGraph (nodes graph) []
    bfs x      = breadthFirstS x graph (\n -> do
      raw <- get
      let (p, g) = information raw
      runWhenJust p $ put (return (p, addUArcs [(fromJust p, n)] g))
      ) $ \n -> do
      raw <- get
      let (_, g) = information raw
      put $ return (Just n, g)

-- Returns True if the undirected graph is connected.  
-- Pre: The graph is undirected.  
isConnected :: Graph a => a -> Bool
isConnected graph
  = (sz == 0) || (information (snd (execState (bfs node) initial)) == sz)
  where
    sz      = numNodes graph
    node    = head $ nodes graph
    initial = ((S.empty, fromList []), return 0)
    bfs n   = breadthFirstS n graph (\n -> do
      raw <- get
      put $ (+ 1) <$> raw
      ) (const $ return ())

-- Returns True if the directed graph is strongly connected.
-- Algorithm: First check if every non-root node can reach the root (randomly 
-- picked), then check if the all nodes are reachable from the root.
isStronglyConnected :: Graph a => a -> Bool
isStronglyConnected graph
  | numNodes graph == 0 = True
  | otherwise           = not (isBreaking traceBack) && isConnected graph
  where
    inner     = (S.empty, fromList [])
    -- h is the root while t contains all the non-root nodes.
    (h : t)   = nodes graph
    -- initial holds the information of all nodes that can reach to the root.
    initial   = (inner, return $ fromDescList [h])
    -- If not strongly connected, traceBack returns Terminate True ().
    traceBack = evalState (forMBreak_ t $ \x -> do
      raw <- get
      let (_, info) = execState (bfs x) raw
      -- If the search terminated early, it means this node reaches a node that
      -- is known to be connected to the root. Otherwise it is not connected.
      if isBreaking info
        then do
          put (inner, info)
          continueLoop
        else breakLoop
      ) initial
    -- The search terminates when the frontier contains a node that is in the
    -- information, which means it can lead to the root.
    bfs x     = breadthFirstS x graph (\n -> do
      raw <- get
      put $ if S.member n (information raw)
        then terminate raw
        else S.insert n <$> raw
      ) (const $ return ())
      

-- Returns the (unweighted) distance between two nodes;  
-- If unreachable returns Nothing.  
-- Pre: The given nodes are in the graph.  
distance :: Graph a => Int -> Int -> a -> Maybe Int
distance = flip . ((!?) .) . breadthFirstNodes
