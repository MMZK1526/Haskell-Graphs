-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs

module Search where

-- Implements Depth-First Search and Breadth-First Search.
-- Checks connectivity of any given graph.
-- Implements topological sorting over any direct acyclic graph.

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable as F
import           Data.Maybe (fromJust, isNothing)

-- May require installation
import           Data.IntMap.Lazy as IM
  (IntMap(..), fromAscList, insert, keys, lookupMin, member, notMember, toList
  , (!), (!?)
  )
import           Data.Sequence hiding (length, null, (!?))
import           Data.Set as S (Set(..), fromDescList, insert, member, size)
import           Prelude hiding (filter)

import           Graph


-- Functions

-- A State that simulates Depth-First Search.
-- This function is convoluted and is not necessary unless you need to do custom
-- actions during the Depth-First Search.
-- See full documentation in README.md.
depthFirstS :: Graph a 
  => Int 
  -> a 
  -> Bool 
  -> (Int -> State (Maybe b) ()) 
  -> (Int -> State (Maybe b) ()) 
  -> State ((Set Int, Set Int), (Maybe b)) ()
depthFirstS x graph allowCycle fEnter fExit
  = dfs x
  where
    dfs x = do
    ((nIn, nOut), mb) <- get
    -- runWhenJust returns () when the input is Nothing, and applies the
    -- following function when the input is a Just.
    runWhenJust mb (if S.member x nIn
      then return ()
      else do
        put ((S.insert x nIn, nOut), execState (fEnter x) mb)
        forM_ (neighbours x graph) (\y -> if S.member y nIn
          then if not $ S.member y nOut || allowCycle
            then put ((S.insert x nIn, nOut), Nothing)
            else return ()
          else dfs y
          )
        ((nIn, nOut), mb) <- get
        runWhenJust mb (put ((nIn, S.insert x nOut), execState (fExit x) mb))
      )

-- Traverses the graph using Depth-First Search from a given node
-- and returns the list of passed nodes and their depths.
-- Pre: The given node is in the graph.
depthFirstNodes :: Graph a => Int -> a -> IntMap Int
depthFirstNodes n graph
  = snd $ fromJust (snd (execState (dfn n) initial))
  where
    -- Contains sets of visited nodes (first pass), exited nodes (last pass),
    -- and a list of nodes with their depth. Starts with all empty.
    initial = ((fromDescList [], fromDescList []), Just (0, fromAscList []))
    dfn x   = depthFirstS x graph True (\n -> do
      raw <- get
      let (d, ns) = fromJust raw
      put $ Just (d + 1, IM.insert n d ns)
      ) (const $ do
      raw <- get
      let (d, ns) = fromJust raw
      put $ Just (d - 1, ns)
      )

-- Traverses the graph using Depth-First Search from a given node
-- and returns the corresponding spanning tree.
-- Pre: The given node is in the graph.
depthFirstTree :: Graph a => Int -> a -> a
depthFirstTree n graph
  = snd $ fromJust (snd (execState (dfn n) initial))
  where
    -- Contains sets of visited nodes (first pass), exited nodes (last pass),
    -- and a graph that builds towards the spanning tree.
    initial    = ((fromDescList [], fromDescList []), Just ([], startGraph))
    startGraph = initGraph (nodes graph) []
    dfn x      = depthFirstS x graph True (\n -> do
      raw <- get
      let (st, g) = fromJust raw
      if null st
        then put $ Just (n : st, g)
        else put $ Just (n : st, addUArcs [(head st, n)] g)
      ) (const $ do
      raw <- get
      let (st, g) = fromJust raw
      put $ Just (tail st, g)
      )

-- Topological sorting of a directed acyclic graph (DAG).
-- If the graph contains a cycle, will return Nothing.
topologicalSort :: Graph a => a -> Maybe [Int]
topologicalSort graph
  = F.toList <$> (snd (execState (forM_ (nodes graph) tSortS) initial))
  where
    initial  = ((fromDescList [], fromDescList []), Just $ fromList [])
    -- Runs the Depth-First Search on each of the nodes.
    tSortS x = depthFirstS x graph False (const $ return ()) (\n -> do
      raw <- get
      put $ Just (insertAt 0 n $ fromJust raw)
      )

-- A State that simulates Breadth-First Search.
-- This function is convoluted and is not necessary unless you need to do custom
-- actions during the Breadth-First Search.
-- See full documentation in README.md.
breadthFirstS :: Graph a 
  => Int 
  -> a 
  -> (Int -> State (Maybe b) ()) 
  -> (Int -> State (Maybe b) ()) 
  -> State ((Set Int, Seq Int), (Maybe b)) ()
breadthFirstS x graph fEnter fExit = do
  ((nIn, queue), mb) <- get
  if S.member x nIn
    then return ()
    else do
      put ((S.insert x nIn, insertAt 0 x queue), execState (fEnter x) mb)
      bfs
  where
    bfs = do
      ((nIn, queue), mb) <- get
      if queue == empty
        then return ()
        else do
          let (q :<| qs) = queue
          runWhenJust mb (do
            put ((nIn, queue), execState (fExit q) mb)
            )
          ((nIn, queue), mb) <- get
          runWhenJust mb (do
            put ((nIn, qs), mb)
            forM_ (neighbours q graph) (\x -> do
              ((nIn, qs), mb) <- get
              runWhenJust mb (if S.member x nIn
                then return ()
                else put ((S.insert x nIn, qs |> x), execState (fEnter x) mb)
                )
              )
            )
          bfs

-- Traverses the graph using Breadth-First Search from a given node
-- and returns the list of passed nodes and their depths
-- Pre: The given node is in the graph.
breadthFirstNodes :: Graph a => Int -> a -> IntMap Int
breadthFirstNodes n graph
  = snd $ fromJust (snd (execState (bfs n) initial))
  where
    initial = ((fromDescList [], fromList []), Just (0, fromAscList []))
    bfs x   = breadthFirstS x graph (\n -> do
      raw <- get
      let (d, ns) = fromJust raw
      put $ Just (d, IM.insert n d ns)
      ) (\n -> do
      raw <- get
      let (_, ns) = fromJust raw
      put $ Just (ns ! n + 1, ns)
      )

-- Traverses the graph using Breadth-First Search from a given node
-- and returns the corresponding spanning tree.
-- Pre: The given node is in the graph.
breadthFirstTree :: Graph a => Int -> a -> a
breadthFirstTree n graph
  = snd $ fromJust (snd (execState (bfs n) initial))
  where
    -- The information contains a Maybe Int that stores the potential parent,
    -- which is Nothing for the root, and a graph that builds towards the tree.
    initial    = ((fromDescList [], fromList []), Just (Nothing, startGraph))
    startGraph = initGraph (nodes graph) []
    bfs x      = breadthFirstS x graph (\n -> do
      raw <- get
      let (parent, g) = fromJust raw
      if isNothing parent
        then return ()
        else put $ Just (parent, addUArcs [(fromJust parent, n)] g)
      ) (\n -> do
      raw <- get
      let (_, g) = fromJust raw
      put $ Just (Just n, g)
      )

-- Returns True if the graph is connected.
isConnected :: Graph a => a -> Bool
isConnected graph
  = (sz == 0) || (snd (execState (bfs (head $ nodes graph)) initial) == Just sz)
  where
    sz      = numNodes graph
    initial = ((fromDescList [], fromList []), Just 0)
    bfs n   = breadthFirstS n graph (\n -> do
      raw <- get
      put $ (+ 1) <$> raw
      ) (const $ return ())

-- Returns the (unweighted) distance between two nodes;  
-- If unreachable returns Nothing.  
-- Pre: The given nodes are in the graph.  
distance :: Graph a => Int -> Int -> a -> Maybe Int
distance = flip . ((!?) .) . breadthFirstNodes
