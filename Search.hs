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


-- Type alias
type SearchResult a = (IntMap Int, a)


-- Functions

-- A State that simulates Depth-First Search.
-- This function is convoluted and is not necessary unless you need to do custom
-- actions during the Depth-First Search.
-- See full documentation in README.md.
depthFirstS 
  :: Graph a 
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
    runWhenJust mb (\a -> if S.member x nIn
      then return ()
      else do
        let a' = execState (fEnter x) (Just a)
        put ((S.insert x nIn, nOut), a')
        forM_ (neighbours x graph) (\y -> if S.member y nIn
          then if not $ S.member y nOut || allowCycle
            then put ((S.insert x nIn, nOut), Nothing)
            else return ()
          else dfs y
          )
        ((nIn, nOut), mb) <- get
        runWhenJust mb (\a -> do
          let a' = execState (fExit x) (Just a)
          put ((nIn, S.insert x nOut), a')
          )
      )

-- Traverses the graph using Depth-First Search from a given node
-- and returns the list of passed nodes and their depths
-- Pre: The given node is in the graph.
depthFirstNodes :: Graph a => Int -> a -> [(Int, Int)]
depthFirstNodes n graph
  = snd $ fromJust (snd (execState (dfn n) initial))
  where
    -- Contains sets of visited nodes (first pass), exited nodes (last pass),
    -- and a list of nodes with their depth. Starts with all empty.
    initial = ((fromDescList [], fromDescList []), Just (0, []))
    dfn x   = depthFirstS x graph True (\n -> do
      raw <- get
      let (d, ns) = fromJust raw
      put $ Just (d + 1, (n, d) : ns)
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

-- Topological sorting of a directed acyclic graph.
-- If the graph contains a cycle, will return nothing
topologicalSort :: Graph a => a -> Maybe [Int]
topologicalSort graph
  = F.toList <$> (snd (execState (forM_ (nodes graph) tSortS) initial))
  where
    -- Contains sets of visited nodes (first pass), exited nodes (last pass),
    -- and a list of topologically ordered nodes. Starts with all empty.
    initial  = ((fromDescList [], fromDescList []), Just $ fromList [])
    -- Runs the Depth-First Search on each of the nodes.
    tSortS x = depthFirstS x graph False (const $ return ()) (\n -> do
      raw <- get
      put $ Just (insertAt 0 n $ fromJust raw)
      )

-- Traverses the graph using Breadth-First Search from a given node
-- and returns the corresponding spanning tree.
-- Pre: The given node is in the graph.
breadthFirstTree :: Graph a => Int -> a -> a
breadthFirstTree = (snd .) . breadthFirst

-- Traverses the graph using Breadth-First Search from a given node
-- and returns the list of passed nodes and their depths
-- Pre: The given node is in the graph.
breadthFirstNodes :: Graph a => Int -> a -> [(Int, Int)]
breadthFirstNodes = ((IM.toList . fst) .) . breadthFirst

-- Traverses the graph using Breadth-First Search from a given node
-- Returns a tuple containing a map <node, depth> and
-- the corresponding spanning tree.
-- Pre: The given node is in the graph.
breadthFirst :: Graph a => Int -> a -> SearchResult a
breadthFirst n graph
  = bfs (fromAscList [(n, 0)]) (initGraph (nodes graph) []) (fromList [n])
  where
    bfs dMap g Empty
      = (dMap, g)
    bfs dMap g (q :<| qs)
      = bfs d' g' (qs >< adj)
      where
        (d', g', adj) 
          = execState (forM_ (neighbours q graph) updateS) (dMap, g, empty)
        depth     = dMap ! q + 1
        updateS e = do
          (m, g, a) <- get
          if IM.member e m
            then return ()
            else
              put (IM.insert e depth m, addUArcs [(q, e)] g, insertAt 0 e a)

-- Returns True if the graph is connected.
isConnected :: Graph a => a -> Bool
isConnected graph
  = (sz == 0) || (expand (fromDescList [n]) (fromList [n]) == sz)
  where
    sz = numNodes graph
    n  = head $ nodes graph
    expand ns Empty
      = size ns
    expand ns (q :<| qs)
      = expand ns' (qs >< adj)
      where
        (ns', adj) 
          = execState (forM_ (neighbours q graph) updateS) (ns, empty)
        updateS e = do
          (m, a) <- get
          if S.member e m
            then return ()
            else put (S.insert e m, insertAt 0 e a)

-- Returns the distance between two nodes. If unreachable returns Nothing.
-- Pre: The given nodes are in the graph.
distance :: Graph a => Int -> Int -> a -> Maybe Int
distance = flip . (((!?) . fst) .) . breadthFirst
