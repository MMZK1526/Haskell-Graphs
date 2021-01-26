-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs

module Search where

-- Implements Depth-First Search and Breadth-First Search on adjacency list.
-- All graphs are undirected here.

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Maybe (fromJust, isNothing)

-- May require installation
import           Data.IntMap.Lazy
  (IntMap(..), fromAscList, insert, keys, member, notMember, toList, (!))
import           Data.Sequence hiding (null)
import           Prelude hiding (filter)

import           Graph


-- Type alias
type SearchResult a = (IntMap Int, a)


-- Test graphs
foo1 :: GraphList
foo1 
  = initUGraph [1..8] 
  [(1, 2), (2, 3), (2, 4), (4, 5), (4, 6), (4, 8), (6, 7), (7, 8)]


class GraphSearch a where
  -- Traverses the graph using Depth-First Search from a given node
  -- and returns the corresponding spanning tree.
  -- Pre: The given node is in the graph.
  depthFirstTree :: Int -> a -> a
  depthFirstTree = (snd .) . depthFirst

  -- Traverses the graph using Depth-First Search from a given node
  -- and returns the list of passed nodes and their depths
  -- Pre: The given node is in the graph.
  depthFirstNodes :: Int -> a -> [(Int, Int)]
  depthFirstNodes = ((toList . fst) .) . depthFirst

  -- Traverses the graph using Depth-First Search from a given node
  -- Returns a tuple containing a map <node, depth> and
  -- the corresponding spanning tree.
  -- Pre: The given node is in the graph.
  depthFirst :: Int -> a -> SearchResult a

  -- Traverses the graph using Breadth-First Search from a given node
  -- and returns the corresponding spanning tree.
  -- Pre: The given node is in the graph.
  breadthFirstTree :: Int -> a -> a
  breadthFirstTree = (snd .) . breadthFirst

  -- Traverses the graph using Breadth-First Search from a given node
  -- and returns the list of passed nodes and their depths
  -- Pre: The given node is in the graph.
  breadthFirstNodes :: Int -> a -> [(Int, Int)]
  breadthFirstNodes = ((toList . fst) .) . breadthFirst

  -- Traverses the graph using Breadth-First Search from a given node
  -- Returns a tuple containing a map <node, depth> and
  -- the corresponding spanning tree.
  -- Pre: The given node is in the graph.
  breadthFirst :: Int -> a -> SearchResult a


instance GraphSearch GraphList where
  depthFirst n (LGraph _ list)
    = dfs (fromAscList []) n (initGraph (keys list) []) 0 []
    where
      dfs dMap n g d stack
        = dfs' (list ! n) stack
        where
          dMap'  = insert n d dMap
          g' = addUArcs [(head stack, n)] g
          dfs' Empty []
            | member n dMap = (dMap, g)
            | otherwise     = (dMap', g)
          dfs' Empty (st : sts)
            | member n dMap = dfs dMap' st g (d - 1) sts
            | otherwise     = dfs dMap' st g' (d - 1) sts
          dfs' (n' :<| ns) stack 
            | member n' dMap = dfs' ns stack
            | member n dMap  = dfs dMap n' g (d + 1) (n : stack)
            | null stack     = dfs dMap' n' g (d + 1) (n : stack)
            | otherwise      = dfs dMap' n' g' (d + 1) (n : stack)

  breadthFirst n (LGraph _ list)
    = bfs (fromAscList [(n, 0)]) (initGraph (keys list) []) (fromList [n])
    where
      bfs dMap g Empty
        = (dMap, g)
      bfs dMap g (q :<| qs)
        = bfs d' g' (qs >< adj)
        where
          depth         = dMap ! q + 1
          (d', g', adj) = execState (forM_ (list ! q) updateS) (dMap, g, empty)
          updateS e     = do
            (m, g, a) <- get
            if member e m
              then return ()
              else put (insert e depth m, addUArcs [(q, e)] g, insertAt 0 e a)
