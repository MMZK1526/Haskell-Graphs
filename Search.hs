-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs

-- Implements Depth-First Search and Breadth-First Search on adjacency list.
-- All graphs are undirected here.

import           Data.Maybe (fromJust, isNothing)

-- May require installation
import           Data.IntMap.Lazy
  (IntMap(..), fromAscList, insert, keys, member, toList, (!)
  )
import           Data.Sequence hiding (null)
import           Prelude hiding (zip)

import           Graph


-- Test graphs
foo :: GraphList
foo 
  = initUGraph [1..8] 
  [(1, 2), (2, 3), (2, 4), (4, 5), (4, 6), (4, 8), (6, 7), (7, 8)]


class GraphSearch a where
  -- Traverses the graph using Depth-First-Search from a given node
  -- and returns the corresponding spanning tree.
  -- Pre: The given node is in the graph.
  depthFirstTree :: Int -> a -> a
  depthFirstTree = (snd .) . depthFirst

  -- Traverses the graph using Depth-First-Search from a given node
  -- and returns the list of passed nodes and their depths
  -- Pre: The given node is in the graph.
  depthFirstNodes :: Int -> a -> [(Int, Int)]
  depthFirstNodes = ((toList . fst) .) . depthFirst

  -- Traverses the graph using Depth-First-Search from a given node
  -- Returns a tuple containing a map <node, depth> and
  -- the corresponding spanning tree.
  -- Pre: The given node is in the graph.
  depthFirst :: Int -> a -> (IntMap Int, a)


instance GraphSearch GraphList where
  depthFirst n (LGraph size list)
    = dfs (fromAscList []) n (initGraph (keys list) []) 0 0 []
    where
      dfs dMap n graph d l stack
        = dfs' (list ! n) stack
        where
          dMap'  = if member n dMap
            then dMap
            else insert n d dMap
          graph' = if not (member n dMap) && not (null stack)
            then addUArcs [(head stack, n)] graph
            else graph
          dfs' Empty []
            = (dMap', graph)
          dfs' Empty (st : sts)
            = dfs dMap' st graph' (d - 1) (l + 1) sts
          dfs' (n' :<| ns) stack 
            | member n' dMap = dfs' ns stack
            | otherwise      = dfs dMap' n' graph' (d + 1) (l + 1) (n : stack)
