-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs

module Search where

-- Implements Depth-First Search and Breadth-First Search on adjacency list.
-- All graphs are undirected here.

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Maybe (fromJust, isNothing)

-- May require installation
import           Data.IntMap.Lazy as I
  (IntMap(..), fromAscList, insert, keys, lookupMin, member, notMember, toList
  , (!), (!?)
  )
import           Data.Sequence hiding (length, null, (!?))
import           Data.Set as S (fromDescList, insert, member, size)
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

  -- Returns True if the graph is connected.
  isConnected :: a -> Bool

  -- Returns the distance between two nodes. If unreachable returns Nothing.
  -- Pre: The given nodes are in the graph.
  distance :: Int -> Int -> a -> Maybe Int
  distance = flip . (((!?) . fst) .) . breadthFirst

instance GraphSearch GraphList where
  depthFirst n (LGraph _ list)
    = dfs (fromAscList []) n (initGraph (keys list) []) 0 []
    where
      dfs dMap n g depth stack
        = dfs' (list ! n) stack
        where
          dMap'  = I.insert n depth dMap
          g' = addUArcs [(head stack, n)] g
          dfs' Empty []
            | I.member n dMap = (dMap, g)
            | otherwise     = (dMap', g)
          dfs' Empty (st : sts)
            | I.member n dMap = dfs dMap' st g (depth - 1) sts
            | otherwise     = dfs dMap' st g' (depth - 1) sts
          dfs' (n' :<| ns) stack
            | I.member n' dMap = dfs' ns stack
            | I.member n dMap  = dfs dMap n' g (depth + 1) (n : stack)
            | null stack     = dfs dMap' n' g (depth + 1) (n : stack)
            | otherwise      = dfs dMap' n' g' (depth + 1) (n : stack)

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
            if I.member e m
              then return ()
              else put (I.insert e depth m, addUArcs [(q, e)] g, insertAt 0 e a)

  isConnected (LGraph s list)
    = expand (fromDescList [n]) (fromList [n]) == s
    where
      n = fst $ fromJust (lookupMin list)
      expand ns Empty
        = size ns
      expand ns (q :<| qs)
        = expand ns' (qs >< adj)
        where
          (ns', adj) = execState (forM_ (list ! q) updateS) (ns, empty)
          updateS e  = do
            (m, a) <- get
            if S.member e m
              then return ()
              else put (S.insert e m, insertAt 0 e a)
