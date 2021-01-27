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
import           Data.Set as S (fromDescList, insert, member, size)
import           Prelude hiding (filter)

import           Graph


-- Type alias
type SearchResult a = (IntMap Int, a)


-- Functions

-- Traverses the graph using Depth-First Search from a given node
-- and returns the corresponding spanning tree.
-- Pre: The given node is in the graph.
depthFirstTree :: Graph a => Int -> a -> a
depthFirstTree = (snd .) . depthFirst

-- Traverses the graph using Depth-First Search from a given node
-- and returns the list of passed nodes and their depths
-- Pre: The given node is in the graph.
depthFirstNodes :: Graph a => Int -> a -> [(Int, Int)]
depthFirstNodes = ((IM.toList . fst) .) . depthFirst

-- Traverses the graph using Depth-First Search from a given node
-- Returns a tuple containing a map <node, depth> and
-- the corresponding spanning tree.
-- Pre: The given node is in the graph.
depthFirst :: Graph a => Int -> a -> SearchResult a
depthFirst n graph
  = dfs (fromAscList []) n (initGraph (nodes graph) []) 0 []
  where
    dfs dMap n g depth stack
      = dfs' (fromList $ neighbours n graph) stack
      where
        dMap' = IM.insert n depth dMap
        g'    = addUArcs [(head stack, n)] g
        dfs' Empty []
          | IM.member n dMap = (dMap, g)
          | otherwise        = (dMap', g)
        dfs' Empty (st : sts)
          | IM.member n dMap = dfs dMap' st g (depth - 1) sts
          | otherwise     = dfs dMap' st g' (depth - 1) sts
        dfs' (n' :<| ns) stack
          | IM.member n' dMap = dfs' ns stack
          | IM.member n dMap  = dfs dMap n' g (depth + 1) (n : stack)
          | null stack        = dfs dMap' n' g (depth + 1) (n : stack)
          | otherwise         = dfs dMap' n' g' (depth + 1) (n : stack)

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

-- Topological sorting from a given root.
-- Pre: The root is in the graph
topologicalSort :: Graph a => a -> Maybe [Int]
topologicalSort graph
  = F.toList <$> (snd <$> (execState tSortS initial))
  where
    initial   = Just ((fromDescList [], fromDescList []), fromList [])
    tSortS    = forM_ (nodes graph) tSortS'
    tSortS' x = do
      raw <- get
      runWhenJust raw (\((nIn, nOut), ts) -> do
        if S.member x nIn
          then return ()
          else do
            let nIn' = S.insert x nIn
            put $ Just ((nIn', nOut), ts)
            forM_ (neighbours x graph) (\y -> do
              raw <- get
              runWhenJust raw (\((nIn, nOut), ts) -> 
                if S.member y nIn
                  then if not $ S.member y nOut
                    then put Nothing
                    else return ()
                  else tSortS' y
                )
              )
            raw <- get
            runWhenJust raw (\((nIn, nOut), ts) -> do
              put $ Just ((nIn, S.insert x nOut), insertAt 0 x ts)
              )
            )


foo :: GraphList
foo = initGraph [1..7] [(1, 2), (6, 2), (2, 7), (2, 5), (3, 4), (4, 5)]
