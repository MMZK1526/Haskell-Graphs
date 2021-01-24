module Graph where

-- Introduces the two representations of graphs
-- Provides initialisers and modifiers

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Maybe
import           Prelude hiding (replicate)

-- Requires installation
import           Data.Sequence hiding (length, filter)


-- A type class for graphs
class Graph a where
  -- Returns the empty graph with no nodes and arcs.
  emptyGraph :: a
  
  -- Initialise the graph by having the nodes from the first argument
  -- and the arcs specified by the list of pairs in the second argument,
  -- e.g. initMGraph [0, 1, 2] [(0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1)]
  -- builds the K3 graph, which has 3 nodes (indexed 0, 1 and 2) and they
  -- connect with each other.
  -- Note that the graph is ordered by default, which means that 
  -- initGraph [0, 1, 2] [(0, 1), (0, 2), (1, 2)] builds 
  -- an ordered graph that has arcs only from smaller indices to greater ones.
  -- Pre: the node indices in the arc list < the number of nodes of the graph.
  initGraph :: [Int] -> [(Int, Int)] -> a
  initGraph = flip addArcs . flip addNodes emptyGraph

  -- Add the arcs specified by the list of pairs in the second argument.
  -- Pre: the node indices in the arc list are in the graph.
  addArcs :: [(Int, Int)] -> a -> a

  -- Add the nodes indicated by the list to the graph, ignoring exising nodes.
  -- There are no arcs between new nodes
  addNodes :: [Int] -> a -> a

  -- Pre: the node indices in the arc list are in the graph.
  removeArcs :: [(Int, Int)] -> a -> a

  removeNodes :: [Int] -> a -> a

  -- Convert the graph to simple graph by removing all parallels and loops
  simplify :: a -> a

  -- Remove all arcs connecting to a node, but retain that node
  -- Pre: the node is in the graph.
  disconnect :: Int -> a -> a


-- Representing a graph as an adjacency matrix
data GraphMatrix = MGraph 
  { nodeNumM :: Int
  , nodesM :: Seq Int
  , nodeMat :: Seq (Seq Int)
  }
  -- deriving Show

instance Show GraphMatrix where
  show (MGraph _ nodes arcs)
    = "Nodes:\n" 
    ++ show (toList nodes) 
    ++ "\nAdjacency Matrix:"
    ++ concatMap (('\n' : ) . show . toList) arcs

instance Graph GraphMatrix where
  emptyGraph = MGraph 0 empty empty

  addArcs arcs (MGraph size nodes mat)
    = MGraph size nodes $ addArcs' mat arcs
    where
      addArcs' m []
        = m
      addArcs' m ((n, n') : as)
        = addArcs' (update nI row' m) as
        where
          row' = update nI' (row `index` nI' + inc) row
          row  = m `index` nI
          inc  = if nI == nI' then 2 else 1
          nI   = fromJust $ elemIndexL n nodes
          nI'  = fromJust $ elemIndexL n' nodes

  addNodes nodes g
    = MGraph size' nodes' arcs'
    where
      size'  = size + length nodesF
      nodes' = nodesM g >< fromList nodesF
      nodesF = filter (isNothing . flip elemIndexL (nodesM g)) nodes
      size   = nodeNumM g
      arcs'  = execState (
        forM_ [size..(size' - 1)] insertRow
        ) $ execState (forM_ [size..(size' - 1)] insertEle) <$> nodeMat g
      insertEle i
        = state $ \s -> ((), insertAt i 0 s)
      insertRow i
        = state $ \s -> ((), insertAt i (replicate size' 0) s)

  removeArcs arcs (MGraph size nodes mat)
    = MGraph size nodes $ removeArcs' mat arcs
    where
      removeArcs' m []
        = m
      removeArcs' m ((n, n') : as)
        = removeArcs' (update n row' m) as
        where
          row' = update n' (max 0 $ row `index` n' - inc) row
          row  = m `index` n
          inc  = if n == n' then 2 else 1

  removeNodes [] g
    = g
  removeNodes (n : ns) g@(MGraph size nodes arcs)
    | notIn     = removeNodes ns g
    | otherwise = removeNodes ns (MGraph (size - 1) nodes' arcs')
    where
      notIn  = isNothing index
      index  = elemIndexL n nodes
      nodes' = del nodes
      arcs'  = del <$> del arcs
      del    = deleteAt (fromJust index)
      
  simplify (MGraph size nodes arcs)
    = MGraph size nodes $ mapWithIndex (mapWithIndex . simp) arcs
    where
      simp r c i
        | r == c    = 0
        | otherwise = min 1 i

  disconnect n (MGraph size nodes arcs)
    = MGraph size nodes $ (overwrite 0) <$> (overwrite (replicate size 0)) arcs
    where
      overwrite = update (fromJust $ elemIndexL n nodes)

-- Test graphs
zrm, k3m, k4m, l4m :: GraphMatrix
zrm = emptyGraph
k3m = initGraph [0..2] [(0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1)]
k4m 
  = addArcs [(0, 3), (1, 3), (2, 3), (3, 0), (3, 1), (3, 2)] $ addNodes [3] k3m
l4m = initGraph [1..4] [(1, 2), (2, 3), (3, 4), (2, 1), (3, 2), (4, 3)]
