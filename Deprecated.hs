{-# LANGUAGE TupleSections #-}

-- This file contains the Ajacency Matrix representation of graphs.
-- It is deprecated because it's much less efficient than the modified
-- Ajacency List representation, and it cannot tell apart an arc with weight 0
-- and an arc that does not exist.

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable (toList)
import           Data.List (intercalate)
import           Data.Maybe (fromJust, isNothing, maybe)
import           Data.Tuple (swap)
import           Prelude hiding (map, replicate)

-- May require installation
import           Data.IntMap.Lazy
  (IntMap(..), adjust, delete, fromAscList, insert, keys, map, mapWithKey
  , member, notMember, (!), (!?)
  )
import           Data.Sequence hiding (adjust, length, lookup, null, zip, (!?))

import           Graph

-- Test Graphs
zrm, k3m, k4m, l4m, prm, lpm :: GraphMatrix
zrm = emptyGraph
k3m = initGraph [0..2] [(0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1)]
k4m = addUArcs [(0, 3), (1, 3), (2, 3)] $ addNodes [3] k3m
l4m = initUGraph [1..4] [(1, 2), (2, 3), (3, 4)]
lpm = initUGraph [1..3] [(1, 1), (2, 2), (3, 3)]
prm = initUGraph [1..3] [(1, 2), (1, 2)]


-- Representing a graph as an adjacency matrix
data GraphMatrix = MGraph 
  { nodeNumM :: Int
  , nodesM :: Seq Int
  , nodeMat :: Seq (Seq Int)
  }

instance Show GraphMatrix where
  show (MGraph _ nodes arcs)
    = "Nodes:\n" 
    ++ show (toList nodes) 
    ++ "\nAdjacency Matrix:"
    ++ concatMap (('\n' : ) . show . toList) arcs

-- For two graphs in adjacency matrix to be equal, we need them to have the same
-- set of nodes (order does not matter), and the same arcs between nodes.
-- When the nodes are listed in a different order, the matrices will also be
-- different even when the graphs may be the same, therefore, we cannot depend 
-- on the derived Eq and must implement a subler sense of equality.
-- Note that (==) DOES NOT test for isomorphism! This is very expensive since
-- we don't have a O(n^k) algorithm for that yet...
instance Eq GraphMatrix where
  MGraph s n a == MGraph s' n' a' 
    = s == s' && sort n == sort n' && eq
    where
      -- The map between indices of nodes in the matrices. For example, if n is
      -- [1, 2, 3] and n' is [2, 3, 1], then dict is [(0, 2), (1, 0), (2, 1)].
      dict    = zip indices transI
      transI  = fmap (\i -> fromJust (elemIndexL (n `index` i) n')) indices
      lookUp  = fromJust . (flip lookup dict)
      indices = [0..(s - 1)]
      eq      = and $ fmap eqRow indices
      eqRow i
        = and $ fmap (\j -> r `index` j == r' `index` (lookUp j)) indices
        where
          r  = a `index` i
          r' = a' `index` (lookUp i)

instance Graph GraphMatrix where
  emptyGraph = MGraph 0 empty empty

  addWArcs arcs (MGraph sz nodes mat)
    = MGraph sz nodes $ addWArcs' mat arcs
    where
      addWArcs' m []
        = m
      -- Basically, find the row and column corresponding to node n and node n',
      -- then increment the value there by one.
      addWArcs' m (((n, n'), w) : as)
        = addWArcs' (update nI row' m) as
        where
          row' = update nI' (row `index` nI' + w) row
          row  = m `index` nI
          nI   = fromJust $ elemIndexL n nodes
          nI'  = fromJust $ elemIndexL n' nodes

  addNodes nodes g
    = MGraph sz' nodes' arcs'
    where
      -- Removes the nodes from the argument that are already in the graph.
      nodesF = Prelude.filter (isNothing . flip elemIndexL (nodesM g)) nodes
      sz'  = sz + length nodesF
      -- (><) is concatenation.
      nodes' = nodesM g >< fromList nodesF
      sz   = nodeNumM g
      -- Basically, add new empty rows to the bottom of the matrix, then add
      -- new empty columns to the right of the matrix.
      arcs'  = execState (
        forM_ [sz..(sz' - 1)] insertRow
        ) $ execState (forM_ [sz..(sz' - 1)] insertEle) <$> nodeMat g
      insertEle i
        = state $ \s -> ((), insertAt i 0 s)
      insertRow i
        = state $ \s -> ((), insertAt i (replicate sz' 0) s)

  removeArcs arcs (MGraph sz nodes mat)
    = MGraph sz nodes $ removeArcs' mat arcs
    where
      removeArcs' m []
        = m
      -- Basically, find the row and column corresponding to node n and node n',
      -- then decrement the value there by one (or remain zero).
      removeArcs' m ((n, n') : as)
        = removeArcs' (update nI row' m) as
        where
          row' = update nI' (max 0 (row `index` nI' - 1)) row
          row  = m `index` nI
          nI   = fromJust $ elemIndexL n nodes
          nI'  = fromJust $ elemIndexL n' nodes

  removeNodes [] g
    = g
  removeNodes (n : ns) g@(MGraph sz nodes arcs)
    | notIn     = removeNodes ns g
    | otherwise = removeNodes ns (MGraph (sz - 1) nodes' arcs')
    where
      notIn  = isNothing index
      index  = elemIndexL n nodes
      -- Removes the given node
      nodes' = del nodes
      -- Removes the row and column corresponding to the node.
      arcs'  = del <$> del arcs
      -- Removal helper
      del    = deleteAt (fromJust index)
      
  simplify (MGraph sz nodes arcs)
    = MGraph sz nodes $ mapWithIndex (mapWithIndex . simp) arcs
    where
      simp r c i
        | r == c    = 0
        | otherwise = min 1 i

  isDisconnectedAt n (MGraph _ nodes arcs)
    = null $ Data.Sequence.filter (/= 0) (arcs `index` i)
    where
      i = fromJust $ elemIndexL n nodes

  nodes = toList . nodesM

  numNodes = nodeNumM

  inDegree n (MGraph _ nodes arcs)
    = sum $ fmap (`index` (fromJust $ elemIndexL n nodes)) arcs

  outDegree n (MGraph _ nodes arcs)
    = sum $ arcs `index` (fromJust $ elemIndexL n nodes)

  degree = outDegree

  neighbours n (MGraph _ nodes arcs)
    = (nodes `index`) <$> [i | (i, e) <- zip [0..] row, e > 0]
    where
      row = toList $ arcs `index` (fromJust $ elemIndexL n nodes)


-- Transitions between matrix and list
listToMat :: GraphList -> GraphMatrix
listToMat (LGraph sz list)
  = initGraph nodes arcs
  where
    nodes = keys list
    arcs  = concatMap (\k -> (k, ) <$> getArcs (list ! k)) nodes
    getArcs m
      = concat $ (\k -> [1..(m ! k)] >> [k]) <$> (keys m)

matToList :: GraphMatrix -> GraphList
matToList (MGraph sz nodes arcs)
  = initGraph (toList nodes) arcs'
  where
    ind   = index nodes
    arcs' = concatMap (uncurry ((. toList) . decode)) (zip [0..] $ toList arcs)
    decode i row
      = concatMap (\(j, s) -> [1..s] >> [(ind i, ind j)]) (zip [0..] row)
