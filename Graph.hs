{-# LANGUAGE TupleSections #-}

module Graph where

-- Introduces the two representations of graphs
-- Provides initialisers and modifiers

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Maybe
import           Data.Tuple
import           Prelude hiding (map, replicate)

-- May require installation
import           Data.IntMap (toAscList)
import           Data.IntMap.Lazy
  (IntMap(..)
  , adjust
  , delete
  , fromAscList
  , insert
  , keys
  , map
  , mapWithKey
  , member
  , (!)
  , (!?)
  )
import           Data.Sequence hiding (adjust, length, lookup, zip, (!?))


-- Test graphs
zrm, k3m, k4m, l4m, prm, lpm :: GraphMatrix
zrm = emptyGraph
k3m = initGraph [0..2] [(0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1)]
k4m = addUArcs [(0, 3), (1, 3), (2, 3)] $ addNodes [3] k3m
l4m = initUGraph [1..4] [(1, 2), (2, 3), (3, 4)]
lpm = initUGraph [1..3] [(1, 1), (2, 2), (3, 3)]
prm = initUGraph [1..3] [(1, 2), (1, 2)]

zrl, k3l, k4l, l4l, prl, lpl :: GraphList
zrl = emptyGraph
k3l = initGraph [0..2] [(0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1)]
k4l = addUArcs [(0, 3), (1, 3), (2, 3)] $ addNodes [3] k3l
l4l = initUGraph [1..4] [(1, 2), (2, 3), (3, 4)]
lpl = initUGraph [1..3] [(1, 1), (2, 2), (3, 3)]
prl = initUGraph [1..3] [(1, 2), (1, 2)]


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
  -- Will cause error if the pre-condition is violated.
  initGraph :: [Int] -> [(Int, Int)] -> a
  initGraph = flip addArcs . flip addNodes emptyGraph

  -- Similar to above, but initialise an undirected graph
  initUGraph :: [Int] -> [(Int, Int)] -> a
  initUGraph nodes arcs
    = initGraph nodes (arcs ++ fmap swap arcs)

  -- Add the arcs specified by the list of pairs in the second argument.
  -- Pre: the node indices in the arc list are in the graph.
  -- Will cause error if the pre-condition is violated.
  addArcs :: [(Int, Int)] -> a -> a

  -- Similar to above, but the arcs are undirected (both n to n' and n' to n)
  -- Pre: the node indices in the arc list are in the graph.
  -- Will cause error if the pre-condition is violated.
  addUArcs :: [(Int, Int)] -> a -> a
  addUArcs arcs g
    = addArcs (arcs ++ fmap swap arcs) g

  -- Add the nodes indicated by the list to the graph, ignoring exising nodes.
  -- There are no arcs between the new nodes
  addNodes :: [Int] -> a -> a

  -- Pre: the node indices in the arc list are in the graph.
  -- Will cause error if the pre-condition is violated
  removeArcs :: [(Int, Int)] -> a -> a

  -- Similar to above, but undirected (removing both n to n' and n' to n)
  -- Pre: the node indices in the arc list are in the graph.
  -- Will cause error if the pre-condition is violated
  removeUArcs :: [(Int, Int)] -> a -> a
  removeUArcs arcs g
    = removeArcs (arcs ++ fmap swap arcs) g

  removeNodes :: [Int] -> a -> a

  -- Convert the graph to simple graph by removing all parallels and loops
  simplify :: a -> a

  -- Remove all arcs connecting to a node, but retain that node
  disconnect :: Int -> a -> a

  -- The in degree of a node in a directed graph
  -- Pre: the node is in the graph
  -- Will cause error if the pre-condition is violated.
  inDegree :: Int -> a -> Int

  -- The out degree of a node in a directed graph
  -- Pre: the node is in the graph
  -- Will cause error if the pre-condition is violated.
  outDegree :: Int -> a -> Int

  -- The degree of a node in an undirected graph
  -- Pre: the node is in the graph and the graph is indeed undirected.
  -- Will cause error/return unexpected result if the pre-condition is violated.
  degree :: Int -> a -> Int


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

  addArcs arcs (MGraph size nodes mat)
    = MGraph size nodes $ addArcs' mat arcs
    where
      addArcs' m []
        = m
      -- Basically, find the row and column corresponding to node n and node n',
      -- then increment the value there by one.
      addArcs' m ((n, n') : as)
        = addArcs' (update nI row' m) as
        where
          row' = update nI' (row `index` nI' + 1) row
          row  = m `index` nI
          nI   = fromJust $ elemIndexL n nodes
          nI'  = fromJust $ elemIndexL n' nodes

  addNodes nodes g
    = MGraph size' nodes' arcs'
    where
      -- Remove the nodes from the argument that are already in the graph.
      nodesF = Prelude.filter (isNothing . flip elemIndexL (nodesM g)) nodes
      size'  = size + length nodesF
      -- (><) is concatenation.
      nodes' = nodesM g >< fromList nodesF
      size   = nodeNumM g
      -- Basically, add new empty rows to the bottom of the matrix, then add
      -- new empty columns to the right of the matrix.
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
          row' = update n' (max 0 $ row `index` n' - 1) row
          row  = m `index` n

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

  disconnect n g@(MGraph size nodes arcs)
    | notIn     = g
    | otherwise = MGraph size nodes $ (rep 0) <$> (rep (replicate size 0)) arcs
    where
      notIn = isNothing index
      index = elemIndexL n nodes
      rep   = update (fromJust index)

  inDegree n (MGraph _ nodes arcs)
    = sum $ fmap (`index` (fromJust $ elemIndexL n nodes)) arcs

  outDegree n (MGraph _ nodes arcs)
    = sum $ arcs `index` (fromJust $ elemIndexL n nodes)

  degree 
    = degree


-- Representing a graph as an adjacency list
-- Note that an undirected loop is stored twice,
-- e.g. (2, 2) is the entry (2, [2, 2]) instead of (2, [2]).
data GraphList = LGraph 
  { nodeNumL :: Int
  , nodeList :: IntMap (Seq Int)
  }

instance Show GraphList where 
  show (LGraph _ list)
    = "Nodes:\n" 
    ++ show (toList $ keys list) 
    ++ "\nAdjacency List:"
    ++ concatMap showEntry (keys list)
    where
      showEntry key
        = '\n' : show key ++ ": " ++ show (toList (list ! key))

instance Eq GraphList where
  LGraph s l == LGraph s' l'
    = s == s' && eq
    where
      k  = keys l
      eq = and $ fmap (\s -> (sort <$> (l !? s)) == (sort <$> (l' !? s))) k

instance Graph GraphList where
  emptyGraph = LGraph 0 $ fromAscList []

  addArcs arcs (LGraph size list)
    = LGraph size $ addArcs' list arcs
    where
      addArcs' l []
        = l
      addArcs' l ((n, n') : as)
        = addArcs' (adjust (insertAt 0 n') n l) as

  addNodes [] g
    = g
  addNodes (n : ns) g@(LGraph size list)
    | member n list = addNodes ns g
    | otherwise     = addNodes ns $ LGraph (size + 1) (insert n empty list)

  removeArcs arcs (LGraph size list)
    = LGraph size $ removeArcs' list arcs
    where
      removeArcs' l []
        = l
      removeArcs' l ((n, n') : as)
        | isNothing index = removeArcs' l as
        | otherwise       = removeArcs' l' as
        where
          index = elemIndexL n' (l ! n)
          l'    = adjust (deleteAt (fromJust index)) n l

  removeNodes [] g
    = g
  removeNodes (n : ns) g@(LGraph size list)
    | member n list = removeNodes ns $ LGraph (size - 1) l'
    | otherwise     = removeNodes ns g
    where
      l' = map (execState removeAll) $ delete n list
      removeEle i = state $ \s -> ((), deleteAt i s)
      removeAll = do
        entry <- get
        let index = elemIndexL n entry
        if (isNothing index)
          then return ()
          else do 
            removeEle (fromJust index)
            removeAll

  simplify (LGraph size list)
    = LGraph size (mapWithKey nub' list)
    where
      nub' _ Empty
        = Empty
      nub' k (n :<| ns)
        | n == k    = nub' k ns
        | notIn     = n :<| nub' k ns
        | otherwise = nub' k ns
        where
          notIn = isNothing $ elemIndexL n ns

  disconnect n (LGraph size list)
    = LGraph size $ map (Data.Sequence.filter (/= n)) (delete n list)

  inDegree n (LGraph _ list)
    = sum $ fmap (length . elemIndicesL n) list

  outDegree n (LGraph _ list)
    = length (list ! n)

  degree 
    = outDegree
  

-- Transitions between matrix and list
listToMat :: GraphList -> GraphMatrix
listToMat (LGraph size list)
  = initGraph nodes arcs
  where
    nodes = keys list
    arcs  = concatMap (\k -> (k, ) <$> toList (list ! k)) nodes

matToList :: GraphMatrix -> GraphList
matToList (MGraph size nodes arcs)
  = initGraph (toList nodes) arcs'
  where
    ind   = index nodes
    arcs' = concatMap (uncurry ((. toList) . decode)) (zip [0..] $ toList arcs)
    decode i row
      -- [1..n] >> [k] means Data.List.replicate s k;
      = concatMap (\(j, s) -> [1..s] >> [(ind i, ind j)]) (zip [0..] row)
