{-# LANGUAGE TupleSections #-}

-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs

module Graph where

-- Introduces the two representations of graphs (adjacency matrix/list).
-- Provides initialisers and modifiers.
-- The list version is more efficient and should be preferred over the matrix.

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


-- A type class for graphs; suitable for both directed and undirected.
-- TODO: Make it compatible with positively weighted simple graph
class Graph a where
  -- Returns the empty graph with no nodes and arcs.
  emptyGraph :: a
  
  -- Initialises the graph by having the nodes from the first argument
  -- and the arcs specified by the list of pairs in the second argument,
  -- e.g. initUGraph [0, 1, 2] [(0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1)]
  -- builds the K3 graph, which has 3 nodes (indexed 0, 1 and 2) and they
  -- connect with each other.
  -- Note that the graph is directed by default, which means that 
  -- initGraph [0, 1, 2] [(0, 1), (0, 2), (1, 2)] builds 
  -- an ordered graph that has arcs only from smaller nodes to greater ones.
  -- Pre: the nodes in the arc list are in node list.
  initGraph :: [Int] -> [(Int, Int)] -> a
  initGraph = flip addArcs . flip addNodes emptyGraph

  -- Similar to above, but initialises an undirected graph.
  initUGraph :: [Int] -> [(Int, Int)] -> a
  initUGraph nodes arcs
    = initGraph nodes (arcs ++ fmap swap arcs)

  -- Adds the arcs specified by the list of pairs in the second argument.
  -- Pre: the node indices in the arc list are in the graph.
  addArcs :: [(Int, Int)] -> a -> a

  -- Similar to above, but the arcs are undirected (both n to n' and n' to n).
  -- Pre: the node indices in the arc list are in the graph.
  addUArcs :: [(Int, Int)] -> a -> a
  addUArcs arcs g
    = addArcs (arcs ++ fmap swap arcs) g

  -- Adds the nodes indicated by the list to the graph, ignoring exising nodes.
  -- There are no arcs between the new nodes
  addNodes :: [Int] -> a -> a

  -- If the arc has parallels (or weight > 1, depending on the interpretation),
  -- removeArcs removes exactly 1 arc rather than breaking the connection.
  -- Pre: the node indices in the arc list are in the graph.
  removeArcs :: [(Int, Int)] -> a -> a

  -- Similar to above, but undirected (removes both n to n' and n' to n).
  -- Pre: the node indices in the arc list are in the graph.
  removeUArcs :: [(Int, Int)] -> a -> a
  removeUArcs arcs g
    = removeArcs (arcs ++ fmap swap arcs) g

  removeNodes :: [Int] -> a -> a

  -- Converts the graph to simple graph by removing all parallels and loops.
  simplify :: a -> a

  -- Removes all arcs connecting to a node, but retain that node.
  disconnect :: Int -> a -> a
  disconnect n g
    = addNodes [n] $ removeNodes [n] g

  -- Returns true if the node is disconnected in an undirected graph.
  -- Pre: the node is in the graph and the graph is indeed undirected.
  isDisconnectedAt :: Int -> a -> Bool
  isDisconnectedAt = ((== 0) .) . degree

  -- Returns the list of nodes.
  nodes :: a -> [Int]

  -- Returns the number of nodes
  numNodes :: a -> Int
  numNodes = length . nodes

  -- The in degree of a node in a directed graph.
  -- Pre: the node is in the graph.
  inDegree :: Int -> a -> Int

  -- The out degree of a node in a directed graph.
  -- Pre: the node is in the graph.
  outDegree :: Int -> a -> Int

  -- The degree of a node in an undirected graph.
  -- Pre: the node is in the graph and the graph is indeed undirected.
  degree :: Int -> a -> Int

  -- Returns the list of nodes connects from the given node
  -- Pre: the node is in the graph.
  neighbours :: Int -> a -> [Int]


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

  addArcs arcs (MGraph sz nodes mat)
    = MGraph sz nodes $ addArcs' mat arcs
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


-- Representing a graph as an adjacency list.
-- Note that an undirected loop is stored twice,
-- e.g. the arc (4, 4) is the represented as 4: [4: 2] instead of 4: [4: 1].
data GraphList = LGraph 
  { nodeNumL :: Int
  , nodeList :: IntMap (IntMap Int)
  }

instance Show GraphList where 
  show (LGraph _ list)
    = "Nodes:\n" ++ show (toList $ keys list) ++ "\nAdjacency List:"
    ++ concatMap showEntry (keys list)
    where
      showEntry k
        = '\n' : show k ++ ": [" ++ showMap (list ! k) ++ "]"
      showMap m
        = intercalate ", " $ (\k -> show k ++ ": " ++ show (m ! k)) <$> (keys m)

instance Eq GraphList where
  LGraph s l == LGraph s' l'
    = s == s' && (and $ fmap (\s -> (l !? s) == (l' !? s)) (keys l))

instance Graph GraphList where
  emptyGraph = LGraph 0 $ fromAscList []

  addArcs arcs (LGraph sz list)
    = LGraph sz $ addArcs' list arcs
    where
      addArcs' l []
        = l
      addArcs' l ((n, n') : as)
        = addArcs' (adjust updateEntry n l) as
        where
          updateEntry m
            | notMember n' l = m
            | member n' m    = adjust (+1) n' m
            | otherwise      = insert n' 1 m

  addNodes [] g
    = g
  addNodes (n : ns) g@(LGraph sz list)
    | member n list = addNodes ns g
    | otherwise     
      = addNodes ns $ LGraph (sz + 1) (insert n (fromAscList []) list)

  removeArcs arcs (LGraph sz list)
    = LGraph sz $ removeArcs' list arcs
    where
      removeArcs' l []
        = l
      removeArcs' l ((n, n') : as)
        | member n l = removeArcs' (adjust updateEntry n l) as
        | otherwise  = removeArcs' l as
        where
          updateEntry m
            | notMember n' m = m
            | m ! n' <= 1    = delete n' m
            | otherwise      = adjust (+ (-1)) n' m

  removeNodes [] g
    = g
  removeNodes (n : ns) g@(LGraph sz list)
    | member n list 
      = removeNodes ns $ LGraph (sz - 1) (map (delete n) (delete n list))
    | otherwise = removeNodes ns g

  simplify (LGraph sz list)
    = LGraph sz (mapWithKey ((map (const 1) .) . delete) list)

  isDisconnectedAt = (null .) . flip ((!) . nodeList)

  nodes = keys . nodeList

  numNodes = nodeNumL

  inDegree = (sum .) . (. nodeList) . fmap . (maybe 0 id .) . flip (!?)

  outDegree = (length .) . flip ((!) . nodeList)

  degree = outDegree

  neighbours = (keys .) . flip ((!) . nodeList)
  

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


-- Utilities
runWhenJust :: Maybe a -> (a -> State (Maybe a) ()) -> State (Maybe a) ()
runWhenJust m f
  | isNothing m = return ()
  | otherwise   = f (fromJust m)
