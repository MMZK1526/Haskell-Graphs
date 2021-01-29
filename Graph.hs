-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs

module Graph where

-- Represents graphs with modified ajacency lists.
-- Provides initialisers and modifiers.

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
zrl, k3l, k4l, l4l, prl, lpl :: GraphList
zrl = emptyGraph
k3l = initGraph [0..2] [(0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1)]
k4l = addUArcs [(0, 3), (1, 3), (2, 3)] $ addNodes [3] k3l
l4l = initUGraph [1..4] [(1, 2), (2, 3), (3, 4)]
lpl = initUGraph [1..3] [(1, 1), (2, 2), (3, 3)]
prl = initUGraph [1..3] [(1, 2), (1, 2)]


-- A type class for graphs; suitable for both directed and undirected.
-- Also compatible with integer-weighted simple graph. One can interpret 
-- an arc of weight two as EITHER a weighted arc OR a parallel.
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
  initUGraph = flip addUArcs . flip initGraph []

  -- Initialise a weighted graph, where the second argument has the form of
  -- ((node1, node2), weight)
  initWGraph :: [Int] -> [((Int, Int), Int)] -> a
  initWGraph = flip addWArcs . flip addNodes emptyGraph

  -- Adds the arcs specified by the list of pairs in the second argument.
  -- If the graph is considered as a weighted graph, this adds the weight of
  -- the arcs by 1.
  -- Pre: the node indices in the arc list are in the graph.
  addArcs :: [(Int, Int)] -> a -> a
  addArcs = addWArcs . (flip (,) 1 <$>)

  -- Similar to above, but the arcs are undirected (both n to n' and n' to n).
  -- Pre: the node indices in the arc list are in the graph.
  addUArcs :: [(Int, Int)] -> a -> a

  -- Adds weighted arcs specified by the list of pairs in the second argument.
  -- Example: ((3, 4), 10) means an arc from node 3 to node 4 with weight 10.
  -- Pre: the node indices in the arc list are in the graph.
  addWArcs :: [((Int, Int), Int)] -> a -> a

  -- Adds the nodes indicated by the list to the graph, ignoring exising nodes.
  -- There are no arcs between the new nodes
  addNodes :: [Int] -> a -> a

  -- Remove the given arcs with all of its parallels.
  -- Note that an arc with weight zero is not an arc that doesn't exist,
  -- Thus use setWeights to set weights to zero if that is the goal.
  -- Pre: the node indices in the arc list are in the graph.
  removeArcs :: [(Int, Int)] -> a -> a

  -- Similar to above, but undirected (removes both n to n' and n' to n).
  -- Pre: the node indices in the arc list are in the graph.
  removeUArcs :: [(Int, Int)] -> a -> a

  removeNodes :: [Int] -> a -> a

  -- Returns the weight of a given arc.
  -- If the arc does not exist, returns Nothing.
  -- Pre: the nodes in the arc are in the graph.
  weight :: (Int, Int) -> a -> Maybe Int

  -- Sets the weight of the given arc.
  -- Pre: the node indices in the arc list are in the graph.
  setWeights :: [((Int, Int), Int)] -> a -> a

  -- Converts the graph to simple graph by removing all parallels and loops.
  simplify :: a -> a

  -- Removes all arcs connecting to a node, but retain that node.
  disconnect :: Int -> a -> a
  disconnect n g
    = addNodes [n] $ removeNodes [n] g

  -- Returns the list of nodes.
  nodes :: a -> [Int]

  -- Returns the number of nodes
  numNodes :: a -> Int
  numNodes = length . nodes

  -- The in degree of a node in a directed graph (weighted).
  -- Pre: the node is in the graph.
  inDegree :: Int -> a -> Int

  -- The out degree of a node in a directed graph (weighted).
  -- Pre: the node is in the graph.
  outDegree :: Int -> a -> Int

  -- The degree of a node in an undirected graph.
  -- Pre: the node is in the graph and the graph is indeed undirected.
  degree :: Int -> a -> Int

  -- Returns the list of nodes connects from the given node
  -- Pre: the node is in the graph.
  neighbours :: Int -> a -> [Int]


-- Representing a graph as an Adjacency List.
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

  addUArcs arcs g
    = addArcs (arcs ++ fmap swap [(n, n') | (n, n') <- arcs,n /= n']) g

  addWArcs arcs (LGraph sz list)
    = LGraph sz $ addWArcs' list arcs
    where
      addWArcs' l []
        = l
      addWArcs' l (((n, n'), w) : as)
        = addWArcs' (adjust updateEntry n l) as
        where
          updateEntry m
            | notMember n' l = m
            | notMember n' m = insert n' w m
            | otherwise      = adjust (+w) n' m

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
        | member n l = removeArcs' (adjust (delete n') n l) as
        | otherwise  = removeArcs' l as

  removeUArcs arcs g
    = removeArcs (arcs ++ fmap swap [(n, n') | (n, n') <- arcs,n /= n']) g

  removeNodes [] g
    = g
  removeNodes (n : ns) g@(LGraph sz list)
    | member n list 
      = removeNodes ns $ LGraph (sz - 1) (map (delete n) (delete n list))
    | otherwise = removeNodes ns g

  setWeights arcs (LGraph sz list)
    = LGraph sz $ set' list arcs
    where
      set' l []
        = l
      set' l (((n, n'), w) : as)
        = set' (adjust updateEntry n l) as
        where
          updateEntry m
            | notMember n' l = m
            | notMember n' m = insert n' w m
            | otherwise      = adjust (const w) n' m 

  weight (n, n') (LGraph _ list)
    | notMember n list = Nothing
    | notMember n' row = Nothing
    | otherwise        = Just $ row ! n'
    where
      row = list ! n

  simplify (LGraph sz list)
    = LGraph sz (mapWithKey ((map (const 1) .) . delete) list)

  nodes = keys . nodeList

  numNodes = nodeNumL

  inDegree = (sum .) . (. nodeList) . fmap . (maybe 0 id .) . flip (!?)

  outDegree = ((maybe 0 id . (sum <$>)) .) . flip ((!?) . nodeList)

  degree n g
    = maybe 0 id (sum <$> mapWithKey (dCount n) <$> (nodeList g !? n))
    where
      dCount key n w
        | key == n  = 2 * w
        | otherwise = w

  neighbours = (keys .) . flip ((!) . nodeList)


-- Utilities

-- runWhenJust returns () when the input is Nothing, and applies the
-- following function when the input is a Just.
runWhenJust :: Maybe a -> (a -> State b ()) -> State b ()
runWhenJust m f
  | isNothing m = return ()
  | otherwise   = f (fromJust m)
