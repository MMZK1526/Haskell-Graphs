-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs

-- Examples of creating graphs and applying algorithms

import           Data.Maybe

import           Graph
import           Search


-------------------------------------------------------------------------------- 
-- Initialisation
-------------------------------------------------------------------------------- 

-- Make an (undirected) K_{3,3} graph that has nodes 1, 2, .., 6
-- such that nodes 1, 2 and 3 does not conenct with each other;
-- nodes 4, 5, and 6 does not connect with each other, but all other distinct
-- nodes are connected.
k33Matrix :: GraphMatrix
k33Matrix 
  = (initUGraph [1..6] 
  [(1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)])
  :: GraphMatrix

k33List :: GraphList
k33List 
  = (initUGraph [1..6] 
  [(1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)])
  :: GraphList

-- We can print them out in ghci
  -- k33Matrix
    -- Nodes:
    -- [1,2,3,4,5,6]
    -- Adjacency Matrix:
    -- [0,0,0,1,1,1]
    -- [0,0,0,1,1,1]
    -- [0,0,0,1,1,1]
    -- [1,1,1,0,0,0]
    -- [1,1,1,0,0,0]
    -- [1,1,1,0,0,0]
  -- k33List
    -- Nodes:
    -- [1,2,3,4,5,6]
    -- Adjacency List:
    -- 1: [4: 1, 5: 1, 6: 1]
    -- 2: [4: 1, 5: 1, 6: 1]
    -- 3: [4: 1, 5: 1, 6: 1]
    -- 4: [1: 1, 2: 1, 3: 1]
    -- 5: [1: 1, 2: 1, 3: 1]
    -- 6: [1: 1, 2: 1, 3: 1]

-- We can also check that they represents the same graph.
  -- listToMat k33List == k33Matrix
    -- True
  -- matToList k33Matrix == k33List
    -- True


-- Some other graphs for testing purpose

-- K5 graph
k5List :: GraphList
k5List = initUGraph [1..5] [(i, j) | i <- [1..5], j <- [1..5], i < j]

-- Disconnected graph with two K3 components
k3t2List :: GraphList
k3t2List = initUGraph [1..6] [(1, 2), (1, 3), (2, 3), (4, 5), (4, 6), (5, 6)]


-------------------------------------------------------------------------------- 
-- Search
-------------------------------------------------------------------------------- 

-- Choose node 1 as the root, conduct a Depth-First Search on the node and
-- find the spanning tree.
-- The functions work for both GraphList and GraphMatrix, but I'm demonstrating
-- the GraphList case here.
k33DFSTree :: GraphList
k33DFSTree = depthFirstTree 1 k33List

-- Again, choose node 1 as the root, conduct a Depth-First Search on the node
-- and find the depths of each node.
k33DFSNodes :: [(Int, Int)]
k33DFSNodes = depthFirstNodes 1 k33List

-- in ghci:
  -- k33DFSTree
    -- Nodes:
    -- [1,2,3,4,5,6]
    -- Adjacency List:
    -- 1: [4: 1]
    -- 2: [4: 1, 5: 1]
    -- 3: [5: 1, 6: 1]
    -- 4: [1: 1, 2: 1]
    -- 5: [2: 1, 3: 1]
    -- 6: [3: 1]
  -- k33DFSNodes
    -- [(1,0),(2,4),(3,2),(4,5),(5,3),(6,1)]
-- We can check that the spanning tree is indeed a tree,
-- and the depths are correct.


-- Choose node 1 as the root, conduct a Breadth-First Search on the node and
-- find the spanning tree.
k33BFSTree :: GraphList
k33BFSTree = breadthFirstTree 1 k33List

-- Again, choose node 1 as the root, conduct a Breadth-First Search on the node
-- and find the depths of each node.
k33BFSNodes :: [(Int, Int)]
k33BFSNodes = breadthFirstNodes 1 k33List

-- in ghci:
  -- k33BFSTree
    -- Nodes:
    -- [1,2,3,4,5,6]
    -- Adjacency List:
    -- 1: [4: 1, 5: 1, 6: 1]
    -- 2: [6: 1]
    -- 3: [6: 1]
    -- 4: [1: 1]
    -- 5: [1: 1]
    -- 6: [1: 1, 2: 1, 3: 1]
  -- k33BFSNodes
    -- [(1,0),(2,2),(3,2),(4,1),(5,1),(6,1)]
-- We can check that the spanning tree is indeed a tree,
-- and the depths are correct.


-- Connectivity check:
  -- isConnected k33List
    -- True
  -- isConnected k3t2List
    -- False


-- Distance check
  -- distance 1 5 k33Matrix
    -- Just 1
  -- distance 1 2 k33Matrix
    -- Just 2
  -- distance 1 1 k33Matrix
    -- Just 0
  -- distance 1 4 k3t2List
    -- Nothing


-- Demo for topological sorting:
-- Topological sorting on a directed acyclic graph is defined as an ordering of
-- its nodes, such that if node i is "greater" than node j, then there is no
-- path from i to j.
-- It is commonly used to describe a list of tasks, where some taske must be
-- completed before others.
procedures :: GraphList
procedures = initGraph [1..7] [(1, 2), (6, 2), (2, 7), (2, 5), (3, 4), (4, 5)]
-- The graph above looks like this:   
  --               1 ---> 2 ----------> 7
  --        6 ----------> 2 ---> 5
  -- 3 ----------> 4 ----------> 5

topologicallySortedProcedures :: [Int]
topologicallySortedProcedures = fromJust $ topologicalSort procedures

-- in ghci
  -- topologicallySortedProcedures
    -- [6,3,4,1,2,7,5]
-- If we sort an undirected graph, it will return nothing because it cannot
-- establish the sense of direction between nodes.
  -- topologicalSort k4m
    -- Nothing


-- More examples on Search
k5DFS :: SearchResult GraphList
k5DFS = depthFirst 3 k5List
-- Through Depth-First Search, the K5 graph is spanned by a linear tree.

k5BFS :: SearchResult GraphList
k5BFS = breadthFirst 3 k5List
-- Through Breadth-First Search, the K5 graph is spanned by a 1-depth tree.
