-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs

-- Examples of creating graphs and applying algorithms

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
    -- 1: [6,5,4]
    -- 2: [6,5,4]
    -- 3: [6,5,4]
    -- 4: [3,2,1]
    -- 5: [3,2,1]
    -- 6: [3,2,1]

-- We can also check that they represents the same graph.
  -- listToMat k33List == k33Matrix
    -- True
  -- matToList k33Matrix == k33List
    -- True

-- Some other graphs for testing purpose
k5List :: GraphList
k5List = initUGraph [1..5] [(i, j) | i <- [1..5], j <- [1..5], i < j]


-------------------------------------------------------------------------------- 
-- Search
-------------------------------------------------------------------------------- 

-- Choose node 1 as the root, conduct a Depth-First Search on the node and
-- find the spanning tree.
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
    -- 1: [6]
    -- 2: [4,5]
    -- 3: [5,6]
    -- 4: [2]
    -- 5: [2,3]
    -- 6: [3,1]
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
      -- 1: [4,5,6]
      -- 2: [4]
      -- 3: [4]
      -- 4: [2,3,1]
      -- 5: [1]
      -- 6: [1]
  -- k33BFSNodes
    -- [(1,0),(2,2),(3,2),(4,1),(5,1),(6,1)]
-- We can check that the spanning tree is indeed a tree,
-- and the depths are correct.

-- More examples
k5DFS :: SearchResult GraphList
k5DFS = depthFirst 3 k5List
-- Through Depth-First Search, the K5 graph is spanned by a linear tree

k5BFS :: SearchResult GraphList
k5BFS = breadthFirst 3 k5List
-- Through Breadth-First Search, the K5 graph is spanned by a 1-depth tree
