-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

-- Examples of creating graphs and applying algorithms.

import           Data.Maybe

-- May require installation
import           Data.IntMap.Lazy (IntMap(..))

import           Graph
import           HamiltonCircuit
import           Search
import           ShortestPath
import           SpanningTree


-------------------------------------------------------------------------------- 
-- Initialisation
-------------------------------------------------------------------------------- 

-- Make an (undirected) K_{3,3} graph that has nodes 1, 2, ..., 6
-- such that nodes 1, 2 and 3 does not connect with each other;
-- nodes 4, 5, and 6 does not connect with each other, but all other distinct
-- nodes are connected.

k33 :: GraphList
k33 
  = (initUGraph [1..6] 
  [(1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)])
  :: GraphList

procedures :: GraphList
procedures = initGraph [1..7] [(1, 2), (6, 2), (2, 7), (2, 5), (3, 4), (4, 5)]
-- The (directed) graph above looks like this:   
  --               1 ---> 2 ----------> 7
  --        6 ----------> 2 ---> 5
  -- 3 ----------> 4 ----------> 5

-- We can print them out in ghci:
  -- k33
    -- Nodes:
    -- [1,2,3,4,5,6]
    -- Adjacency List:
    -- 1: [4: 1, 5: 1, 6: 1]
    -- 2: [4: 1, 5: 1, 6: 1]
    -- 3: [4: 1, 5: 1, 6: 1]
    -- 4: [1: 1, 2: 1, 3: 1]
    -- 5: [1: 1, 2: 1, 3: 1]
    -- 6: [1: 1, 2: 1, 3: 1]
  -- procedures
    -- Nodes:
    -- [1,2,3,4,5,6,7]
    -- Adjacency List:
    -- 1: [2: 1]
    -- 2: [5: 1, 7: 1]
    -- 3: [4: 1]
    -- 4: [5: 1]
    -- 5: []
    -- 6: [2: 1]
    -- 7: []
-- Note that since the given graph is unweighted, the weight is default to 1.
  -- weight (1, 2) k33
    -- Nothing
  -- weight (1, 4) k33
    -- Just 1


-- Example of weighted graph:
wGraphEx :: GraphList
wGraphEx = initWGraph [1..3] [((1, 2), 0), ((1, 3), 1),((2, 3), 2)]

-- In ghci:
  -- wGraphEx
    -- Nodes:
    -- [1,2,3]
    -- Adjacency List:
    -- 1: [2: 0, 3: 1]
    -- 2: [3: 2]
    -- 3: []
-- Note that there is an arc from 1 to 2 with weight of zero, but it DOES
-- NOT mean there is no arc between 1 and 2!
  -- neighbours 1 wGraphEx
    -- [2,3]
-- 2 is a neighbour of 1 despite that the weight of the arc (1, 2) is zero.
  -- neighbours 2 wGraphEx
    -- [3]
-- 1 is NOT a neighbour of 2 because the arr (1, 2) is directed.


-- Some other graphs for testing purpose:

-- K5 graph
k5 :: GraphList
k5 = initUGraph [1..5] [(i, j) | i <- [1..5], j <- [1..5], i < j]

-- Disconnected graph with two K3 components:
k3t2 :: GraphList
k3t2 = initUGraph [1..6] [(1, 2), (1, 3), (2, 3), (4, 5), (4, 6), (5, 6)]


-------------------------------------------------------------------------------- 
-- Search
-------------------------------------------------------------------------------- 

-- Choose node 1 as the root, conduct a Depth-First Search on the node and
-- find the spanning tree.
k33DFSTree :: GraphList
k33DFSTree = depthFirstTree 1 k33

-- Again, choose node 1 as the root, conduct a Depth-First Search on the node
-- and find the depths of each node.
k33DFSNodes :: IntMap Int
k33DFSNodes = depthFirstNodes 1 k33

-- In ghci:
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
    -- fromList [(1,0),(2,2),(3,4),(4,1),(5,3),(6,5)]
-- We can check that the spanning tree is indeed a tree,
-- and the depths are correct.


-- Choose node 1 as the root, conduct a Breadth-First Search on the node and
-- find the spanning tree.
k33BFSTree :: GraphList
k33BFSTree = breadthFirstTree 1 k33

-- Again, choose node 1 as the root, conduct a Breadth-First Search on the node
-- and find the depths of each node.
k33BFSNodes :: IntMap Int
k33BFSNodes = breadthFirstNodes 1 k33

-- In ghci:
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
    -- fromList [(1,0),(2,2),(3,2),(4,1),(5,1),(6,1)]
-- We can check that the spanning tree is indeed a tree,
-- and the depths are correct.


-- Connectivity check:
  -- isConnected k33
    -- True
  -- isConnected k3t2
    -- False
-- isConnected may not work properly on directed graph:
  -- isConnected procedures
    -- True
-- To check connecivity for directed graphs, use isStronglyConnected:
  -- isStronglyConnected procedures
    -- False
  -- isStronglyConnected wGraphEx
    -- False
  -- isStronglyConnected k33
    -- True


-- Distance check
  -- distance 1 5 k33
    -- Just 1
  -- distance 1 2 k33
    -- Just 2
  -- distance 1 1 k33
    -- Just 0
  -- distance 1 4 k3t2
    -- Nothing


-- Demo for topological sorting:
-- Topological sorting on a directed acyclic graph is defined as an ordering of
-- its nodes, such that if node i is "greater" than node j, then there is no
-- path from i to j.
-- It is commonly used to describe a list of tasks, where some of the tasks must 
-- be completed before others.

topologicallySortedProcedures :: [Int]
topologicallySortedProcedures = fromJust $ topologicalSort procedures

-- In ghci:
  -- topologicallySortedProcedures
    -- [6,3,4,1,2,7,5]
-- If we sort an undirected graph, it will return Nothing because it cannot
-- establish the sense of direction between nodes.
  -- topologicalSort k4l
    -- Nothing


-- More examples on Search
k5DFSNodes :: IntMap Int
k5DFSNodes = depthFirstNodes 3 k5

k5DFSTree :: GraphList
k5DFSTree = depthFirstTree 3 k5
-- Through Depth-First Search, the K5 graph is spanned by a linear tree.

k5BFSNodes :: IntMap Int
k5BFSNodes = breadthFirstNodes 3 k5

k5BFSTree :: GraphList
k5BFSTree = breadthFirstTree 3 k5
-- Through Breadth-First Search, the K5 graph is spanned by a depth-1 tree.


-------------------------------------------------------------------------------- 
-- Minimum Spanning Tree
-------------------------------------------------------------------------------- 

-- Examples of weighted graphs:
wg1, wg2, wg3 :: GraphList
wg1 
  = initUWGraph [1..4] 
  [((1, 2), 3), ((1, 3), 4), ((2, 4), 4), ((3, 4), 1), ((1, 4), 5)]
wg2
  = initUWGraph [1..10]
  [((1, 2), 3), ((1, 3), 6), ((1, 4), 9), ((2, 3), 4), ((2, 4), 9), ((2, 5), 2),
   ((2, 7), 9), ((3, 5), 2), ((3, 6), 9), ((4, 7), 8), ((4, 8), 18),
   ((5, 6), 9), ((5, 7), 8), ((6, 7), 7), ((6, 9), 4), ((6, 10), 5),
   ((7, 8), 10), ((7, 10), 9), ((8, 9), 4), ((8, 10), 3), ((9, 10), 1)
  ]
wg3
  = initUWGraph [1..8]
  [((1, 2), 5), ((1, 8), 7), ((2, 3), 6), ((2, 8), 3), ((2, 6), 8), ((3, 6), 1),
   ((3, 4), 4), ((4, 5), 10), ((4, 6), 7), ((5, 6), 8), ((5, 7), 2), 
  ((6, 7), 11), ((7, 8), 9)
  ]


-- Fiding Minimum Spanning Trees using Prim's Algorithm:
  -- primMST wg1
    -- Just Nodes:
    -- [1,2,3,4]
    -- Adjacency List:
    -- 1: [2: 3, 3: 4]
    -- 2: [1: 3]
    -- 3: [1: 4, 4: 1]
    -- 4: [3: 1]
  -- primMST wg2
    -- Just Nodes:
    -- [1,2,3,4,5,6,7,8,9,10]
    -- Adjacency List:
    -- 1: [2: 3]
    -- 2: [1: 3, 5: 2]
    -- 3: [5: 2]
    -- 4: [7: 8]
    -- 5: [2: 2, 3: 2, 7: 8]
    -- 6: [7: 7, 9: 4]
    -- 7: [4: 8, 5: 8, 6: 7]
    -- 8: [10: 3]
    -- 9: [6: 4, 10: 1]
    -- 10: [8: 3, 9: 1]
-- We can also calculate the total weight of the spanning tree:
  -- primMSTWeights wg1
    -- Just 8
  -- primMSTWeights wg2
    -- Just 38
-- If the graph is not connected, returns Nothing:
  -- primMST k3t2
    -- Nothing

-- Fiding Minimum Spanning Trees using Kruskal's Algorithm:
  -- kruskalMST wg3
    -- 1: [2: 5]
    -- 2: [1: 5, 3: 6, 8: 3]
    -- 3: [2: 6, 4: 4, 6: 1]
    -- 4: [3: 4]
    -- 5: [6: 8, 7: 2]
    -- 6: [3: 1, 5: 8]
    -- 7: [5: 2]
    -- 8: [2: 3]
  -- kruskalMSTWeights wg3
    -- Just 29


-------------------------------------------------------------------------------- 
-- Shortest Distance
-------------------------------------------------------------------------------- 

wg4, wg5 :: GraphList
wg4
  = initUWGraph [1..7] 
  [((1, 2), 4), ((1, 5), 5), ((1, 7), 6), ((2, 3), 3), ((2, 5), 8), ((3, 4), 3),
   ((3, 5), 7), ((4, 5), 9), ((5, 6), 3), ((6, 7), 10)
  ]
wg5
  = initUWGraph [1..7] 
  [((1, 2), 3), ((1, 5), 5), ((1, 6), 7), ((1, 7), 5), ((2, 3), 7), ((3, 4), 2),
   ((3, 5), 6), ((4, 5), 9), ((4, 6), 8), ((6, 7), 7)
  ]

-- Shortest Distance between node 1 and the other nodes:
  -- shortestDistances 1 wg4
    -- fromList [(1,0),(2,4),(3,7),(4,10),(5,5),(6,8),(7,6)]
  -- shortestDistances 1 wg5
    -- fromList [(1,0),(2,3),(3,10),(4,12),(5,5),(6,7),(7,5)]

-- If the graph is not strongly connected, the function returns the distance
-- to all reachable nodes:
  -- shortestDistances 1 procedures
    -- fromList [(1,0),(2,1),(5,2),(7,2)]
  -- shortestDistances 7 procedures
    -- fromList []

-- The distance and path between two nodes:
  -- shortestDistance 1 6 wg4
    -- Just (8,[(1,5),(5,6)])
  -- shortestDistance 1 4 wg5
    -- Just (12,[(1,2),(2,3),(3,4)])
  -- shortestDistance 1 1 procedures
    -- Just (0,[])
  -- shortestDistance 2 1 procedures
    -- Nothing


-- If we provide a consitent heuristic function that estimates the distance
-- between all nodes and the end node, we can use efficient A* algorithm.

-- The heuristic distance betweeen any node and node 7 in wg4:
hWg4 :: Int -> Int
hWg4 1 = 7
hWg4 2 = 4
hWg4 3 = 2
hWg4 4 = 0
hWg4 5 = 8
hWg4 6 = 10
hWg4 7 = 13

-- In ghci:
  -- shortestDistanceWithHeuristic hWg4 1 6 wg4
    -- Just (8,[(1,5),(5,6)])
-- If the heuristic is not consistent, the function may return wrong result!
  -- shortestDistanceWithHeuristic (\s -> (7 - s) * 100) 1 6 wg4
    -- Just (16,[(1,7),(7,6)])


-- It must be pointed out that the algorithms does not work for graphs that
-- contain negative cycle.
negCyc :: GraphList
negCyc = initWGraph [1..3] [((1, 2), -1), ((2, 3), -1), ((3, 1), -1)]

-- In ghci:
  -- shortestDistance 1 3 negCyc
    -- Just (-2,[(1,2),(2,3)])
-- But in reality there is no shortest path.


-- Shortest paths between all pairs of nodes:
  -- shortestPathsFully wg5
    -- fromList [((1,1),Just (0,1)),((1,2),Just (3,2)),((1,3),Just (10,2)),
    -- ((1,4),Just (12,2)),((1,5),Just (5,5)),((1,6),Just (7,6)),
    -- ((1,7),Just (5,7)),((2,1),Just (3,1)),((2,2),Just (0,2)),
    -- ((2,3),Just (7,3)),((2,4),Just (9,3)),((2,5),Just (8,1)),
    -- ((2,6),Just (10,1)),((2,7),Just (8,1)),((3,1),Just (10,2)),
    -- ((3,2),Just (7,2)),((3,3),Just (0,3)),((3,4),Just (2,4)),
    -- ((3,5),Just (6,5)),((3,6),Just (10,4)),((3,7),Just (15,2)),
    -- ((4,1),Just (12,3)),((4,2),Just (9,3)),((4,3),Just (2,3)),
    -- ((4,4),Just (0,4)),((4,5),Just (8,3)),((4,6),Just (8,6)),
    -- ((4,7),Just (15,6)),((5,1),Just (5,1)),((5,2),Just (8,1)),
    -- ((5,3),Just (6,3)),((5,4),Just (8,3)),((5,5),Just (0,5)),
    -- ((5,6),Just (12,1)),((5,7),Just (10,1)),((6,1),Just (7,1)),
    -- ((6,2),Just (10,1)),((6,3),Just (10,4)),((6,4),Just (8,4)),
    -- ((6,5),Just (12,1)),((6,6),Just (0,6)),((6,7),Just (7,7)),
    -- ((7,1),Just (5,1)),((7,2),Just (8,1)),((7,3),Just (15,1)),
    -- ((7,4),Just (15,6)),((7,5),Just (10,1)),((7,6),Just (7,6)),
    -- ((7,7),Just (0,7))]
-- From the table, above, we can see from ((1,4),Just (12,2)) that the shortest 
-- distance from 1 to 4 is 12, and to travel along this path, 
-- one first travel from 1 to 2. Then we check the entry (2, 4) to see the next
-- node is 3, and checking the entry (3, 4) gives 4. Therefore the shortest path
-- from 1 to 4 is 1--2--3--4.


-- Maximum paths between all pairs of nodes:
  -- bandwithFully wg5
    -- fromList [((1,2),Just (6,6)),((1,3),Just (6,6)),((1,4),Just (7,6)),
    -- ((1,5),Just (7,6)),((1,6),Just (7,6)),((1,7),Just (7,6)),
    -- ((2,1),Just (6,3)),((2,3),Just (7,3)),((2,4),Just (6,3)),
    -- ((2,5),Just (6,3)),((2,6),Just (6,3)),((2,7),Just (6,3)),
    -- ((3,1),Just (6,5)),((3,2),Just (7,2)),((3,4),Just (6,5)),
    -- ((3,5),Just (6,5)),((3,6),Just (6,5)),((3,7),Just (6,5)),
    -- ((4,1),Just (7,6)),((4,2),Just (6,5)),((4,3),Just (6,5)),
    -- ((4,5),Just (9,5)),((4,6),Just (8,6)),((4,7),Just (7,6)),
    -- ((5,1),Just (7,4)),((5,2),Just (6,3)),((5,3),Just (6,3)),
    -- ((5,4),Just (9,4)),((5,6),Just (8,4)),((5,7),Just (7,4)),
    -- ((6,1),Just (7,1)),((6,2),Just (6,4)),((6,3),Just (6,4)),
    -- ((6,4),Just (8,4)),((6,5),Just (8,4)),((6,7),Just (7,7)),
    -- ((7,1),Just (7,6)),((7,2),Just (6,6)),((7,3),Just (6,6)),
    -- ((7,4),Just (7,6)),((7,5),Just (7,6)),((7,6),Just (7,6))]
-- From the table, above, we can see from ((1,2),Just (6,6)) that the maximum 
-- bandwith from 1 to 2 is 6, despite the direct arc only have weight 3.
-- To travel along this path, one first travel from 1 to 6. 
-- Then we check the entry (6, 2) to see the next node is 4, 
-- and checking the entry (4, 2) gives 5, then 3 and finally 2. Thus the path
-- from 1 to 2 with the maximum bandwith is 1--6--4--5--3--2.


-------------------------------------------------------------------------------- 
-- Hamilton Circuit
-------------------------------------------------------------------------------- 

ham1, ham2 :: GraphList
ham1
  = initUWGraph [1..7] 
  [((1, 2), 1), ((1, 5), 3), ((2, 3), 2), ((2, 4), 1), ((3, 5), 2), ((3, 6), 7),
   ((3, 7), 3), ((4, 5), 1), ((5, 6), 2), ((6, 7), 7)
  ]
ham2
  = initUWGraph [1..8] 
  [((1, 2), 4), ((1, 3), 10), ((1, 8), 5), ((2, 3), 11), ((2, 4), 15),
   ((3, 4), 13), ((3, 5), 3), ((3, 8), 11), ((4, 5), 6), ((4, 6), 5), 
   ((5, 6), 2), ((5, 7), 5), ((6, 7), 8), ((7, 8), 7)
  ]

-- The first graph has no Hamiltonian circuit, while the second has a number of.
-- The algorithm will always pick the one with the shortest total distance:
  -- hamiltonCircuit ham1
    -- Nothing
  -- hamiltonCircuit ham2
    -- Just (49,[1,8,7,6,4,5,3,2,1])
