# Haskell-Graphs
Implements graph-related algorithms in `Haskell`.  
Based on the Imperial College First-Year Course: 40008 Graphs and Algorithms.  
For a quick guide on the algorithms, check out [Examples.hs](./Examples.hs).  
For full documentation of the functions, see below.  

Dependencies: `Data.IntMap`, `Data.Map`, `Data.Sequence`, `Data.Set`.  
Install the hackage `containers`.  
<br />

# Contents   
## 1. [Graph.hs](#graphhs)  

## 2. [Search.hs](#searchhs)  

## 3. [SpanningTree.hs](#spanningtreehs)  

## 4. [ShortestPath.hs](#shortestpathhs)

## 5. [HamiltonCircuit.hs](#hamiltoncircuiths)

## 6. [Utilities.hs](#utilitieshs)  
  * ### [Monadic Loop Control](#monadic-loop-control-1)
  * ### [Union Find](#union-find-1)  
  * ### [Miscellaneous](#miscellaneous-1)  
<br />

# [Graph.hs](./Graph.hs)
Implements graphs using Adjacency List;  
Provides basic access/modification methods.  
  
* `class Graph a`  
  * A type class for both unweighted graphs and integer-weighted simple graphs;  
  * All nodes are named with integer;  
  * This graph representation does not tell apart parallels and weighted arcs;  
  * For example, an arc from node 1 to node 2 with weight 2 can also be interpreted as two simple arcs from node 1 to node 2;  
  * The weight of any arc in a simple graph is 1;  
  * Implemented by newtype `GraphList`.  
  
  * `emptyGraph :: a`  
    * An empty graph with zero nodes and arcs.  

  * `initGraph :: [Int] -> [(Int, Int)] -> a`  
    * Initialises a **directed** graph with the nodes from the first argument and the arcs specified by the list of pairs in the second argument.  
    * **Argument 1 `Int`:**  
      * The list of nodes.  
    * **Argument 2 `[(Int, Int)]`:**  
      * The list of arcs specified by the pairs of nodes.  
    * **Result:**
      * A directed graph that contains the given nodes and arcs.  
    * **Pre:** The nodes in the arc list are in node list.  
  
  * `initUGraph :: [Int] -> [(Int, Int)] -> a`  
    * Initialises an **undirected** graph with the nodes from the first argument and the arcs specified by the list of pairs in the second argument.  
    * **Argument 1 `Int`:**  
      * The list of nodes.  
    * **Argument 2 `[(Int, Int)]`:**  
      * The list of arcs specified by the pairs of nodes.  
    * **Result:**  
      * An undirected graph that contains the given nodes and arcs.  
    * **Pre:** The nodes in the arc list are in node list.  

  * `initWGraph :: [Int] -> [((Int, Int), Int)] -> a`  
    * Initialises an integer-weighted (will be refered as simply 'weighted' for convenience) **directed** graph with the nodes from the first argument and the arcs specified by the list of pairs in the second argument.  
    * **Argument 1 `Int`:**  
      * The list of nodes.  
    * **Argument 2 `[((Int, Int), Int)]`:**  
      * The list of arcs (the inner tuple as the first element of the outer tuple) specified by the pairs of nodes together with there corresponding weights (the integer as the second element of the outer tuple).  
    * **Result:**  
      * An weighted directed graph that contains the given nodes, arcs, and weights.  
    * **Pre:** The nodes in the arc list are in node list.  

  * `initUWGraph :: [Int] -> [((Int, Int), Int)] -> a`  
    * Initialises an integer-weighted **undirected** graph with the nodes from the first argument and the arcs specified by the list of pairs in the second argument.  
    * **Argument 1 `Int`:**  
      * The list of nodes.  
    * **Argument 2 `[((Int, Int), Int)]`:**  
      * The list of arcs (the inner tuple as the first element of the outer tuple) specified by the pairs of nodes together with there corresponding weights (the integer as the second element of the outer tuple).  
    * **Result:**  
      * An weighted undirected graph that contains the given nodes, arcs, and weights.  
    * **Pre:** The nodes in the arc list are in node list.  

  * `addArcs :: [(Int, Int)] -> a -> a`  
    * Adds the **directed** arcs specified by the list of pairs in the first argument;  
    * If the graph is weighted, then add the weight of the referred arcs by 1;  
    * **Argument 1 `[(Int, Int)]`:**  
      * The list of arcs specified by the pairs of nodes.  
    * **Argument 2 `a`:**  
      * The directed graph to be modified.  
    * **Result:**  
      * A new directed graph that contains the original graph as well as the new arcs.  
    * **Pre:** The node in the arc list are in the graph.   

  * `addUArcs :: [(Int, Int)] -> a -> a`  
    * Adds the **undirected** arcs specified by the list of pairs in the first argument;  
    * If the graph is weighted, then add the weight of the referred arcs by 1.  
    * **Argument 1 `[(Int, Int)]`:**  
      * The list of arcs specified by the pairs of nodes.  
    * **Argument 2 `a`:**  
      * The undirected graph to be modified.  
    * **Result:**  
      * A new undirected graph that contains the original graph as well as the new arcs.  
    * **Pre:** The node in the arc list are in the graph.  

  * `addWArcs :: [((Int, Int), Int)] -> a -> a`  
    * Adds the **directed** weighted arcs specified by the list of pairs in the first argument;  
    * Note that an arc can have a weight of zero.  
    * **Argument 1 `[((Int, Int), Int)]`:**  
      * The list of arcs (the inner tuple as the first element of the outer tuple) specified by the pairs of nodes together with there corresponding weights (the integer as the second element of the outer tuple).  
    * **Argument 2 `a`:**  
      * The weighted graph to be modified.  
    * **Result:**  
      * A new weighted graph that contains the original graph as well as the new arcs.  
    * **Pre:** The node in the arc list are in the graph.  

  * `addUWArcs :: [((Int, Int), Int)] -> a -> a`  
    * Adds the **undirected** weighted arcs specified by the list of pairs in the first argument;  
    * Note that an arc can have a weight of zero.  
    * **Argument 1 `[((Int, Int), Int)]`:**  
      * The list of arcs (the inner tuple as the first element of the outer tuple) specified by the pairs of nodes together with there corresponding weights (the integer as the second element of the outer tuple).  
    * **Argument 2 `a`:**  
      * The weighted graph to be modified.  
    * **Result:**  
      * A new weighted undirected graph that contains the original graph as well as the new arcs.  
    * **Pre:** The node in the arc list are in the graph.  

  * `addNodes :: [Int] -> a -> a`  
    * Adds the nodes indicated by the list to the graph, ignoring duplicates.  
    * **Argument 1 `[Int]`:**  
      * The list of nodes.  
    * **Argument 2 `a`:**  
      * The graph to be modified.  
    * **Result:**  
     * A new graph that contains the original graph as well as the new nodes.  
  
  * `removeArcs :: [(Int, Int)] -> a -> a`  
    * Removes the directed arcs specified by the list of pairs in the first argument;  
    * If the graph is weighted, then remove the arcs regardless of the weight.  
    * **Argument 1 `[(Int, Int)]`:**  
      * The list of arcs specified by the pairs of nodes.  
    * **Argument 2 `a`:**  
      * The directed graph to be modified.  
    * **Result:**  
      * A new directed graph that removes the given arcs from the original graph.  
    * **Pre:** The node in the arc list are in the graph.  

  * `removeUArcs :: [(Int, Int)] -> a -> a`  
    * Removes the undirected arcs specified by the list of pairs in the first argument;  
    * If the graph is weighted, then remove the arcs regardless of the weight.  
    * **Argument 1 `[(Int, Int)]`:**  
      * The list of arcs specified by the pairs of nodes.  
    * **Argument 2 `a`:**  
      * The graph to be modified.  
    * **Result:**  
      * A new graph that removes the given arcs from the original graph.  
    * **Pre:** The node in the arc list are in the graph.  

  * `removeNodes :: [Int] -> a -> a`  
    * Removes the nodes indicated by the list to the graph, ignoring duplicates.  
    * **Argument 1 `[Int]`:**  
      * The list of nodes.  
    * **Argument 2 `a`:**  
      * The graph to be modified.  
    * **Result:**  
     * A new graph that removes the given nodes from the original graph.  

  * `weight :: (Int, Int) -> a -> Maybe Int`
    * Returns the weight of the given arc, wrapped in a `Just`;  
    * If the arc does not exist, then Nothing;  
      * **Argument 1 `[(Int, Int)]`:**  
        * The list of arcs specified by the pairs of nodes.  
      * **Argument 2 `a`:**  
        * The weighted graph.  
      * **Result:**  
        * The weight of the given arc.  

  * `setWeights :: [((Int, Int), Int)] -> a -> a`
    * Sets the weights of the given arcs.  
    * **Argument 1 `[((Int, Int), Int)]`:**  
      * The list of arcs (the inner tuple as the first element of the outer tuple) specified by the pairs of nodes together with there corresponding weights (the integer as the second element of the outer tuple).  
    * **Argument 2 `a`:**  
      * The weighted graph.  
    * **Result:**  
      * A new weighted graph that sets the weight of the given arcs to the given values from the original graph.  
    * **Pre:** The node in the arc list are in the graph.  

  * `simplify :: a -> a`
    * Convert a graph to a simple unweighted graph by removing all loops and parallels.  
    * **Argument 1:**  
      * The graph to be converted.  
    * **Result:**  
      * The corresponding simple graph.  

  * `disconnect :: Int -> a -> a`  
    * Remove all arcs that originate from or lead to a specified node.  
    * **Argument 1 `Int`:**  
      * The node to be disconnected.  
    * **Argument 2 `a`:**  
      * The graph to be modified.  
    * **Result:**  
      * The original graph without any arcs relating to the given node.  

  * `nodes :: a -> [Int]`  
    * **Argument 1:**  
      * The graph.  
    * **Result:**  
      * The list of nodes in the graph.  

  * `numNodes :: a -> [Int]`  
    * **Argument 1:**  
      * The graph.  
    * **Result:**  
      * The number of nodes in the graph.  

  *  `wArcs :: a -> [((Int, Int), Int)]`
    * **Argument 1:**  
      * The graph.  
    * **Result:**  
      * The list of **directed** arcs in the graph.  
    * **Pre:** The graph is directed.  

  *  `uArcs :: a -> [((Int, Int), Int)]`
    * **Argument 1:**  
      * The graph.  
    * **Result:**  
      * The list of **undirected** arcs in the graph.  
    * **Pre:** The graph is undirected.  

  * `inDegree :: Int -> a -> Int`  
    * **Argument 1 `Int`:**  
      * The node.  
    * **Argument 2 `a`:**  
      * The graph.  
    * **Result:**  
      * The indegree of the node.  
    * **Pre:** The node is in the graph.  

  * `outDegree :: Int -> a -> Int`  
    * **Argument 1 `Int`:**  
      * The node.  
    * **Argument 2 `a`:**  
      * The graph.  
    * **Result:**  
      * The outdegree of the node.  
    * **Pre:** The node is in the graph.  

  * `degree :: Int -> a -> Int`  
    * Returns the degree of a node in an unweighted graph;
    * Loops are counted twice.  
    * **Argument 1 `Int`:**  
      * The node.  
    * **Argument 2 `a`:**  
      * The graph.  
    * **Result:**  
      * The outdegree of the node.  
    * **Pre:** The node is in the graph and the graph is unweighted.  

  * `neighbours :: Int -> a -> [Int]`
  * Returns the list of nodes that connects from the given node.  
  * Loops are counted twice.  
  * **Argument 1 `Int`:**  
    * The node.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Result:**  
    * The list of adjacent nodes from the given node.  
  * **Pre:** The node is in the graph.  

* `data GraphList`  
  * Represents unweighted graphs/simple integer-weighted graphs in the form of Adjacency List.  
  * Can be printed out and can compare for equality (Equality in the sense of the identity isomorphism instead of homomorphism in general, as the latter is potentially an NP Prolbem).  
<br />  

## [Back to Title](#Haskell-Graphs)  
<br />

# [Search.hs](./Search.hs)
Implements Depth-First Search & Breadth-First Search;  
Generates spanning trees and computes depth of each node given the root;  
Checks connectivity of graphs;  
Computes distance between two nodes;  
Conducts topological sort on directed acylic graphs (DAG).  

* `depthFirstNodes :: Graph a => Int -> a -> [(Int, Int)]`  
  * Traverses the graph using Depth-First Search from a given node and returns the list of passed nodes and their depths.  
  * **Argument 1 `Int`:**  
    * The root node for the search.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Result:**  
    * A list of tuples, where for each entry the first element is the node, and the second element is the depth of the corresponding node.  
  * **Pre:** The given node is in the graph.  

* `depthFirstTree :: Graph a => Int -> a -> a`  
  * Traverses the graph using Depth-First Search from a given node and returns the corresponding spanning tree.  
  * **Argument 1 `Int`:**  
    * The root node for the search.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Result:**  
    * The spanning tree generated by the search.  
  * **Pre:** The given node is in the graph.  

* `topologicalSort :: Graph a => a -> Maybe [Int]`  
  * Topological sorting of a directed acyclic graph (DAG);  
  * If the graph contains a cycle, will return Nothing.  
  * **Argument 1:**  
    * The graph.  
  * **Result:**  
    * A topological ordering of the nodes of the graph, if exists, wrapped in a `Just`; if no such ordering exists, then Nothing.  

* `breadthFirstNodes :: Graph a => Int -> a -> [(Int, Int)]`  
  * Traverses the graph using Breadth-First Search from a given node and returns the list of passed nodes and their depths.  
  * **Argument 1 `Int`:**  
    * The root node for the search.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Result:**  
    * A list of tuples, where for each entry the first element is the node, and the second element is the depth of the corresponding node.  
  * **Pre:** The given node is in the graph.  

* `breadthFirstTree :: Graph a => Int -> a -> a`  
  * Traverses the graph using Breadth-First Search from a given node and returns the corresponding spanning tree.  
  * **Argument 1 `Int`:**  
    * The root node for the search.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Result:**  
    * The spanning tree generated by the search.  
  * **Pre:** The given node is in the graph.  

* `isConnected :: Graph a => a -> Bool`  
  * Test if the undirected graph is connected.  
  * **Argument 1:**
    * The undirected graph.   
  * **Result:**  
    * `True` if the graph is connected;
    * `False` if the graph is disconnected.  
  * **Pre:** The graph is undirected. 

* `isStronglyConnected :: Graph a => a -> Bool`  
  * Test if the (directed) graph is strongly connected.   
  * **Argument 1:**
    * The undirected graph.  
  * **Result:**  
    * `True` if the graph is strongly connected, otherwise `False`.  

* `distance :: Graph a => Int -> Int -> a -> Maybe Int`  
  * Returns the (unweighted) distance between two nodes.  
  * **Argument 1 `Int`:**
    * The first node.  
  * **Argument 2 `Int`:**
    * The second node.  
  * **Argument 3 `a`:**
    * The graph.   
  * **Result:**  
    * A non-negative Int representing the unweighted distance from the first node to the second node;  
    * If no such path exists, then Nothing.  
  * **Pre:** The given nodes are in the graph.  

* `depthFirstS :: (Graph a, Flaggable l1, Flaggable l2) => Int -> a -> (Int -> State b l1) -> (Int -> State b l2) -> State ((Set Int, Seq Int), b) (Terminate ())`  
  * A `State` that simulates Depth-First Search;  
  * This function is convoluted and is not necessary unless you need to do custom actions during the Depth-First Search;  
  * The polymorphic type `b` represents the information produced by the search, *e.g.* a spanning tree or a list of nodes in some specific order;  
  * The polymorphic types `l1` and `l2` represent instances of `Flaggable` (see the secion **Utilities.hs** for clarification).  
  * **Argument 1 `Int`:**  
    * The root node for the search.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Argument 3 `Bool`:**  
    * If `False`, then the search will terminate when a cycle is encountered;  
    * If `True`, then the search will continue when a cycle is encountered;  
    * Note that here an undirected arc is considered as a cycle as well.  
  * **Argument 4 `Int -> State b l1`:**  
    * This function will be called whenever the search passes a new node for the FIRST time.  
    * *Argument 1:*  
      * The node that the search encounters for the first time.  
    * *Result:*  
      * A `State` that updates the information and returns a `Flaggable`;  
      * If it terminates (*e.g.* ends in `breakLoop`), then the search will terminate;  
      * Otherwise (*e.g.* ends in `continueLoop`), the search will continue;  
      * Termination is used in the occasion where it is OK to end the search prematurely.  
  * **Argument 5 `Int -> State b l2`:**  
    * This function will be called whenever the search passes a new node for the LAST time.  
    * *Argument 1:*  
      * The node that the search encounters for the last time.  
    * *Result:*  
      * A `State` that updates the information and returns a `Flaggable`;  
      * If it terminates (*e.g.* ends in `breakLoop`), then the search will terminate;  
      * Otherwise (*e.g.* ends in `continueLoop`), the search will continue.  
  * **Result:**  
    * A `State` that stores the tuple of firstly/lastly visited nodes (should be the same if the search is not prematurely terminated), as well as information produced by the search;  
    * To make a valid search, the initial state should be in the form of `((empty, empty), info)`, where empty is the empty Set.  
  * **Pre:** The given node is in the graph.  

* `breadthFirstS :: (Graph a, Flaggable l1, Flaggable l2) => Int -> a -> (Int -> State b l1) -> (Int -> State b l2) -> State ((Set Int, Seq Int), b) (Terminate ())`  
  * A `State` that simulates Breadth-First Search;  
  * This function is convoluted and is not necessary unless you need to do custom actions during the Breadth-First Search;  
  * The polymorphic type `b` represents the information produced by the search, *e.g.* a spanning tree or a list of nodes in some specific order;  
  * The polymorphic types `l1` and `l2` represent instances of `Flaggable`.  
  * **Argument 1 `Int`:**  
    * The root node for the search.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Argument 3 `Int -> State b l2`:**  
    * This function will be called whenever the search passes a new node for the FIRST time (when the node is added to the frontier).  
    * *Argument 1:*  
      * The node that the search encounters for the first time.  
    * *Result:*  
      * A `State` that updates the information and returns a `Flaggable`;  
      * If it terminates, then the search will terminate;  
      * Otherwise, the search will continue;  
      * Termination is used in the occasion where it is OK to end the search prematurely.  
  * **Argument 4 `Int -> State b l2`:**  
    * This function will be called whenever the search passes a new node for the LAST time (when the node is popped from the frontier).  
    * *Argument 1:*  
      * The node that the search encounters for the last time.  
    * *Result:*  
      * A `State` that updates the information and returns a `Flaggable`;  
      * If it terminates, then the search will terminate;  
      * Otherwise, the search will continue.  
  * **Result:**  
    * A `State` that stores the tuple consists all visited nodes and the frontier (which should be empty if the search is not prematurely terminated), as well as information produced by the search;  
    * To make a valid search, the initial state should be in the form of `((Data.Set.empty, Data.Sequence.empty), info)`.  
  * **Pre:** The given node is in the graph.  
<br />  

## [Back to Title](#Haskell-Graphs)  
<br />

# [SpanningTree.hs](./SpanningTree.hs)  
Minimum Spanning Tree of weighted undirected graphs with Prim's Algorithm and Kruskal's Algorithm.  

* `primMST :: Graph a => a -> Maybe a`  
  * Generates the Minimum Spanning Tree via Prim's Algorithm.  
  * **Argument 1:**  
    * The graph.  
  * **Result:**  
    * The Minimum Spanning Tree wrapped in a `Just` if existed;  
    * If the graph is disconnected, then `Nothing`.  
  * **Pre:** The graph is undirected.  

* `primMSTWeights :: Graph a => a -> Maybe Int`  
  * Returns the total weight of the Minimum Spanning Tree via Prim's Algorithm.  
  * **Argument 1:**  
    * The graph.  
  * **Result:**  
    * The Minimum Spanning Tree's total weight wrapped in a `Just` if existed;  
    * If the graph is disconnected, then `Nothing`.  
  * **Pre:** The graph is undirected.  

* `kruskalMST :: Graph a => a -> Maybe a`  
  * Generates the Minimum Spanning Tree via Kruskal's Algorithm.  
  * **Argument 1:**  
    * The graph.  
  * **Result:**  
    * The Minimum Spanning Tree wrapped in a `Just` if existed;  
    * If the graph is disconnected, then `Nothing`.  
  * **Pre:** The graph is undirected.  

* `kruskalMSTWeights :: Graph a => a -> Maybe Int`  
  * Returns the total weight of the Minimum Spanning Tree via Kruskal's Algorithm.  
  * **Argument 1:**  
    * The graph.  
  * **Result:**  
    * The Minimum Spanning Tree's total weight wrapped in a `Just` if existed;  
    * If the graph is disconnected, then `Nothing`.  
  * **Pre:** The graph is undirected.  

* `primS :: (Graph a, Flaggable l) => a -> (Int -> Int -> Int -> State b l) -> State b (Terminate ())`  
  * A `State` that simulates Prim's Algorithm;  
  * This function is convoluted and is not necessary unless you need to do custom actions during the formation of the spanning tree;  
  * The polymorphic type `b` represents the information produced by the search, *e.g.* the spanning tree itself or the total weight of the tree.  
  * The polymorphic type `l` represents an instance of `Flaggable`.  
  * **Argument 1 `a`:**  
    * The graph.  
  * **Argument 2: `Int -> Int -> Int -> State b l`**
    * This function will be called whenever the algorithm finds a new arc for the Minimum Spanning Tree.  
    * *Argument 1 `Int`:*  
      * One of the end of the newly discovered arc.  
    * *Argument 2 `Int`:*  
      * The other end of the newly discovered arc.  
    * *Argument 3 `Int`:*  
      * The weight the newly discovered arc.  
    * *Result:*  
      * A `State` that updates the information and returns a `Flaggable`;  
      * If it terminates, then the algorithm will terminate;  
      * Otherwise, the algorithm will continue;  
  * **Result:**  
    * A state that stores the information and returns an instance of type `Terminate ()`;
    * If the output indicates `break`, then the algorithm ends prematurely, either because Argument 2 kills the process, or because the graph is not connected and thus a spanning tree is impossible.  
  * **Pre:** The graph is undirected.  

* `kruskalS :: (Graph a, Flaggable l) => a -> (Int -> Int -> Int -> State b l) -> State b (Terminate ())`  
  * A `State` that simulates Kruskal's Algorithm;  
  * This function is convoluted and is not necessary unless you need to do custom actions during the formation of the spanning tree;  
  * The polymorphic type `b` represents the information produced by the search.  
  * The polymorphic type `l` represents an instance of `Flaggable`.  
  * **Argument 1 `a`:**  
    * The graph.  
  * **Argument 2: `Int -> Int -> Int -> State b l`**
    * This function will be called whenever the algorithm finds a new arc for the Minimum Spanning Tree.  
    * *Argument 1 `Int`:*  
      * One of the end of the newly discovered arc.  
    * *Argument 2 `Int`:*  
      * The other end of the newly discovered arc.  
    * *Argument 3 `Int`:*  
      * The weight the newly discovered arc.  
    * *Result:*  
      * A `State` that updates the information and returns a `Flaggable`;  
      * If it terminates, then the algorithm will terminate;  
      * Otherwise, the algorithm will continue;  
  * **Result:**  
    * A state that stores the information and returns an instance of type `Terminate ()`;
    * If the output indicates `break`, then the algorithm ends prematurely.  
  * **Pre:** The graph is undirected.  
<br />  

## [Back to Title](#Haskell-Graphs)  
<br />


# [ShortestPath.hs](./ShortestPath.hs)

Finds shortest distance and shortest path between nodes of a weighted graph with Dijkstra's Algorithm and A* Algorithm.  
Finds shortest distances between all pairs of nodes using Floyd-Warshall Algorithm.  
Finds max bandwith between all distinct pairs of nodes.  
Produces closure of graphs.  

* `shortestDistances :: (Graph a) => Int -> a -> IntMap Int`  
  * Returns the shortest distance from a given node to all other reachable nodes in the graph, using Dijkstra's Algorithm.  
  * **Argument 1 `Int`:**  
    * The starting node.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Result:**  
    * An `IntMap` containing key-value pairs of nodes and shortest distance from the origin;  
    * The distance for the origin itself is 0.  
  * *Pre:* The node is in the graph;  
  * *Pre:* The graph does not contain negative cycles.  

* `shortestDistance :: (Graph a) => Int -> Int -> a -> Maybe (Int, [(Int, Int)])`  
  * Returns the shortest distance and the corresponding path between two nodes in the graph, using Dijkstra's Algorithm.  
  * **Argument 1 `Int`:**  
    * The starting node.  
  * **Argument 2 `Int`:**  
    * The finishing node.  
  * **Argument 3 `a`:**  
    * The graph.  
  * **Result:**  
    * If there exists a path connecting the origin with the destination, returns a tuple of shortest distance and list of arcs corresponding to the path, wrapped in a `Just`;  
    * If such path does not exist, returns `Nothing`.  
  * *Pre:* The nodes are in the graph;  
  * *Pre:* The graph does not contain negative cycles.    

* `shortestDistanceWithHeuristic :: (Graph a) => (Int -> Int) -> Int -> Int -> a -> Maybe (Int, [(Int, Int)])`  
  * Returns the shortest distance and the corresponding path between two nodes in the graph, using A* Algorithm.  
  * **Argument 1 `Int -> Int`:**  
    * The heuristic function that estimates the distance from any node to the finishing node.  
  * **Argument 2 `Int`:**  
    * The starting node.  
  * **Argument 3 `Int`:**  
    * The finishing node.  
  * **Argument 4 `a`:**  
    * The graph.  
  * **Result:**  
    * If there exists a path connecting the origin with the destination, returns a tuple of shortest distance and list of arcs corresponding to the path, wrapped in a `Just`;  
    * If such path does not exist, returns `Nothing`.  
  * *Pre:* The nodes are in the graph;  
  * *Pre:* The graph does not contain negative cycles;  
  * *Pre:* The heuristic is consistent, in other words, the heuristic value of the destination is zero, and if there is a path between x and y, h(x) <= h(y) + W(x, y).  

* `shortestDistanceSpanningTree :: (Graph a) => Int -> a -> a`  
  * Returns a spanning tree that represents the shortest paths from the root to any reachable nodes.  
  * **Argument 1 `Int`:**  
    * The starting node.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Result:**  
    * The spanning tree generated by Dijkstra's Algorithm.  
  * *Pre:* The nodes are in the graph;  
  * *Pre:* The graph does not contain negative cycles.  

* `shortestPathsFully :: (Graph a) => a -> Map (Int, Int) (Maybe (Int, Int))`  
  * Computing the shortest path between all pairs of nodes in a graph using Floyd-Warshall Algorithm;  
  * Returns the shortest distance and routing table for all pairs of nodes.  
  * **Argument 1:**  
    * The graph.  
  * **Result:**  
    * The result contains a map of shortest distance and routing table;  
    * The keys are tuples `(Int, Int)` recording the starting node and ending node;  
    * The values are `Maybe (Int, Int)` recording the shortest distance between start and end (if exists) and the next node after start on this path, all wrapped in a `Just`;  
    * If there is no path, then `Nothing`;  
  * *Pre:* The graph does not contain negative cycles.  

* `bandwithFully :: (Graph a) => a -> Map (Int, Int) (Maybe (Int, Int))`  
  * Computing the maximum bandwith between all pairs of nodes in a graph using Floyd-Warshall Algorithm;  
  * Returns the maximum bandwith and routing table for all pairs of distinct nodes.  
  * **Argument 1:**  
    * The graph.  
  * **Result:**  
    * The result contains a map of maximum bandwith and routing table;  
    * The keys are tuples `(Int, Int)` recording the starting node and ending node, given that start `/=` end;  
    * The values are `Maybe (Int, Int)` recording the maximum bandwith between start and end (if exists) and the next node after start on this path, all wrapped in a `Just`;  
    * If there is no path, then `Nothing`;  
  * *Pre:* The graph does not contain negative cycles.  

* `graphClosure :: (Graph a) => a -> a`  
  * Produces the closure of the given graph;  
  * The closure of a graph G is a simple graph that contains an arc from i to j to there is a path from i to j in G.  
  * **Argument 1:**  
    * The graph.
  * **Result:**  
    * The closure of the graph.  

* `dijkstraS :: (Graph a, Flaggable l) => Int -> a -> (Int -> Int -> Int -> State b l) -> State b (Terminate ())`  
  * A `State` that simulates Dijkstra's Algorithm;  
  * This function is convoluted and is not necessary unless you need to do custom actions during the formation of the shortest path spanning tree;  
  * The polymorphic type `b` represents the information produced by the algorithm;  
  * The polymorphic type `l` represents instance of `Flaggable`.  
  * **Argument 1 `Int`:**  
    * The starting node.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Argument 3 `Int -> Int -> Int -> State b l`:**  
    * This function will be called whenever the algorithm finds a new node.  
    * *Argument 1 `Int`:*  
      * The parent of the new node;  
      * If the new node is the root itself, then this is also the root.  
    * *Argument 2 `Int`:*  
      * The new node discovered by the algorithm.  
    * *Argument 3 `Int`:*  
      * The weight of the arc from Argument 1 to Argument 2;  
      * If the new node is the root, this value is `0`.  
    * *Result:*  
      * A `State` that updates the information and returns a `Flaggable`;  
      * If it terminates, then the algorithm will terminate;  
      * Otherwise, the algorithm will continue;  
  * **Result:**  
    * A `State` containing the information produced by the algorithm;  
    * If the output indicates `break`, then the algorithm ends prematurely.  
  * *Pre:* The given node is in the graph;  
  * *Pre:* The graph does not contain negative cycles.  

* `aStarS :: (Graph a, Flaggable l) => Int -> a -> (Int -> Int -> Int -> State b l) -> (Int -> Int) -> State b (Terminate ())`  
  * A `State` that simulates A* Algorithm;  
  * This function is convoluted and is not necessary unless you need to do custom actions during the formation of the shortest path spanning tree;  
  * The polymorphic type `b` represents the information produced by the algorithm;  
  * The polymorphic type `l` represents instance of `Flaggable`.  
  * **Argument 1 `Int`:**  
    * The starting node.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Argument 3 `Int -> Int -> Int -> State b l`:**  
    * This function will be called whenever the algorithm finds a new node.  
    * *Argument 1 `Int`:*  
      * The parent of the new node;  
      * If the new node is the root itself, then this is also the root.  
    * *Argument 2 `Int`:*  
      * The new node discovered by the algorithm.  
    * *Argument 3 `Int`:*  
      * The weight of the arc from Argument 1 to Argument 2;  
      * If the new node is the root, this value is `0`.  
    * *Result:*  
      * A `State` that updates the information and returns a `Flaggable`;  
      * If it terminates, then the algorithm will terminate;  
      * Otherwise, the algorithm will continue;  
  * **Argument 5 `Int -> Int`:**  
    * The heuristic function that estimates the distance from any node to the finishing node.  
  * **Result:**  
    * A `State` containing the information produced by the algorithm;  
    * If the output indicates `break`, then the algorithm ends prematurely.  
  * *Pre:* The given node is in the graph;  
  * *Pre:* The graph does not contain negative cycles;  
  * *Pre:* The heuristic function is consistent.  

* `floydWarshallS :: (Graph a, Flaggable l1, Flaggable l2) => (Int -> Int -> State b l1) -> (Int -> Int -> Int -> State b l2) -> a -> State b (Terminate ())`
  * A State that simulates the bare-bones of Floyd-Warshall Algorithm;  
  * The polymorphic type `b` represents the information produced by the algorithm;  
  * The polymorphic types `l1` and `l2` represent instances of `Flaggable`.  
  * **Argument 1 `Int -> Int -> State b l1`:**  
    * The initialising function called once for every pair of nodes at the beginning of the algorithm;  
    * For example, to calculate the shortest distance, a valid initialiasing function would could be something like `\n1 n2 -> Just w` where w is the weight of the arc (n1, n2), `\n1 n1 -> Just 0`, and ``\n1 n2 -> Nothing` if the arc does not exist.  
  * **Argument 1 `Int -> Int -> Int -> State b l2`:**  
    * The comparison function.  
    * *Argument 1 `Int`:*  
      * The starting node of the arc considered by the algorithm.  
    * *Argument 2 `Int`:*  
      * The ending node of the arc considered by the algorithm.  
    * *Argument 2 `Int`:*  
      * The intermediate node of the arc considered by the algorithm.  
    * *Result:*  
      * The updated information, usually computed with a comparison.  
    * For example, to calculate the shortest distance, a valid comparison function would could be something like `\n1 n2 n3 -> min (weight n1 n2) (weight n1 n3 + weight n3 n2)`.  
  * **Result:**  
    * A `State` containing the information produced by the algorithm;  
    * If the output indicates `break`, then the algorithm ends prematurely.  

<br />  

## [Back to Title](#Haskell-Graphs)  
<br />

# [HamiltonCircuit.hs](./HamiltonCircuit.hs)
Finds Hamiltonian Circuit of graph, if exists.  
  
* `hamiltonCircuit :: Graph a => a -> Maybe (Int, [Int])`
  * Finds the Hamiltonian Circuit with shortest total distance.  
  * The algorithm has super-exponential complexity, thus impractical with big graphs.  
  * **Argument 1 `a`:**  
    * The graph.  
  * **Result:**  
    * `Nothing` if a Hamiltonian Circuit does not exist;  
    * `Just (distance, path)` otherwise, where `path` is of type `[Int]` containing the nodes in the order of the shortest Hamiltonian Circuit;  
    * `distance` is the total distance of the path.  

* `bellmanHeldKarpS :: (Graph a) => (Int -> Int -> State b ()) -> (Integer -> Integer -> Int -> Int -> State b ()) -> (Integer -> Int -> Int -> State b ()) -> a -> State b ()`  
  * A State that simulates the bare-bones of Bellman-Held-Karp Algorithm;  
  * This function is convoluted and is not necessary unless you need to do custom actions during the algorithm;  
  * The polymorphic type `b` represents the information produced by the search;  
  * It must contain both the data stored for dynamic programming **AND** the final result;  
  * Note that this one does not have any `Terminate` feature, because I'm quite done with it.  
  * **Argument 1 `Int -> Int -> State b ()`:**  
    * A `State` that updates the information during initialisation, that is, the data corresponding to the empty subset in the algorithm.  
    * *Argument 1 `Int`:*  
      * The starting node, chosen by the mother function.  
    * *Argument 2 `Int`:*  
      * The current node during iteration through the rest of the nodes.  
  * **Argument 2 `Integer -> Integer -> Int -> Int -> State b ()`:**  
    * A `State` that updates the information during the dynamic programming part of the algorithm.  
    * *Argument 1 `Integer`:*  
      * The current subset.  
    * *Argument 2 `Integer`:*  
      * The current subset without one of its element.  
    * *Argument 3 `Int`:*  
      * The current node being considered (which is not in the subset).  
    * *Argument 4 `Int`:*  
      * The node being removed from the subset in Argument 1.  
  * **Argument 3 `Integer -> Int -> Int -> State b ()`:**  
    * A `State` that calculates the final result based on all the data gathered through dynamic programming.  
    * *Argument 1 `Integer`:*  
      * The current subset, which is the full set of nodes except the starting node, minus one of its element.  
    * *Argument 2 `Int`:*  
      * The starting node chosen by the mother function.  
    * *Argument 3 `Int`:*  
      * The node removed from the full set in Argument 1.  
  * **Argument 4 `a`:**  
    * The graph.  
  * **Result:**  
    * A state that stores the information, including both the desired result and all the data computed through the dynamic programming part.  

<br />  

## [Back to Title](#Haskell-Graphs)  
<br />

# [Utilities.hs](./Utilities.hs)

## Monadic Loop Control  
In the higher-order functions for Depth-First and Breadth-First Searches, the algorithms may halt the search based on a given condition (*e.g.* when a cycle is detected), in this case we would like a mechanism that simulates `break` in imperative languages such as `C`.  
A natural way of doing so is to wrap the information in an `Either`.  

The following example shows how this works:  
```
import           Control.Monad.Trans.State
import           Data.Either
-- Adds numbers from 1 to n
foo n 
  = execState (sigma (Right ()) 1) 0
  where
    sigma bFlag i = do
      if isLeft bFlag
        then return (Left ())
        else do
          acc <- get
          b' <- if i <= n
            then put (acc + i) >> return (Right ())
            else return (Left ())
          sigma b' (i + 1)
```

Apparently the above example implements a simple function in a laborious and unclear way, but by encapsulating the idea of having a state of breaking, we can write functions that contains elements resembling imperative `break` and `continue` in a fluent manner.  

With the tools in this file, we can rewrite the function above as:  

```
foo n
  = snd $ execState (loop_ (do 
      (i, sum) <- get
      breakWhen (i > n) $ put (i + 1, sum + i)
      )) (0, 0)
```

* `class (Flaggable a)`  
  * A type class that translates the instance to a `Bool` that indicates break or continue.  
  * Will refer to this type as 'flag' for the sake of simplicity.  
  
  * `isBreaking :: a -> Bool`  
    * Produces `True` for any data in the form of `Left a`;  
    * Produces `False` for any other data.  
    * **Argument 1:**  
      * The flag.  
    * **Result:** 
      * A `Bool` that indicates break or continue.  

* `type Terminate a`
  * A type alias that simulates breaking from an iterative action;  
  * It is implemented in the form of `Either a a`;  
  * The polymorphic type `a` is the information wrapped within;  
  * The idea is that if the value is in the form of `Left a` (which returns `True` when applied to `isBreaking`), the function that controls the iteration shall break short, otherwise it shall continue the iteration.  

* `terminate :: Terminate a -> Terminate a`  
  * **Argument 1:**  
    * The flag to be set to break.  
  * **Result:**  
    * A new flag containing the same information but set to break (`Left`).  

* `start :: Terminate a -> Terminate a`  
  * **Argument 1:**  
    * The flag to be set to continue.  
  * **ResultL**  
    * A new flag containing the same information but set to continue (`Right`).  

* `information :: Terminate a -> a`:  
  * Extracts the information within the flag regardless of the state of breaking.  
  * **Argument 1:**   
    * The flag.  
  * **Result:**  
    * The information wrapped within.  

* `breakFlag :: Terminate ()`:  
  * A flag that signifies break, without any information (*pure flag*).

* `continueFlag :: Terminate ()`:  
  * A flag that signifies continue, without any information (*pure flag*).  

* `breakLoop :: Monad m => m (Terminate ())`:  
  * The polymorphic type `m` indicates any monad;  
  * `breakFlag` wrapped in a monad.  

* `continueLoop :: Monad m => m (Terminate ())`:  
  * The polymorphic type `m` indicates any monad;  
  * `continueFlag` wrapped in a monad.  

* `flag :: Flaggable l => l -> Terminate ()`:
  * Removes the information from the given flag;  
  * The polymorphic type `l` represents any type (since any type is an instance of `Flaggable`).  
  * **Argument 1:**   
    * The flag.  
  * **Result:**  
    * A new flag that has the same state of breaking devoid of any information.  

* `breakUpon :: Monad m => Bool -> m (Terminate ())`
  * Breaks the loop (*i.e.* produces `breakLoop`) if the predicate is true, otherwise continue the loop;  
  * The polymorphic type `m` represents any monad.  
  * **Argument 1:**  
    * The predicate.  
  * **Result:**  
    * A pure flag according to the predicate.  

* `continueWhen :: (Monad m, Flaggable l) => Bool -> m l -> m (Terminate ())`
  * The polymorphic type `m` represents any monad;  
  * The polymorphic type `l` represents an instance of `Flaggable`, i*i.e.* any type.  
  * **Argument 1 `Bool`:**  
    * The predicate.  
  * **Argument 2 `m l`:**  
    * A monadic action that returns a `Flaggable`.  
  * **Result:**  
    * If the predicate is `True`, produces `continueLoop`;  
    * If the predicate is `False`, runs the monadic action.  

* `breakWhen :: (Monad m, Flaggable l) => Bool -> m l -> m (Terminate ())`
  * The polymorphic type `m` represents any monad;  
  * The polymorphic type `l` represents an instance of `Flaggable`, *i.e.* any type.  
  * **Argument 1 `Bool`:**  
    * The predicate.  
  * **Argument 2 `m l`:**  
    * A monadic action that returns a `Flaggable`.  
  * **Result:**  
    * If the predicate is `True`, produces `breakLoop`;  
    * If the predicate is `False`, runs the monadic action.  

* `runUnlessBreak :: (Monad m, Flaggable l1, Flaggable l2) => l1 -> m l2 -> m (Terminate ())`
  * The polymorphic type `m` represents any monad;  
  * The polymorphic types `l1` and `l2` represent instances of `Flaggable`.  
  * **Argument 1 `l1`:**  
    * A `Flaggable` that indicates if the next argument is to be run.  
  * **Argument 2 `m l`:**  
    * A monadic action.  
  * **Result:**  
    * If the `Flaggable` indicates break, produces `breakLoop`;  
    * Otherwise the function runs the monadic action;  
    * In otherwords, this function behaves like `breakWhen` except with the "predicate" in the form of a `Flaggable`.  

* `forMBreak_ :: (Foldable f, Monad m, Flaggable l) => f a -> (a -> m l) -> m (Terminate ())`  
  * Applies all elements of the `Foldable` to the monadic action in sequential order, disgarding the intermediate results;  
  * Whenever the monadic action returns a break flag, the function immediately terminates itself and produces `breakLoop`;  
  * If all elements of the `Foldable` are succesfully applied, the function produces `continueLoop`;  
  * The polymorphic type `m` represents any monad;  
  * The polymorphic type `l` represents an instance of `Flaggable`.  
  * **Argument 1 `f a`:**  
    * A `Foldable` that contains the elements to be applied to the following monadic action.  
  * **Argument 2 `m l`:**  
    * A monadic action.  
  * **Result:**  
    * A new monadic action formed by iterating Argument 2, and it produces a flag based on if Argument 1 is fully iterated.  

* `loop_ :: (Monad m, Flaggable l) => m l -> m (Terminate ())`  
  * Iterates an argumentless monadic action indefinitely until the action returns a break flag;  
  * The polymorphic type `m` represents any monad;  
  * The polymorphic type `l` represents an instance of `Flaggable`.  
  * **Argument 1:** 
    * A monadic action.  
  * **Result:**  
    * A new monadic action formed by iterating the argument until break;  
    * If the function terminates, then it always produces `breakLoop` since it only terminates from a break flag.  

* `loop :: Monad m => a -> (a -> m (Terminate a)) -> m (Terminate a)`  
  * Iterates a monadic action that takes one aggument indefinitely until the action returns a break flag;  
  * The polymorphic type `m` represents any monad;  
  * The polymorphic type `l` represents an instance of `Flaggable`.  
  * **Argument 1 `a`:**  
    * The initial value.  
  * **Argument 2 `a -> m (Terminate a)`:**  
    * A monadic action.  
  * **Result:**  
    * A new monadic action formed by iterating the second argument until break;  
    * If the function terminates, then it always produces a result wrapped in `Terminate` that represents break (*i.e.* `Left _`).  

## Union Find  
This file also contains a data type, `UnionFind`, that is vital in Kruskal's Algorithm.  

* `data UnionFind = UF (IntMap Int) (IntMap (Seq Int))`  
  * Represents a group of (disjoint) equivalence classes on a subset of **N**;  
  * Each equivalent class has a representative element;  
  * The equivalence classes are dynamic, which means we can merge two classes into one.  

* `emptyUF :: UnionFind`  
  * Returns an empty `UnionFind` with zero equivalence classes.  

* `initUF :: [Int] -> UnionFind`  
  * Initialises a trivial `UnionFind`.  
  * **Argument 1:**  
    * The list of integers to be stored.  
  * **Result:**  
    * A group of equivalence classes using the raltionship `=`, *i.e.* every integer is equivalent only to itself.  
  * **Pre:** The list must contain no duplicates.  

* `getRep :: Int -> UnionFind -> Int`
  * **Argument 1 `Int`:**  
    * An integer in one of the equivalence classes.  
  * **Argument 2 `UnionFind`:**  
    * The `UnionFind` data.  
  * **Result:**  
    * The representative of the given integer in the given `UnionFind`.  
  * **Pre:** The integer must be a member of one of the equivalence classes.  

* `equiv :: Int -> Int -> UnionFind -> Bool`  
  * **Argument 1 `Int`:**  
    * An integer in one of the equivalence classes.  
  * **Argument 2 `Int`:**  
    * Another integer in one of the equivalence classes.  
  * **Argument 2 `UnionFind`:**  
    * The `UnionFind` data.  
  * **Result:**  
    * `True` if the two integers belong to the same equivalence class in the `UnionFind`, otherwise `False`.  
  * **Pre:** The integers must belong to some of the equivalence classes.  

* `unionFind :: Int -> Int -> UnionFind -> UnionFind`  
  * Takes the union of two equivalence classes in a `UnionFind`, *i.e.* merges them into one and pick one of the representatives to be the new representative.  
  * **Argument 1 `Int`:**  
    * An integer in one of the equivalence classes.  
  * **Argument 2 `Int`:**  
    * Another integer in one of the equivalence classes.  
  * **Argument 3 `UnionFind`:**  
    * The `UnionFind` data.  
  * **Result:**  
    * An updated `UnionFind` where the two original equivalence classes corresponding to the given integers are merged. One of the original representatives is picked to be the representative of the bigger equivalence class.
  * **Pre:** The integers must belong to some of the equivalence classes;  
  * **Pre:** The integers must not belong to the same equivalence class.  

## Miscellaneous  
* `runWhenJust :: Monad m => Maybe a -> m b -> m ()`  
  * Apply the monadic action if the first argument is not `Nothing`;  
  * The polymorphic type `m` represents any monad.  
  * **Argument 1 (`Maybe a`):**  
    * A piece of information or `Nothing`.  
  * **Argument 2 (`m b`):**  
    * A monadic action.  
  * **Result:**  
    * Simply returns `()` if the Argument 1 is `Nothing`;  
    * Otherwise run the monadic action (that still returns `()` eventually).  

* `minMaybeOn :: (Ord b) => (a -> b) -> Maybe a -> Maybe a -> Maybe a`  
  * **Argument 1 `a -> b`:**  
    * A function that renders the following arguments comparable.  
  * **Argument 2 `Maybe a`:**  
    * An operand.  
  * **Argument 3 `Maybe a`:**  
    * Another operand.  
  * **Result:**  
    * If one of the operands is `Nothing`, returns the other one;  
    * Otherwise returns the one that has the smaller value when applied to Argument 1;  
    * If the results are the same, Argument 2 is preferred.  

* `maxMaybeOn :: (Ord b) => (a -> b) -> Maybe a -> Maybe a -> Maybe a`  
  * **Argument 1 `a -> b`:**  
    * A function that renders the following arguments comparable.  
  * **Argument 2 `Maybe a`:**  
    * An operand.  
  * **Argument 3 `Maybe a`:**  
    * Another operand.  
  * **Result:**  
    * If one of the operands is `Nothing`, returns the other one;  
    * Otherwise returns the one that has the greater value when applied to Argument 1;  
    * If the results are the same, Argument 2 is preferred.  

<br />  

## [Back to Title](#Haskell-Graphs)  
<br />
