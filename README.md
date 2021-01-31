# Haskell-Graphs
Implements graph-related algorithms in Haskell.  
Based on the Imperial College First-Year Course: 40008 Graphs and Algorithms. 
(Just starting, I'll add algorithms as the course goes on)  
For a quick guide on the algorithms, check out Example.hs.  
For full documentation of the functions, see below.  

# Graph.hs
Implements graphs using Adjacency List;  
Provides basic access/modification methods.  
  
Dependencies: `Data.IntMap`, `Data.Sequence`, `Data.Set`.  
These should be available in default GHC. If not, install the hackage `containers`.  

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
    * Initialises a directed graph with the nodes from the first argument and the arcs specified by the list of pairs in the second argument.  
    * **Argument 1 `Int`:**  
      * The list of nodes.  
    * **Argument 2 `[(Int, Int)]`:**  
      * The list of arcs specified by the pairs of nodes.  
    * **Result:**
      * A directed graph that contains the given nodes and arcs.  
    * Pre: the nodes in the arc list are in node list.  
  
  * `initUGraph :: [Int] -> [(Int, Int)] -> a`  
    * Initialises an undirected graph with the nodes from the first argument and the arcs specified by the list of pairs in the second argument.  
    * **Argument 1 `Int`:**  
      * The list of nodes.  
    * **Argument 2 `[(Int, Int)]`:**  
      * The list of arcs specified by the pairs of nodes.  
    * **Result:**  
      * An undirected graph that contains the given nodes and arcs.  
    * Pre: the nodes in the arc list are in node list.  

  * `initWGraph :: [Int] -> [((Int, Int), Int)] -> a`  
    * Initialises a integer-weighted (will be refered as simply 'weighted' for convenience) directed graph with the nodes from the first argument and the arcs specified by the list of pairs in the second argument.  
    * **Argument 1 `Int`:**  
      * The list of nodes.  
    * **Argument 2 `[((Int, Int), Int)]`:**  
      * The list of arcs (the inner tuple as the first element of the outer tuple) specified by the pairs of nodes together with there corresponding weights (the integer as the second element of the outer tuple).  
    * **Result:**  
      * An weighted directed graph that contains the given nodes, arcs, and weights.  
    * Pre: the nodes in the arc list are in node list.  

  * `addArcs :: [(Int, Int)] -> a -> a`  
    * Adds the directed arcs specified by the list of pairs in the first argument;  
    * If the graph is weighted, then add the weight of the referred arcs by 1;  
    * **Argument 1 `[(Int, Int)]`:**  
      * The list of arcs specified by the pairs of nodes.  
    * **Argument 2 `a`:**  
      * The directed graph to be modified.  
    * **Result:**  
      * A new directed graph that contains the original graph as well as the new arcs.  
    * Pre: the node in the arc list are in the graph.   

  * `addUArcs :: [(Int, Int)] -> a -> a`  
    * Adds the undirected arcs specified by the list of pairs in the first argument;  
    * If the graph is weighted, then add the weight of the referred arcs by 1.  
    * **Argument 1 `[(Int, Int)]`:**  
      * The list of arcs specified by the pairs of nodes.  
    * **Argument 2 `a`:**  
      * The undirected graph to be modified.  
    * **Result:**  
      * A new undirected graph that contains the original graph as well as the new arcs.  
    * Pre: the node in the arc list are in the graph.  

  * `addWArcs :: [((Int, Int), Int)] -> a -> a`  
    * Adds the directed weighted arcs specified by the list of pairs in the first argument;  
    * Note that an arc can have a weight of zero.  
    * **Argument 1 `[((Int, Int), Int)]`:**  
      * The list of arcs (the inner tuple as the first element of the outer tuple) specified by the pairs of nodes together with there corresponding weights (the integer as the second element of the outer tuple).  
    * **Argument 2 `a`:**  
      * The weighted graph to be modified.  
    * **Result:**  
      * A new weighted graph that contains the original graph as well as the new arcs.  
    * Pre: the node in the arc list are in the graph.  

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
    * Pre: the node in the arc list are in the graph.  

  * `removeUArcs :: [(Int, Int)] -> a -> a`  
    * Removes the undirected arcs specified by the list of pairs in the first argument;  
    * If the graph is weighted, then remove the arcs regardless of the weight.  
    * **Argument 1 `[(Int, Int)]`:**  
      * The list of arcs specified by the pairs of nodes.  
    * **Argument 2 `a`:**  
      * The graph to be modified.  
    * **Result:**  
      * A new graph that removes the given arcs from the original graph.  
    * Pre: the node in the arc list are in the graph.  

 * `removeNodes :: [Int] -> a -> a`  
    * Removes the nodes indicated by the list to the graph, ignoring duplicates.  
    * **Argument 1 `[Int]`:**  
      * The list of nodes.  
    * **Argument 2 `a`:**  
      * The graph to be modified.  
    * **Result:**  
     * A new graph that removes the given nodes from the original graph.  

* `weight :: (Int, Int) -> a -> Maybe Int`
  * Returns the weight of the given arc, wrapped in Just;  
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
  * Pre: the node in the arc list are in the graph.  

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

* `inDegree :: Int -> a -> Int`  
  * **Argument 1`Int`:**  
    * The node.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Result:**  
    * The indegree of the node.  
  * Pre: the node is in the graph.  

* `outDegree :: Int -> a -> Int`  
  * **Argument 1`Int`:**  
    * The node.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Result:**  
    * The outdegree of the node.  
  * Pre: the node is in the graph.  

* `degree :: Int -> a -> Int`  
  * Returns the degree of a node in an unweighted graph;
  * Loops are counted twice.  
  * **Argument 1`Int`:**  
    * The node.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Result:**  
    * The outdegree of the node.  
  * Pre: the node is in the graph and the graph is unweighted.  

  * `neighbours :: Int -> a -> [Int]`
  * Returns the list of nodes that connects from the given node.  
  * Loops are counted twice.  
  * **Argument 1`Int`:**  
    * The node.  
  * **Argument 2 `a`:**  
    * The graph.  
  * **Result:**  
    * The list of adjacent nodes from the given node.  
  * Pre: the node is in the graph.  

# Search
Implements Depth-First Search & Breadth-First Search;  
Generates spanning trees and computes depth of each node given the root;  
Checks connectivity of graphs;  
Computes distance between two nodes;  
Conducts topological sort on directed acylic graphs (DAG).  

* `depthFirstNodes :: Graph a => Int -> a -> [(Int, Int)]`  
  * Traverses the graph using Depth-First Search from a given node and returns the list of passed nodes and their depths.  
  * The polymorphic type 'a' represents the graph.  
  * **Argument 1 `Int`:**  
    * The root node for the search.  
  * **Argument 2 `Graph a => a`:**  
    * The graph.  
  * **Result:**  
    * A list of tuples, where for each entry the first element is the node, and the second element is the depth of the corresponding node.  
  * Pre: The given node is in the graph.  

* `depthFirstTree :: Graph a => Int -> a -> a`  
  * Traverses the graph using Depth-First Search from a given node and returns the corresponding spanning tree;  
  * The polymorphic type 'a' represents the graph.  
  * **Argument 1 `Int`:**  
    * The root node for the search.  
  * **Argument 2 `Graph a => a`:**  
    * The graph.  
  * **Result:**  
    * The spanning tree generated by the search.  
  * Pre: The given node is in the graph.  

* `topologicalSort :: Graph a => a -> Maybe [Int]`  
  * Topological sorting of a directed acyclic graph (DAG);  
  * If the graph contains a cycle, will return Nothing;  
  * The polymorphic type 'a' represents the graph.  
  * **Argument 1:**  
    * The graph.  
  * **Result:**  
    * A topological ordering of the nodes of the graph, if exists, wrapped in a Just; if no such ordering exists, then Nothing.  

* `breadthFirstNodes :: Graph a => Int -> a -> [(Int, Int)]`  
  * Traverses the graph using Breadth-First Search from a given node and returns the list of passed nodes and their depths.  
  * The polymorphic type 'a' represents the graph.  
  * **Argument 1 `Int`:**  
    * The root node for the search.  
  * **Argument 2 `Graph a => a`:**  
    * The graph.  
  * **Result:**  
    * A list of tuples, where for each entry the first element is the node, and the second element is the depth of the corresponding node.  
  * Pre: The given node is in the graph.  

* `breadthFirstTree :: Graph a => Int -> a -> a`  
  * Traverses the graph using Breadth-First Search from a given node and returns the corresponding spanning tree;  
  * The polymorphic type 'a' represents the graph.  
  * **Argument 1 `Int`:**  
    * The root node for the search.  
  * **Argument 2 `Graph a => a`:**  
    * The graph.  
  * **Result:**  
    * The spanning tree generated by the search.  
  * Pre: The given node is in the graph.  

* `isConnected :: Graph a => a -> Bool`  
  * Test if the graph is connected;  
  * The polymorphic type 'a' represents the graph.  
  * **Argument 1:**
    * The graph.   
  * **Result:**  
    * True if the graph is connected (strongly connected if it is directed);
    * False if the graph is disconnected.  

* `distance :: Graph a => Int -> Int -> a -> Maybe Int`  
  * Returns the (unweighted) distance between two nodes;   
  * The polymorphic type 'a' represents the graph.  
  * **Argument 1 `Int`:**
    * The first node.  
  * **Argument 2 `Int`:**
    * The second node.  
  * **Argument 3 `Graph a => a`:**
    * The graph.   
  * **Result:**  
    * A non-negative Int representing the unweighted distance from the first node to the second node;  
    * If no such path exists, then Nothing.  
  * Pre: The given nodes are in the graph.  

* `depthFirstS :: Graph a => Int -> a -> Bool -> (Int -> State (Maybe b) ()) -> (Int -> State (Maybe b) ()) -> State ((Set Int, Set Int), (Maybe b)) ()`  
  * A State that simulates Depth-First Search.  
  * This function is convoluted and is not necessary unless you need to do custom actions during the Depth-First Search.  
  * The polymorphic type 'a' represents the graph.  
  * The polymorphic type 'b' represents the information produced by the search, e.g. a spanning tree or a list of nodes in some specific order.  
  * **Argument 1 `Int`:**  
    * The root node for the search.  
  * **Argument 2 `Graph a => a`:**  
    * The graph.  
  * **Argument 3 `Bool`:**  
    * If False, then the search will terminate when a cycle is encountered;  
    * If True, then the search will continue when a cycle is encountered;  
    * Note that here an undirected arc is considered as a cycle as well.  
  * **Argument 4 `Int -> State (Maybe b) ()`:**  
    * This function will be called whenever the search passes a new node for the FIRST time.  
    * *Argument 1:*  
      * The node that the search encounters for the first time.  
    * *Result:*  
      * A State that updates the information;  
      * If it is Nothing, then the search will terminate.  
  * **Argument 5 `(Int -> State (Maybe b) ())`:**  
    * This function will be called whenever the search passes a new node for the LAST time.  
    * *Argument 1:*  
      * The node that the search encounters for the last time.  
    * *Result:*  
      * A State that updates the information;  
      * If it is Nothing, then the search will terminate.  
  * **Result:**  
    * A State that stores the tuple of firstly/lastly visited nodes (should be the same if the search is not prematurely terminated), as well as information produced by the search;  
    * To make a valid search, the initial state should be in the form of `Just ((empty, empty), info)`, where empty is the empty Set.  
  * Pre: The given node is in the graph.  

* `breadthFirstS :: Graph a => Int -> a -> (Int -> State (Maybe b) ()) -> (Int -> State (Maybe b) ()) -> State ((Set Int, Seq Int), (Maybe b)) ()`  
  * A State that simulates Breadth-First Search.  
  * This function is convoluted and is not necessary unless you need to do custom actions during the Breadth-First Search.  
  * The polymorphic type 'a' represents the graph.  
  * The polymorphic type 'b' represents the information produced by the search, e.g. a spanning tree or a list of nodes in some specific order.  
  * **Argument 1 `Int`:**  
    * The root node for the search.  
  * **Argument 2 `Graph a => a`:**  
    * The graph.  
  * **Argument 3 `Int -> State (Maybe b) ()`:**  
    * This function will be called whenever the search passes a new node for the FIRST time (when the node is added into the frontier).  
    * *Argument 1:*  
      * The node that the search encounters for the first time.  
    * *Result:*  
      * A State that updates the information;  
      * If it is Nothing, then the search will terminate.  
  * **Argument 4 `Int -> State (Maybe b) ()`:**  
    * This function will be called whenever the search passes a new node for the LAST time (when the node is popped from the frontier).  
    * *Argument 1:*  
      * The node that the search encounters for the last time.  
    * *Result:*  
      * A State that updates the information;  
      * If it is Nothing, then the search will terminate.  
  * **Result:**  
    * A State that stores the tuple consists all visited nodes and the frontier (which should be empty if the search is not prematurely terminated), as well as information produced by the search;  
    * To make a valid search, the initial state should be in the form of `Just ((Data.Set.empty, Data.Sequence.empty), info)`.  
  * Pre: The given node is in the graph.  
