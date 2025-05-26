# Railway Network Optimization using Dijkstra’s Algorithm (Haskell)

This project implements a railway network simulation in Haskell, representing stations and railroads as a directed, weighted graph using an adjacency matrix. The project leverages advanced data structures and algorithms to efficiently compute shortest paths and enable dynamic network modification. It also significantly enhances problem-solving skills, as I had to adopt a new approach of thinking compared to imperative programming languages.

Key features:
-Adjacency Matrix Graph Representation: Models railway stations as nodes and tracks as directed, weighted edges, using a 2D list structure for efficient access and manipulation.
-Custom Priority Queue Data Structure: Utilizes a recursive priority queue and node type to support priority-based traversal, crucial for implementing Dijkstra’s algorithm in a purely functional paradigm.
-Random Network Generation: Automatically constructs random railway graphs with configurable size and seed, supporting reproducible experiments and testing.
-Edge Modification (pullLever): Allows real-time modification of edge weights, simulating operational changes or infrastructure updates within the network.
-Dijkstra’s Shortest Path Algorithm: Implements a robust version of Dijkstra’s algorithm for computing the minimal traversal cost between any two stations in the network, adapting classical graph theory to functional programming.
-Functional Design and Tests: Emphasizes immutability and recursion, adhering to Haskell best practices and providing test cases.

This project demonstrates the practical application of functional programming to solve complex graph-theoretic problems and provides a foundation for further exploration of transportation networks, optimization, and algorithm design in Haskell. 

