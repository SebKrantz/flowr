# Transport Modeling

*flowr* provides efficient tools for transportation modeling,
specifically route enumeration and traffic assignment tasks. The package
implements the path-sized logit model for traffic assignment and
provides utilities for network processing.

**Network Processing**

[`linestrings_to_graph()`](https://sebkrantz.github.io/flowr/reference/linestrings_to_graph.md)
— Convert LINESTRING geometries to graph  
[`create_undirected_graph()`](https://sebkrantz.github.io/flowr/reference/create_undirected_graph.md)
— Convert directed graph to undirected  
[`consolidate_graph()`](https://sebkrantz.github.io/flowr/reference/consolidate_graph.md)
— Consolidate graph by removing intermediate nodes  
[`simplify_network()`](https://sebkrantz.github.io/flowr/reference/simplify_network.md)
— Simplify network by keeping only traversed edges  

**Traffic Assignment**

[`run_assignment()`](https://sebkrantz.github.io/flowr/reference/run_assignment.md)
— Run traffic assignment using path-sized logit model  

**Graph Utilities**

[`nodes_from_graph()`](https://sebkrantz.github.io/flowr/reference/nodes_from_graph.md)
— Extract unique nodes from graph  
[`normalize_graph()`](https://sebkrantz.github.io/flowr/reference/normalize_graph.md)
— Normalize node IDs to consecutive integers  
[`linestrings_from_graph()`](https://sebkrantz.github.io/flowr/reference/linestrings_from_graph.md)
— Convert graph to LINESTRING geometries  
[`dist_mat_from_graph()`](https://sebkrantz.github.io/flowr/reference/dist_mat_from_graph.md)
— Compute distance matrix from graph  

## Details

The package uses efficient C implementations for critical path
operations and leverages:

- `collapse` - Fast data transformations

- `igraph` - Graph operations and shortest path algorithms

## Author

Sebastian Krantz <sebastian.krantz@graduateinstitute.ch> and Kamol Roy
<kamol.roy08@gmail.com>
