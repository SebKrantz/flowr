# Compute Distance Matrix from Graph

Compute a distance matrix for all node pairs in a graph using
cppRouting.

## Usage

``` r
dist_mat_from_graph(graph_df, directed = FALSE, cost.column = "cost", ...)
```

## Arguments

- graph_df:

  A data frame representing a graph with columns: `from`, `to`, and
  `cost`.

- directed:

  Logical (default: FALSE). If TRUE, treats the graph as directed; if
  FALSE, treats it as undirected.

- cost.column:

  Character string (optional). Name of the cost column in `graph_df`.
  Alternatively, a numeric vector of edge costs with length equal to
  `nrow(graph_df)`.

- ...:

  Additional arguments passed to
  [`distances()`](https://r.igraph.org/reference/distances.html).

## Value

A matrix of distances between all node pairs, where rows and columns
correspond to node IDs. The matrix contains the shortest path distances
(based on the `cost` column) between all pairs of nodes.

## Details

This function:

- Converts the graph data frame to a cppRouting graph object

- Contracts the graph for efficient distance computation

- Computes the distance matrix for all node pairs using the specified
  algorithm

The graph is contracted using `cpp_contract` to optimize distance
calculations, which is particularly efficient for repeated queries.
