# Consolidate Graph

Consolidate a graph by removing intermediate nodes (nodes that occur
exactly twice) and optionally dropping loops and multiple edges. This
simplifies the network topology while preserving connectivity.

## Usage

``` r
consolidate_graph(
  graph_df,
  directed = FALSE,
  drop.edges = c("loops", "multiple"),
  consolidate = TRUE,
  keep.nodes = NULL,
  fun.aggregate = fmean,
  ...,
  verbose = TRUE
)
```

## Arguments

- graph_df:

  A data frame representing a graph with columns: `from` and `to` (node
  IDs), and optionally other columns to preserve.

- directed:

  Logical (default: FALSE). Whether the graph is directed.

- drop.edges:

  Character vector (default: `c("loops", "multiple")`). Types of edges
  to drop: `"loops"` removes self-loops (edges where from == to),
  `"multiple"` removes duplicate edges. Set to `NULL` to keep all edges.

- consolidate:

  Logical (default: TRUE). If TRUE, consolidates the graph by removing
  intermediate nodes (nodes that occur exactly twice) and merging
  connecting edges. If FALSE, only drops edges as specified in
  `drop.edges`.

- keep.nodes:

  Numeric vector (optional). Node IDs to preserve during consolidation,
  even if they occur exactly twice.

- fun.aggregate:

  Function (default: `fmean`). Aggregation function to apply to columns
  when consolidating edges. Must be a collapse package function (e.g.,
  `fmean`, `fsum`, `fmin`, `fmax`).

- ...:

  Further arguments passed to `fun.aggregate`.

- verbose:

  Logical (default: TRUE). Whether to print messages about dropped
  edges.

## Value

A data frame representing the consolidated graph with:

- All columns from `graph_df` (aggregated if consolidation occurred)

- Attribute `"keep.edges"` - Indices of original edges that were kept

- Attribute `"gid"` - Edge group IDs mapping consolidated edges to
  original edges

## Details

This function simplifies a graph by:

- **Dropping edges**: Optionally removes self-loops and duplicate edges

- **Consolidating nodes**: Removes intermediate nodes (nodes that occur
  exactly twice) by merging the two edges connected through them into a
  single longer edge

- **Aggregating attributes**: When edges are merged, numeric attributes
  are aggregated using `fun.aggregate` (e.g., mean, sum, min, max)

Consolidation is useful for simplifying network topology while
preserving connectivity. For example, if node B connects A-\>B and
B-\>C, it will be removed and replaced with A-\>C. The process continues
iteratively until no more nodes can be consolidated.

For undirected graphs, the algorithm also handles cases where a node
appears twice as either origin or destination.

## See also

[create_undirected_graph](https://sebkrantz.github.io/flowr/reference/create_undirected_graph.md)
[simplify_network](https://sebkrantz.github.io/flowr/reference/simplify_network.md)
[flowr-package](https://sebkrantz.github.io/flowr/reference/flowr-package.md)
