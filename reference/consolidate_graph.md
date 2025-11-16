# Consolidate Graph

Consolidate a graph by removing intermediate nodes (nodes that occur
exactly twice) and optionally dropping loop, duplicate, and singleton
edges. This simplifies the network topology while preserving
connectivity.

## Usage

``` r
consolidate_graph(
  graph_df,
  directed = FALSE,
  drop.edges = c("loop", "duplicate", "single"),
  consolidate = TRUE,
  keep.nodes = NULL,
  ...,
  recursive = TRUE,
  verbose = TRUE
)
```

## Arguments

- graph_df:

  A data frame representing a graph with columns: `from` and `to` (node
  IDs), and optionally other columns to preserve. If coordinate columns
  (`FX`, `FY`, `TX`, `TY`) are present, they will be preserved and
  updated based on the consolidated node coordinates.

- directed:

  Logical (default: FALSE). Whether the graph is directed.

- drop.edges:

  Character vector (default: `c("loop", "duplicate", "single")`). Types
  of edges to drop: `"loop"` removes self-loops (edges where from ==
  to), `"duplicate"` removes duplicate edges (same from-to pair),
  `"single"` removes edges leading to singleton nodes (nodes that occur
  only once). Set to `NULL` to keep all edges.

- consolidate:

  Logical (default: TRUE). If TRUE, consolidates the graph by removing
  intermediate nodes (nodes that occur exactly twice) and merging
  connecting edges. If FALSE, only drops edges as specified in
  `drop.edges`.

- keep.nodes:

  Numeric vector (optional). Node IDs to preserve during consolidation,
  even if they occur exactly twice. Also used to preserve nodes when
  dropping singleton edges.

- ...:

  Arguments passed to
  [`collap()`](https://sebkrantz.github.io/collapse/reference/collap.html)
  for aggregation across consolidated edges. The defaults are
  `FUN = fmean` for numeric columns and `catFUN = fmode` for categorical
  columns. Select columns using `cols` or use argument
  `custom = list(fmean = cols1, fsum = cols2, fmode = cols3)` to map
  different columns to specific aggregation functions. It is highly
  recommended to weight the aggregation (using `w = ~ weight_col`) by
  the length/cost of the edges.

- recursive:

  Logical (default: TRUE). If TRUE, recursively consolidates the graph
  until no further consolidation is possible. This ensures that long
  chains of intermediate nodes are fully consolidated in a single call.

- verbose:

  Logical (default: TRUE). Whether to print messages about dropped edges
  and consolidation progress.

## Value

A data frame representing the consolidated graph with:

- `line` - Line identifier (added as first column)

- All columns from `graph_df` (aggregated if consolidation occurred),
  excluding `from`, `to`, and optionally `FX`, `FY`, `TX`, `TY` (which
  are re-added if present in original)

- `from`, `to` - Node IDs (updated after consolidation)

- Coordinate columns (`FX`, `FY`, `TX`, `TY`) if present in original

- Attribute `"keep.edges"` - Indices of original edges that were kept

- Attribute `"gid"` - Edge group IDs mapping consolidated edges to
  original edges

## Details

This function simplifies a graph by:

- **Dropping edges**: Optionally removes self-loops, duplicate edges,
  and edges leading to singleton nodes (nodes that appear only once in
  the graph)

- **Consolidating nodes**: Removes intermediate nodes (nodes that occur
  exactly twice) by merging the two edges connected through them into a
  single longer edge

- **Aggregating attributes**: When edges are merged, attributes/columns
  are aggregated using
  [`collap()`](https://sebkrantz.github.io/collapse/reference/collap.html).
  The default aggregation is mean for numeric columns and mode for
  categorical columns.

- **Recursive consolidation**: If `recursive = TRUE`, the function
  continues consolidating until no more nodes can be consolidated,
  ensuring complete simplification

Consolidation is useful for simplifying network topology while
preserving connectivity. For example, if node B connects A-\>B and
B-\>C, it will be removed and replaced with A-\>C. With
`recursive = TRUE`, long chains (A-\>B-\>C-\>D) are fully consolidated
to A-\>D in a single call.

For undirected graphs, the algorithm also handles cases where a node
appears twice as either origin or destination (circular cases).

If coordinate columns (`FX`, `FY`, `TX`, `TY`) are present in the input,
they are preserved and updated based on the consolidated node
coordinates from the original graph.

## See also

[create_undirected_graph](https://sebkrantz.github.io/flowr/reference/create_undirected_graph.md)
[simplify_network](https://sebkrantz.github.io/flowr/reference/simplify_network.md)
[flowr-package](https://sebkrantz.github.io/flowr/reference/flowr-package.md)
