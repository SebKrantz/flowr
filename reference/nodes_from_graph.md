# Extract Nodes from Graph

Extract unique nodes with their coordinates from a graph data frame.

## Usage

``` r
nodes_from_graph(graph_df, return.sf = FALSE, crs = 4326)
```

## Arguments

- graph_df:

  A data frame representing a graph with columns: `from`, `to`, `FX`,
  `FY`, `TX`, `TY`.

- return.sf:

  Logical. If TRUE, returns result as an `sf` POINT object. Default:
  FALSE.

- crs:

  Coordinate reference system for sf output; default is 4326.

## Value

A data frame (or sf object if `return.sf = TRUE`) with unique nodes and
coordinates:

- `node` - Node ID

- `X` - Node X-coordinate (typically longitude)

- `Y` - Node Y-coordinate (typically latitude)

Result is sorted by node ID.

## Details

This function extracts all unique nodes from both the `from` and `to`
columns of the graph, along with their corresponding coordinates.
Duplicate nodes are removed, keeping only unique node IDs with their
coordinates.
