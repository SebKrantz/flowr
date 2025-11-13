# Simplify Network

Simplify a network by keeping only edges that are traversed by shortest
paths between origin-destination pairs in the OD matrix.

## Usage

``` r
simplify_network(x, od_matrix_long, cost.column = NULL)
```

## Arguments

- x:

  Either an sf object with LINESTRING geometry representing the network,
  or a data.frame with columns `from` and `to` representing the graph
  edges.

- od_matrix_long:

  A data.frame representing the origin-destination matrix in long
  format. If `x` is a LINETRING geometry, it should have columns `FX`,
  `FY`, `TX`, `TY` representing the origin/destination zone centroids.
  If `x` is a graph data frame, it should have columns `from` and `to`
  matching nodes in `x`.

- cost.column:

  Character string (optional). Name of the cost column in `x`. If `NULL`
  and `x` is an sf object, uses `st_length(x)` as the cost.

## Value

If `x` is an sf object, returns a list with:

- `network` - sf object containing only edges that were traversed

- `graph_df` - data.frame with graph representation of traversed edges

If `x` is a data.frame, returns a data.frame containing only edges that
were traversed.

## Details

This function simplifies a network by:

- Converting the input to a graph representation (if needed)

- Validating that all origin and destination nodes exist in the network

- Computing shortest paths from each origin to all destinations using
  igraph

- Marking all edges that are traversed by at least one shortest path

- Returning only the subset of edges that were traversed

The function filters the OD matrix to include only rows with finite,
positive flow values. All shortest paths are computed using edge costs
(either from `cost.column` or geometric length for sf objects).
