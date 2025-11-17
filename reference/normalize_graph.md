# Normalize Graph Node IDs

Normalize node IDs in a graph to be consecutive integers starting
from 1. This is useful for ensuring compatibility with graph algorithms
that require sequential node IDs.

## Usage

``` r
normalize_graph(graph_df)
```

## Arguments

- graph_df:

  A data frame representing a graph with columns: `from` and `to` (node
  IDs).

## Value

A data frame with the same structure as `graph_df`, but with `from` and
`to` columns remapped to consecutive integer IDs starting from 1. All
other columns are preserved unchanged.

## Details

This function:

- Extracts all unique node IDs from both `from` and `to` columns

- Sorts them in ascending order

- Remaps the original node IDs to sequential integers (1, 2, 3, ...)

- Updates both `from` and `to` columns with the normalized IDs

Normalization is useful when:

- Node IDs are non-consecutive (e.g., 1, 5, 10, 20)

- Node IDs are non-numeric or contain gaps

- Graph algorithms require sequential integer node IDs starting from 1

Note: This function only normalizes the node IDs; it does not modify the
graph structure or any other attributes. The mapping preserves the
relative ordering of nodes.

## See also

[nodes_from_graph](https://sebkrantz.github.io/flowr/reference/nodes_from_graph.md)
[flowr-package](https://sebkrantz.github.io/flowr/reference/flowr-package.md)
