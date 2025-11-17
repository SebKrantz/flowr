# Create Undirected Graph

Convert a directed graph to an undirected graph by normalizing edges and
aggregating duplicate connections.

## Usage

``` r
create_undirected_graph(graph_df, ...)
```

## Arguments

- graph_df:

  A data frame representing a directed graph including columns: `from`,
  `to`, and (optionally) `line`, `FX`, `FY`, `TX`, `TY`.

- ...:

  Arguments passed to
  [`collap()`](https://sebkrantz.github.io/collapse/reference/collap.html)
  for aggregation across duplicated (diretional) edges. The defaults are
  `FUN = fmean` for numeric columns and `catFUN = fmode` for categorical
  columns. Select columns using `cols` or use argument
  `custom = list(fmean = cols1, fsum = cols2, fmode = cols3)` to map
  different columns to specific aggregation functions. You can weight
  the aggregation (using `w = ~ weight_col`).

## Value

A data frame representing an undirected graph with:

- `line` - Line identifier (first value from duplicates)

- `from` - Starting node ID (normalized to be \< `to`)

- `to` - Ending node ID (normalized to be \> `from`)

- `FX` - Starting node X-coordinate (first value from duplicates)

- `FY` - Starting node Y-coordinate (first value from duplicates)

- `TX` - Ending node X-coordinate (first value from duplicates)

- `TY` - Ending node Y-coordinate (first value from duplicates)

- Aggregated columns

## Details

This function converts a directed graph to an undirected graph by:

- Normalizing edge directions so that `from < to` for all edges

- Collapsing duplicate edges (same `from` and `to` nodes)

- For spatial/identifier columns (`line`, `FX`, `FY`, `TX`, `TY`),
  taking the first value from duplicates

- For aggregation columns,
  [`collap()`](https://sebkrantz.github.io/collapse/reference/collap.html)
  will be applied.
