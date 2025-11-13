# Create Undirected Graph

Convert a directed graph to an undirected graph by normalizing edges and
aggregating duplicate connections.

## Usage

``` r
create_undirected_graph(
  graph_df,
  cols.aggregate = "cost",
  fun.aggregate = fmean,
  ...
)
```

## Arguments

- graph_df:

  A data frame representing a directed graph with columns: `from`, `to`,
  `line`, `FX`, `FY`, `TX`, `TY`, and any columns specified in
  `cols.aggregate`.

- cols.aggregate:

  Character vector (default: "cost"). Column names to aggregate when
  collapsing duplicate edges.

- fun.aggregate:

  Function (default: `fmean`). Aggregation function to apply to columns
  specified in `cols.aggregate`. Must be a collapse package function
  (e.g., `fmean`, `fsum`, `fmin`, `fmax`).

- ...:

  Further arguments to pass to `fun.aggregate`.

## Value

A data frame representing an undirected graph with:

- `from` - Starting node ID (normalized to be \< `to`)

- `to` - Ending node ID (normalized to be \> `from`)

- `line` - Line identifier (first value from duplicates)

- `FX` - Starting node X-coordinate (first value from duplicates)

- `FY` - Starting node Y-coordinate (first value from duplicates)

- `TX` - Ending node X-coordinate (first value from duplicates)

- `TY` - Ending node Y-coordinate (first value from duplicates)

- Aggregated columns as specified in `cols.aggregate`

## Details

This function converts a directed graph to an undirected graph by:

- Normalizing edge directions so that `from < to` for all edges

- Collapsing duplicate edges (same `from` and `to` nodes)

- For spatial/identifier columns (`line`, `FX`, `FY`, `TX`, `TY`),
  taking the first value from duplicates

- For aggregation columns (specified in `cols.aggregate`), applying the
  specified aggregation function (e.g., mean, sum, min, max)
