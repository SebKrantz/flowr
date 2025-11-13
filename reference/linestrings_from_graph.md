# Convert Graph to Linestrings

Convert a graph data frame with node coordinates to an sf object with
LINESTRING geometries.

## Usage

``` r
linestrings_from_graph(graph_df, crs = 4326)
```

## Arguments

- graph_df:

  A data frame representing a graph with columns: `FX`, `FY`, `TX`, `TY`
  (starting and ending node coordinates), and optionally other columns
  to preserve.

- crs:

  Numeric or character (default: 4326). Coordinate reference system to
  assign to the output sf object.

## Value

An sf data frame with LINESTRING geometry, containing all columns from
`graph_df` except `FX`, `FY`, `TX`, and `TY`. Each row represents an
edge as a LINESTRING connecting the from-node (`FX`, `FY`) to the
to-node (`TX`, `TY`).

## Details

This function is the inverse operation of
[`linestrings_to_graph`](https://sebkrantz.github.io/flowr/reference/linestrings_to_graph.md).
It:

- Creates LINESTRING geometries from node coordinates (`FX`, `FY`, `TX`,
  `TY`)

- Removes the coordinate columns from the output

- Preserves all other columns from the input graph data frame

- Returns an sf object suitable for spatial operations and visualization

## See also

[linestrings_to_graph](https://sebkrantz.github.io/flowr/reference/linestrings_to_graph.md)
[flowr-package](https://sebkrantz.github.io/flowr/reference/flowr-package.md)
