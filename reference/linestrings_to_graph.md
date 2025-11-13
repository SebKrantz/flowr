# Convert Linestring to Graph

Convert Linestring to Graph

## Usage

``` r
linestrings_to_graph(lines, digits = 6, keep.cols = NULL)
```

## Arguments

- lines:

  An sf data frame of LINESTRING geometries.

- digits:

  Numeric rounding applied to coordinates (to ensure that matching
  points across different linestrings is not impaired by numeric
  precision issues). Set to `NA/Inf/FALSE` to disable.

- keep.cols:

  Character vector of column names to keep from the input data frame.

## Value

A data.frame representing the graph with columns:

- `line` - Line identifier

- `from` - Starting node ID

- `FX` - Starting node X-coordinate (longitude)

- `FY` - Starting node Y-coordinate (latitude)

- `to` - Ending node ID

- `TX` - Ending node X-coordinate (longitude)

- `TY` - Ending node Y-coordinate (latitude)

## See also

[simplify_network](https://sebkrantz.github.io/flowr/reference/simplify_network.md)
[flowr-package](https://sebkrantz.github.io/flowr/reference/flowr-package.md)
