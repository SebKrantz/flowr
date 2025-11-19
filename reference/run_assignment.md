# Run Traffic Assignment

Assign traffic flows to network edges using path-sized logit (PSL)
model.

## Usage

``` r
run_assignment(
  graph_df,
  od_matrix_long,
  directed = FALSE,
  cost.column = "cost",
  method = "PSL",
  beta = -1,
  ...,
  detour.max = 1.5,
  angle.max = 90,
  return.extra = NULL,
  precompute.dmat = TRUE,
  verbose = TRUE
)

# S3 method for class 'flowr'
print(x, ...)
```

## Arguments

- graph_df:

  A data.frame with columns `from`, `to`, and optionally a cost column.
  Represents the network graph with edges between nodes.

- od_matrix_long:

  A data.frame with columns `from`, `to`, and `flow`. Represents the
  origin-destination matrix in long format with flow values.

- directed:

  Logical (default: FALSE). Whether the graph is directed.

- cost.column:

  Character string (default: "cost") or numeric vector. Name of the cost
  column in `graph_df`, or a numeric vector of edge costs with length
  equal to `nrow(graph_df)`.

- method:

  Character string (default: "PSL"). Assignment method. Currently only
  "PSL" (Path-Sized Logit) is implemented.

- beta:

  Numeric (default: -1). Path-sized logit parameter (beta_PSL).

- ...:

  Additional arguments (currently ignored).

- detour.max:

  Numeric (default: 1.5). Maximum detour factor for alternative routes
  (applied to shortest paths cost). This is a key parameter controlling
  the execution time of the algorithm: considering more routes (higher
  `detour.max`) substantially increases computation time.

- angle.max:

  Numeric (default: 90). Maximum detour angle (in degrees, two sided).
  I.e., nodes not within this angle measured against a straight line
  from origin to destination node will not be considered for detours.

- return.extra:

  Character vector specifying additional results to return. Options
  include: `"graph"`, `"dmat"`, `"paths"` (most memory intensive),
  `"edges"`, `"counts"`, `"costs"`, and `"weights"`. Use `"all"` to
  return all available extra results.

- precompute.dmat:

  description

- verbose:

  Show progress bar?

- x:

  An object of class `flowr`, typically returned by `run_assignment`.

## Value

A list containing:

- `call` - The function call

- `final_flows` - Numeric vector of assigned flows for each edge

- Additional elements as specified in `return.extra`

## Details

This function performs traffic assignment using a path-sized logit
model:

- Creates a graph from `graph_df` using igraph

- Computes shortest path distance matrix for all node pairs

- For each origin-destination pair in `od_matrix_long`:

  - Identifies alternative routes (detours) that are within `detour.max`
    of shortest path cost

  - Finds shortest paths from origin to intermediate nodes and from
    intermediate nodes to destination

  - Filters paths to remove those with duplicate edges

  - Computes path-sized logit probabilities accounting for route overlap

  - Assigns flows to edges based on probabilities

- Returns results including final flows and optionally additional
  information

The path-sized logit model accounts for route overlap by adjusting
probabilities based on the number of alternative routes using each edge.
Flows are assigned proportionally to the computed probabilities.

## See also

[flowr-package](https://sebkrantz.github.io/flowr/reference/flowr-package.md)
