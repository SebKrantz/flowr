# flowr

**Transport Modeling: Route Enumeration and Traffic Assignment with the
Path-Sized Logit**

***NOTE: Package is still under development***

`flowr` provides efficient tools for transportation modeling,
specifically route enumeration and traffic assignment tasks. The package
implements the path-sized logit (PSL) model for traffic assignment and
provides powerful utilities for network processing.

## Features

- **Path-Sized Logit Model**: Efficient traffic assignment accounting
  for route overlap
- **Network Processing**: Convert LINESTRING geometries to graphs,
  simplify networks, and handle directed/undirected graphs
- **Route Enumeration**: Efficient algorithm for finding alternative
  routes between origin-destination pairs
- **High Performance**: C implementations for critical path operations

## Installation

``` r
# Install from source
install.packages("flowr", repos = NULL, type = "source")
```

## Dependencies

- `collapse` (\>= 2.0.0) - Fast data transformations
- `igraph` (\>= 1.3.0) - Graph operations and shortest path algorithms
- `sf` (\>= 1.0.0) - Spatial data handling

## Quick Start

### Basic Usage

``` r
library(flowr)

# Create a small graph data frame
graph_df <- data.frame(
  from = c(1, 2, 2, 3),
  to   = c(2, 3, 4, 4),
  cost = c(5, 3, 2, 4)
)

# Prepare OD matrix with the same node IDs as in graph_df
od_matrix_long <- data.frame(
  from = c(1, 2, 3),
  to   = c(4, 4, 4),
  flow = c(100, 80, 60)
)

# Run traffic assignment
result <- run_assignment(
  graph_df = graph_df,
  od_matrix_long = od_matrix_long
)

# Access results
result$final_flows
```

### Working with Spatial Networks

``` r
library(flowr)
library(sf)

# Read network from shapefile
network <- st_read("data/network/base_network.shp")

# Simplify network by keeping only traversed edges along shortest paths
simplified <- simplify_network(
  x = network,
  od_matrix_long = od_matrix_long,
  cost.column = "cost"  # Uses st_length() if NULL
)

# Convert to graph
graph_df <- simplified |>
  linestrings_to_graph(keep.cols = "cost") |>
  create_undirected_graph()
```

## Main Functions

### Traffic Assignment

- **[`run_assignment()`](https://sebkrantz.github.io/flowr/reference/run_assignment.md)** -
  Assign traffic flows to network edges using path-sized logit model
  - Supports directed and undirected graphs
  - Configurable detour factors and angle constraints
  - Returns flows and optional path information

### Network Processing

- **[`linestrings_to_graph()`](https://sebkrantz.github.io/flowr/reference/linestrings_to_graph.md)** -
  Convert LINESTRING geometries to graph data frame
- **[`create_undirected_graph()`](https://sebkrantz.github.io/flowr/reference/create_undirected_graph.md)** -
  Convert directed graph to undirected with edge aggregation
- **[`simplify_network()`](https://sebkrantz.github.io/flowr/reference/simplify_network.md)** -
  Simplify network by keeping only edges traversed by shortest paths

### Graph Utilities

- **[`nodes_from_graph()`](https://sebkrantz.github.io/flowr/reference/nodes_from_graph.md)** -
  Extract unique nodes with coordinates from graph
- **[`linestrings_from_graph()`](https://sebkrantz.github.io/flowr/reference/linestrings_from_graph.md)** -
  Convert graph to LINESTRING geometries
- **[`dist_mat_from_graph()`](https://sebkrantz.github.io/flowr/reference/dist_mat_from_graph.md)** -
  Compute distance matrix for all node pairs

## Key Parameters

### `run_assignment()`

- **`beta`** (default: -1): Path-sized logit parameter (beta_PSL)
- **`detour.max`** (default: 1.5): Maximum detour factor for alternative
  routes. Higher values consider more routes but increase computation
  time
- **`angle.max`** (default: 90): Maximum detour angle in degrees
  (two-sided)
- **`return.extra`**: Additional results to return (`"graph"`, `"dmat"`,
  `"paths"`, `"edges"`, `"costs"`, `"weights"`, or `"all"`)

## Example Workflow

``` r
library(flowr)
library(sf)

# 1. Load network and zone nodes
network <- st_read("data/network/base_network.shp")
zone_nodes <- st_read("data/zone_nodes/network_nodes.shp")

# 2. Convert network to graph
graph_df <- network |>
  linestrings_to_graph(keep.cols = "cost") |>
  create_undirected_graph()

# 3. Map zones to nearest network nodes
nodes_df <- nodes_from_graph(graph_df)
nearest_nodes <- st_nearest_feature(
  zone_nodes,
  st_as_sf(nodes_df, coords = c("X", "Y"), crs = 4326)
)

# 4. Prepare OD matrix
od_matrix_long <- data.frame(
  from = rep.int(nearest_nodes, ncol(od_matrix)),
  to = rep(nearest_nodes, each = nrow(od_matrix)),
  flow = vec(od_matrix)
) |> fsubset(is.finite(flow) & flow > 0)

# 5. Run assignment
result <- run_assignment(
  graph_df = graph_df,
  od_matrix_long = od_matrix_long,
  return.extra = "all"
)

# 6. Visualize results
network$final_flows <- NA_real_
network$final_flows[attr(graph_df, "group.starts")] <- result$final_flows
```

## Authors

- Sebastian Krantz (<sebastian.krantz@graduateinstitute.ch>)
- Kamol Roy (<kamol.roy08@gmail.com>)

## License

GPL-3

## Citation

If you use `flowr` in your research, please cite:

``` r
citation("flowr")
```
