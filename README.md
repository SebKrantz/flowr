# flowr

**Transport Modeling: Route Enumeration and Traffic Assignment with the Path-Sized Logit**

***NOTE: Package is still under development***

`flowr` provides efficient tools for transportation modeling, specifically route enumeration and traffic assignment tasks. The package implements the path-sized logit (PSL) model for traffic assignment and provides powerful utilities for network processing.

## Features

- **Path-Sized Logit Model**: Efficient traffic assignment accounting for route overlap
- **Network Processing**: Convert LINESTRING geometries to graphs, consolidate graphs, simplify networks, and handle directed/undirected graphs
- **Route Enumeration**: Efficient algorithm for finding alternative routes between origin-destination pairs
- **High Performance**: C implementations for critical path operations

## Installation

```r
# Install development version from GitHub
remotes::install_github("SebKrantz/flowr")
```

## Dependencies

- `collapse` (>= 2.0.0) - Fast data transformations
- `igraph` (>= 1.3.0) - Graph operations and shortest path algorithms
- `sf` (>= 1.0.0) - Spatial data handling

## Quick Start

### Basic Usage

```r
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

# Run traffic assignment (and route enumeration)
result <- run_assignment(graph_df, od_matrix_long)

# Access results
result$final_flows
```

### Working with Spatial Networks

```r
library(flowr)
library(sf)

# Read network from shapefile and create undirected graph (optional)
graph_df <- st_read("data/network.shp") |> 
  linestrings_to_graph() |>
  create_undirected_graph()

# Read zone centroids and get nearest nodes
od_zones <- st_read("data/od_zones.shp") |> st_centroid()
nodes <- nodes_from_graph(graph_df, sf = TRUE)
nearest_nodes <- st_nearest_feature(od_zones, nodes)

# Consolidate Graph (optional)
graph_df <- consolidate_graph(graph_df, keep = nearest_nodes, w = ~ cost)

# Simplify network by keeping only traversed edges along shortest paths (optional)
graph_df <- simplify_network(graph_df, nearest_nodes, cost.column = "cost")
```

## Main Functions

### Traffic Assignment and Route Enumeration

- **`run_assignment()`**:
  - Iterates through OD-pairs generating sensible alternative routes
  - Assigns traffic flows to network edges using path-sized logit model
  - Supports directed and undirected graphs
  - Returns flows and optional path/route information
  - **Key Parameters**:
    - **`beta`** (default: -1): Path-sized logit parameter (beta_PSL)
    - **`detour.max`** (default: 1.5): Maximum detour factor for alternative routes. Higher values consider more routes but increase computation time
    - **`angle.max`** (default: 90): Maximum detour angle in degrees (two-sided)
    - **`return.extra`**: Additional results to return from the route enumeration stage (`"graph"`, `"dmat"`, `"paths"`, `"edges"`, `"counts"`, `"costs"`, `"weights"`, or `"all"`)


### Network Processing

- **`linestrings_to_graph()`** - Convert LINESTRING geometries to graph data frame
- **`create_undirected_graph()`** - Convert directed graph to undirected with edge aggregation
- **`consolidate_graph()`** - Consolidate graph by removing intermediate nodes and merging edges
- **`simplify_network()`** - Simplify network by keeping only edges traversed by shortest paths

### Graph Utilities

- **`nodes_from_graph()`** - Extract unique nodes with coordinates from graph
- **`linestrings_from_graph()`** - Convert graph to LINESTRING geometries
- **`dist_mat_from_graph()`** - Compute distance matrix for all node pairs

## Example Workflow

```r
library(fastverse)
fastverse_extend(flowr, sf)

# 1. Load network and OD zone nodes
network <- st_read("data/network.shp")
od_zones <- st_read("data/od_zones.shp") |> st_centroid()
od_matrix <- fread("data/od_container_flows.csv") |> qM(1)
if(!all(dim(od_matrix) == nrow(od_zones))) stop("zones and OD matrix must match")

# 2. Convert network to graph
graph_df <- network |>
  linestrings_to_graph() |>
  create_undirected_graph()

# 3. Map zones to nearest network nodes
nodes <- nodes_from_graph(graph_df, sf = TRUE)
nearest_nodes <- st_nearest_feature(od_zones, nodes)

# 4. Prepare OD matrix
od_matrix_long <- data.frame(
  from = rep.int(nearest_nodes, ncol(od_matrix)),
  to = rep(nearest_nodes, each = nrow(od_matrix)),
  flow = vec(od_matrix)
) |> fsubset(is.finite(flow) & flow > 0)

# 5. Run assignment
result <- run_assignment(graph_df, od_matrix_long)

# 6. Visualize results (optional)
network$final_flows <- NA_real_
network$final_flows[attr(graph_df, "group.starts")] <- result$final_flows
mapview::mapview(network, zcol = "final_flows")
```

## Authors

- Sebastian Krantz (sebastian.krantz@graduateinstitute.ch)
- Kamol Roy (kamol.roy08@gmail.com)

## License

GPL-3

## Citation

If you use `flowr` in your research, please cite:

```r
citation("flowr")
```

