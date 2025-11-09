#' Utility Functions
#'
#' Various utility functions for network processing, OD matrix processing,
#' graph conversion, and spatial calculations.
#' @title Process OD Matrix
#' @description Process OD matrix files from a directory.
#'
#' @param od_matrix_directory Character string path to directory containing OD matrix CSV files.
#'
#' @return A data.table containing processed OD matrix with columns:
#' \itemize{
#'   \item \code{orig} - Origin zone number
#'   \item \code{dest} - Destination zone number
#'   \item \code{cargo_type} - Cargo type
#'   \item \code{tons_2019} - Tonnage for 2019
#'   \item \code{tons_2030} - Tonnage for 2030
#'   \item \code{tons_2040} - Tonnage for 2040
#' }
#'
#' @details
#' Processes multiple OD matrix CSV files, handles different column name formats,
#' converts from matrix format to long format, and filters out zero tonnage rows.
#'
#' @export
#' @importFrom data.table fread rbindlist %ilike%
#' @importFrom collapse qM
process_od_matrix <- function(od_matrix_directory, cargo_type, period = NULL) {
  files <- list.files(od_matrix_directory)
  files <- files[files %ilike% period]
  # cargo_type = c("Container", "Drybulk", "Liquidbulk", "General", "HighValue")
  # period = "2019"
  sapply(cargo_type, function(x) {
    fread(paste(od_matrix_directory, files[files %ilike% x][1], sep = "/")) |> qM(1)
  }, simplify = FALSE)
}


#' @title Convert Linestring to Graph
#'
#' @param lines An sf data frame of LINESTRING geometries.
#' @return A data.frame representing the graph with columns:
#' \itemize{
#'  \item \code{line} - Line identifier
#'  \item \code{from} - Starting node ID
#'  \item \code{FX} - Starting node X-coordinate (longitude)
#'  \item \code{FY} - Starting node Y-coordinate (latitude)
#'  \item \code{to} - Ending node ID
#'  \item \code{TX} - Ending node X-coordinate (longitude)
#'  \item \code{TY} - Ending node Y-coordinate (latitude)
#' }
#' @export
linestring_to_graph <- function(lines, digits = 6) {
  gt <- st_geometry_type(lines, by_geometry = FALSE)
  if(length(gt) != 1L || gt != "LINESTRING") stop("lines needs to be a sf data frame of LINESTRING's")
  graph <- st_coordinates(lines) |> qDF()
  g <- GRP(list(line = graph$L1), return.order = FALSE)
  graph <- add_vars(fselect(graph, X, Y) |> ffirst(g, na.rm = FALSE, use.g.names = FALSE) |> add_stub("F"),
                    fselect(graph, X, Y) |> flast(g, na.rm = FALSE, use.g.names = FALSE) |> add_stub("T")) |>
           add_vars(g$groups, pos = "front")
  if(is.numeric(digits) && is.finite(digits)) fselect(graph, FX, FY, TX, TY) %<>% lapply(round, digits = digits)
  graph |>
    fmutate(from = unclass(group(FX, FY)),
            to = from[fmatch(list(TX, TY), list(FX, FY))],
            to = fifelse(is.na(to), unclass(group(TX, TY)) %+=% fmax(to), to)) |>
    colorder(line, from, FX, FY, to, TX, TY)
}

#' @title Create Undirected Graph
#' @description Convert a directed graph to an undirected graph by normalizing edges and aggregating duplicate connections.
#'
#' @param graph_df A data frame representing a directed graph with columns:
#'   \code{from}, \code{to}, \code{line}, \code{FX}, \code{FY}, \code{TX}, \code{TY},
#'   and any columns specified in \code{agg_cols}.
#' @param agg_cols Character vector (default: "cost"). Column names to aggregate
#'   when collapsing duplicate edges.
#' @param agg_func Function (default: \code{fmean}). Aggregation function to apply
#'   to columns specified in \code{agg_cols}. Must be a collapse package function
#'   (e.g., \code{fmean}, \code{fsum}, \code{fmin}, \code{fmax}).
#'
#' @return A data frame representing an undirected graph with:
#'   \itemize{
#'     \item \code{from} - Starting node ID (normalized to be < \code{to})
#'     \item \code{to} - Ending node ID (normalized to be > \code{from})
#'     \item \code{line} - Line identifier (first value from duplicates)
#'     \item \code{FX} - Starting node X-coordinate (first value from duplicates)
#'     \item \code{FY} - Starting node Y-coordinate (first value from duplicates)
#'     \item \code{TX} - Ending node X-coordinate (first value from duplicates)
#'     \item \code{TY} - Ending node Y-coordinate (first value from duplicates)
#'     \item Aggregated columns as specified in \code{agg_cols}
#'   }
#'
#' @details
#' This function converts a directed graph to an undirected graph by:
#' \itemize{
#'   \item Normalizing edge directions so that \code{from < to} for all edges
#'   \item Collapsing duplicate edges (same \code{from} and \code{to} nodes)
#'   \item For spatial/identifier columns (\code{line}, \code{FX}, \code{FY}, \code{TX}, \code{TY}),
#'     taking the first value from duplicates
#'   \item For aggregation columns (specified in \code{agg_cols}), applying the
#'     specified aggregation function (e.g., mean, sum, min, max)
#' }
#'
#' @export
#' @importFrom collapse ftransform collap ffirst fmean
create_undirected_graph <- function(graph_df, agg_cols = "cost", agg_func = fmean) {
  graph_df |>
    ftransform(from = pmin(from, to), to = pmax(from, to)) |>
    collap( ~ from + to, custom = list(c("line", "FX", "FY", "TX", "TY"), agg_cols) |>
              setNames(c("ffirst", deparse(substitute(agg_func)))), sort = FALSE)
}

#' @title Extract Nodes from Graph
#' @description Extract unique nodes with their coordinates from a graph data frame.
#'
#' @param graph_df A data frame representing a graph with columns:
#'   \code{from}, \code{to}, \code{FX}, \code{FY}, \code{TX}, \code{TY}.
#'
#' @return A data frame with unique nodes and their coordinates:
#'   \itemize{
#'     \item \code{node} - Node ID
#'     \item \code{X} - Node X-coordinate (longitude)
#'     \item \code{Y} - Node Y-coordinate (latitude)
#'   }
#'   Nodes are sorted by node ID.
#'
#' @details
#' This function extracts all unique nodes from both the \code{from} and \code{to}
#' columns of the graph, along with their corresponding coordinates. Duplicate nodes
#' are removed, keeping only unique node IDs with their coordinates.
#'
#' @export
#' @importFrom collapse rowbind fselect funique
nodes_from_graph <- function(graph_df) {
  rowbind(graph_df |> fselect(from, FX, FY),
          graph_df |> fselect(to, TX, TY), use.names = FALSE) |>
    set_names(c("node", "X", "Y")) |>
    funique(cols = "node", sort = TRUE)
}

#' @title Compute Distance Matrix from Graph
#' @description Compute a distance matrix for all node pairs in a graph using cppRouting.
#'
#' @param graph_df A data frame representing a graph with columns:
#'   \code{from}, \code{to}, and \code{cost}.
#' @param directed Logical (default: FALSE). If TRUE, treats the graph as directed;
#'   if FALSE, treats it as undirected.
#' @param algorithm Character string (default: "mch"). Algorithm to use for distance
#'   computation. Options include "mch" (many-to-many with contraction hierarchies),
#'   "dijkstra", "bi", etc. See \code{cppRouting::get_distance_matrix} for details.
#' @param ... Additional arguments passed to \code{get_distance_matrix}.
#'
#' @return A matrix of distances between all node pairs, where rows and columns
#'   correspond to node IDs. The matrix contains the shortest path distances
#'   (based on the \code{cost} column) between all pairs of nodes.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Converts the graph data frame to a cppRouting graph object
#'   \item Contracts the graph for efficient distance computation
#'   \item Computes the distance matrix for all node pairs using the specified algorithm
#' }
#' The graph is contracted using \code{cpp_contract} to optimize distance calculations,
#' which is particularly efficient for repeated queries.
#'
#' @export
#' @importFrom collapse fselect
#' @importFrom cppRouting makegraph cpp_contract get_distance_matrix
dist_mat_from_graph <- function(graph_df, directed = FALSE, algorithm = "mch", ...) {
  graph <- makegraph(graph_df |> fselect(from, to, cost), directed = directed) # directed = FALSE # cpp_simplify()
  nodes <- graph$dict$ref
  get_distance_matrix(cpp_contract(graph), from = nodes, to = nodes, algorithm = algorithm, ...)
}

#' @title Check Path Duplicates
#' @description Check if combined paths (from paths1 and paths2) have duplicated edges.
#'
#' @param paths1 List of integer vectors containing edge numbers for the first part of paths.
#' @param paths2 List of integer vectors containing edge numbers for the second part of paths.
#' @param delta_ks Integer vector used as a hash table. Must be large enough to index all
#'   edge numbers (i.e., length should be >= max edge number + 1).
#'
#' @return An integer vector of with the indices of paths for which no duplicates exist.
#'
#' @details
#' For each path index k, this function combines \code{paths1[[k]]} and \code{paths2[[k]]}
#' into a single path and checks if any edge appears more than once. The function uses
#' \code{delta_ks} as a hash table that is cleared after checking each path.
#'
#' The function requires that:
#' \itemize{
#'   \item \code{paths1} and \code{paths2} are lists of equal length
#'   \item Each element of \code{paths1} and \code{paths2} is an integer vector
#'   \item \code{delta_ks} is an integer vector with length >= max edge number + 1
#' }
#' @useDynLib flowr, .registration = TRUE
check_path_duplicates <- function(paths1, paths2, delta_ks) {
  .Call(C_check_path_duplicates, paths1, paths2, delta_ks)
}

#' @title Compute Path-Sized Logit
#' @description Efficiently compute path-sized logit probabilities and update flows.
#'
#' @param paths1 List of numeric vectors containing edge numbers for first part of paths.
#' @param paths2 List of numeric vectors containing edge numbers for second part of paths.
#' @param no_dups Integer vector of path indices (1-based) that have no duplicate edges.
#' @param shortest_path Numeric vector of edge numbers for the shortest path.
#' @param cost Numeric vector of edge costs.
#' @param cost_ks Numeric vector of path costs for paths in no_dups.
#' @param d_ij Numeric scalar, cost of shortest path.
#' @param beta_PSL Numeric scalar, path-sized logit parameter.
#' @param flow Numeric scalar, flow value for this OD pair.
#' @param delta_ks Integer vector used as hash table (will be modified and reset).
#' @param final_flows Numeric vector of final flows (will be modified in place).
#' @param free_delta_ks Logical scalar (default: TRUE). If TRUE, resets delta_ks to zero after computation.
#'
#' @return Numeric vector of probabilities with length \code{length(no_dups) + 1}.
#'   The last element corresponds to the shortest path.
#'
#' @details
#' This function efficiently computes:
#' \itemize{
#'   \item Updates delta_ks (edge usage counts) for all paths
#'   \item Computes gamma correction factors for path-sized logit
#'   \item Computes probabilities using exponential utility with path-sized correction
#'   \item Resets delta_ks to zero if \code{free_delta_ks} is TRUE
#'   \item Updates final_flows with weighted probabilities
#' }
#'
#' @useDynLib flowr, .registration = TRUE
compute_path_sized_logit <- function(paths1, paths2, no_dups, shortest_path,
                                     cost, cost_ks, d_ij, beta_PSL, flow,
                                     delta_ks, final_flows, free_delta_ks = TRUE) {
  .Call(C_compute_path_sized_logit, paths1, paths2, no_dups, shortest_path,
        cost, cost_ks, d_ij, beta_PSL, flow, delta_ks, final_flows, free_delta_ks)
}

#' @title Get Geo Data Frame
#' @description Convert data.table to sf spatial data frame.
#'
#' @param df A data.table or data.frame.
#' @param lat_col Character string specifying the latitude column name.
#' @param lon_col Character string specifying the longitude column name.
#'
#' @return An sf object with point geometry.
#'
#' @export
#' @importFrom sf st_as_sf st_set_crs
get_geo_dataframe <- function(df, lat_col, lon_col) {
  stop("Not yet implemented")
}

#' @title Calculate Haversine Distance
#' @description Calculate haversine distance between two points on Earth.
#'
#' @param lat1 Numeric vector of latitude values for first point(s).
#' @param lon1 Numeric vector of longitude values for first point(s).
#' @param lat2 Numeric vector of latitude values for second point(s).
#' @param lon2 Numeric vector of longitude values for second point(s).
#'
#' @return Numeric vector of distances in meters.
#'
#' @details
#' Uses the haversine formula to calculate great-circle distances between points
#' on a sphere. Earth's radius is assumed to be 6371000 meters.
#'
#' @export
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  stop("Not yet implemented")
}

#' @title Convert Shapefile to Graph
#' @description Convert shapefile to routable graph (cppRouting or igraph).
#'
#' @param gdf An sf object with network data (linestrings).
#' @param make_G_bidi Logical (default: TRUE). If TRUE, assumes linestrings are bidirectional.
#' @param name Character string (default: "unnamed"). Optional name of graph.
#'
#' @return A graph object compatible with cppRouting or igraph.
#'
#' @details
#' Validates that the shapefile contains linestring geometry and length attributes.
#' Creates a graph with nodes and edges, optionally adding bidirectional edges.
#'
#' @export
#' @importFrom sf st_geometry_type
convert_shp2graph <- function(gdf, make_G_bidi = TRUE, name = "unnamed") {
  stop("Not yet implemented")
}

#' @title Get Link GDF with Generalized Cost
#' @description Calculate generalized cost for links based on cargo type and scenario.
#'
#' @param link_gdf An sf object with network link data.
#' @param cargo_type Character string specifying the cargo type.
#' @param scenario_name Character string specifying the scenario name.
#'
#' @return An sf object with additional columns for:
#' \itemize{
#'   \item \code{tariff_cost} - Tariff costs
#'   \item \code{vot} - Value of time
#'   \item \code{BPC_cost} - Border crossing costs
#'   \item \code{BPC_time} - Border crossing times
#'   \item \code{Speed} - Link speeds
#'   \item \code{TP_Cost} - Transfer penalty costs
#'   \item \code{TP_Time} - Transfer penalty times
#'   \item \code{total_cost} - Total cost
#'   \item \code{total_time} - Total time
#'   \item \code{generalized_cost} - Generalized cost
#' }
#'
#' @details
#' Reads parameter files from the scenario directory and applies:
#' \itemize{
#'   \item Tariffs based on mode and length
#'   \item Value of time assumptions
#'   \item Border crossing costs and times
#'   \item Speed assumptions by mode and region
#'   \item Transfer penalty costs and times
#' }
#' Then calculates generalized cost as: (total_cost)/(vot*24) + (total_time/24)
#'
#' @export
get_link_gdf_with_gc <- function(link_gdf, cargo_type, scenario_name) {
  stop("Not yet implemented")
}

#' @title Calculate Bearing
#' @description Calculate bearing angle of a line segment.
#'
#' @param segment An sf linestring object.
#'
#' @return Numeric vector of bearing angles in degrees (0-360).
#'
#' @details
#' Calculates the angle from north (0 degrees) in clockwise direction.
#'
#' @export
calculate_bearing <- function(segment) {
  stop("Not yet implemented")
}

#' @title Assign Direction
#' @description Assign cardinal direction based on bearing.
#'
#' @param bearing Numeric vector of bearing angles in degrees.
#'
#' @return Character vector of directions ("NB" for northbound, "SB" for southbound).
#'
#' @details
#' \itemize{
#'   \item 0-90 degrees or 270-360 degrees: Northbound (NB)
#'   \item 90-270 degrees: Southbound (SB)
#' }
#'
#' @export
assign_direction <- function(bearing) {
  stop("Not yet implemented")
}

#' @title Find Nearest Nodes
#' @description Find nearest nodes in a graph for given coordinates using haversine distance.
#'
#' @param graph A graph object (from cppRouting or igraph).
#' @param x_coords Numeric vector of longitude coordinates.
#' @param y_coords Numeric vector of latitude coordinates.
#' @param return_dist Logical (default: FALSE). If TRUE, return distances as well.
#'
#' @return If \code{return_dist = FALSE}, returns numeric vector of nearest node IDs.
#'   If \code{return_dist = TRUE}, returns a list with:
#'   \itemize{
#'     \item \code{nodes} - Numeric vector of nearest node IDs
#'     \item \code{distances} - Numeric vector of distances in meters
#'   }
#'
#' @export
find_nearest_nodes <- function(graph, x_coords, y_coords, return_dist = FALSE) {
  stop("Not yet implemented")
}

#' @title Load Choice Parameters
#' @description Load choice model parameters from Excel file.
#'
#' @param parameter_directory Character string path to directory containing parameter files.
#'
#' @return A list with three data.tables:
#' \itemize{
#'   \item \code{gen_lambda_df} - Lambda parameters by mode
#'   \item \code{mode_lambda_df} - Upper level lambda parameters by mode
#'   \item \code{mode_c_df} - C parameters by mode
#' }
#'
#' @details
#' Reads from \code{choice_params.xlsx} with sheets: 'lambda', 'lambda_mode', 'C'.
#'
#' @export
load_choice_parameters <- function(parameter_directory) {
  stop("Not yet implemented")
}

