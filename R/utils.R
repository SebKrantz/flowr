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
#' @importFrom data.table fread %ilike%
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
#' @importFrom sf st_geometry_type st_coordinates
#' @importFrom collapse qDF GRP add_vars fselect ffirst flast add_stub fmutate group fmatch %+=% fmax colorder
#' @importFrom data.table fifelse
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
create_undirected_graph <- function(graph_df, agg_cols = "cost", agg_func = fmean, ...) {
  graph_df %<>% ftransform(from = pmin(from, to), to = pmax(from, to))
  g <- GRP(graph_df, ~ from + to, sort = FALSE)
  if(last(as.character(substitute(agg_func))) %!in% .FAST_STAT_FUN)
    agg_func <- function(x, g, ...) BY(x, g, agg_func, ...)
  res <- add_vars(g$groups,
    ffirst(get_vars(graph_df, c("line", "FX", "FY", "TX", "TY")), g, use.g.names = FALSE),
    agg_func(get_vars(graph_df, agg_cols), g, use.g.names = FALSE)) |>
    colorderv(c("line", "from", "FX", "FY", "to", "TX", "TY", agg_cols))
  attr(res, "group.starts") <- g$group.starts
  res
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
dist_mat_from_graph <- function(graph_df, directed = FALSE, ...) {

  # graph <- makegraph(graph_df |> fselect(from, to, cost), directed = directed) # directed = FALSE # cpp_simplify()
  # nodes <- graph$dict$ref
  # get_distance_matrix(cpp_contract(graph), from = nodes, to = nodes, algorithm = algorithm, ...)
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
#' @useDynLib mmflowr, .registration = TRUE
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
#'   \item Resets delta_ks to zero
#'   \item Updates final_flows with weighted probabilities
#' }
#'
#' @useDynLib mmflowr, .registration = TRUE
compute_path_sized_logit <- function(paths1, paths2, no_dups, shortest_path,
                                     cost, cost_ks, d_ij, beta_PSL,
                                     flow, delta_ks, final_flows) {
  .Call(C_compute_path_sized_logit, paths1, paths2, no_dups, shortest_path,
        cost, cost_ks, d_ij, beta_PSL, flow, delta_ks, final_flows)
}


#' @title Simplify Network
#' @description Simplify a network by keeping only edges that are traversed by shortest paths
#'   between origin-destination pairs in the OD matrix.
#'
#' @param x Either an sf object with LINESTRING geometry representing the network, or a
#'   data.frame with columns \code{from} and \code{to} representing the graph edges.
#' @param od_matrix_long A data.frame with columns \code{from}, \code{to}, and \code{flow}
#'   representing the origin-destination matrix in long format.
#' @param cost_col Character string (optional). Name of the cost column in \code{x} or
#'   \code{graph_df}. If \code{NULL} and \code{x} is an sf object, uses \code{st_length(x)}
#'   as the cost.
#'
#' @return If \code{x} is an sf object, returns a list with:
#'   \itemize{
#'     \item \code{network} - sf object containing only edges that were traversed
#'     \item \code{graph_df} - data.frame with graph representation of traversed edges
#'   }
#'   If \code{x} is a data.frame, returns a data.frame containing only edges that were traversed.
#'
#' @details
#' This function simplifies a network by:
#' \itemize{
#'   \item Converting the input to a graph representation (if needed)
#'   \item Validating that all origin and destination nodes exist in the network
#'   \item Computing shortest paths from each origin to all destinations using igraph
#'   \item Marking all edges that are traversed by at least one shortest path
#'   \item Returning only the subset of edges that were traversed
#' }
#'
#' The function filters the OD matrix to include only rows with finite, positive flow values.
#' All shortest paths are computed using edge costs (either from \code{cost_col} or
#' geometric length for sf objects).
#'
#' @export
#' @importFrom collapse fselect fsubset fnrow ss ckmatch
#' @importFrom igraph graph_from_data_frame delete_vertex_attr igraph_options shortest_paths
#' @importFrom sf st_length
#' @useDynLib mmflowr, .registration = TRUE
simplify_network <- function(x, od_matrix_long, cost_col = NULL) {

  if(inherits(x, "sf")) {
    graph_df <- linestring_to_graph(x)
    if(is.null(cost_col)) graph_df$cost <- st_length(x)
  } else if (all(c("from", "to") %in% names(x))) graph_df <- x
  else stop("x must be a linestring sf object or a graph data.frame with 'from' and 'to' columns")

  g <- graph_df |> fselect(from, to) |>
    graph_from_data_frame(directed = FALSE) |>
    delete_vertex_attr("name")
  iopt <- igraph_options(return.vs.es = FALSE) # sparsematrices = TRUE
  on.exit(igraph_options(iopt))

  if(!all(c("from", "to", "flow") %in% names(od_matrix_long))) stop("od_matrix_long needs to have columns 'from', 'to' and 'flow'")
  od_matrix_long %<>% fsubset(is.finite(flow) & flow > 0)
  from <- od_matrix_long$from
  to <- od_matrix_long$to

  nodes_df <- nodes_from_graph(graph_df)
  ckmatch(from, nodes_df$node, e = "Unknown origin nodes:")
  ckmatch(to, nodes_df$node, e = "Unknown destination nodes:")

  edges_traversed <- integer(fnrow(graph_df))
  for (i in from) {
    pathsi <- shortest_paths(g, from = i, to = to, weights = cost, output = "epath")$epath
    .Call(C_mark_edges_traversed, pathsi, edges_traversed)
  }

  if(inherits(x, "sf")) {
    return(list(network = x[edges_traversed > 0, ],
                graph_df = graph_df |> ss(edges_traversed > 0)))
  }
  graph_df |> ss(edges_traversed > 0)
}




