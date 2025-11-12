
#' @title Convert Linestring to Graph
#'
#' @param lines An sf data frame of LINESTRING geometries.
#' @param digits Numeric rounding applied to coordinates (to ensure that matching points across different linestrings is not impaired by numeric precision issues). Set to \code{NA/Inf/FALSE} to disable.
#' @param keep.cols Character vector of column names to keep from the input data frame.
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
#'
#' @seealso \link{simplify_network} \link{flowr-package}
#'
#' @export
#' @importFrom sf st_geometry_type st_coordinates
#' @importFrom collapse qDF GRP get_vars get_vars<- add_vars add_vars<- fselect ffirst flast add_stub fmutate group fmatch %+=% fmax colorder whichNA setv unattrib
linestrings_to_graph <- function(lines, digits = 6, keep.cols = NULL) {
  gt <- st_geometry_type(lines, by_geometry = FALSE)
  if(length(gt) != 1L || gt != "LINESTRING") stop("lines needs to be a sf data frame of LINESTRING's")
  graph <- st_coordinates(lines) |> qDF()
  g <- GRP(list(line = graph$L1), return.order = FALSE)
  graph <- add_vars(fselect(graph, X, Y) |> ffirst(g, na.rm = FALSE, use.g.names = FALSE) |> add_stub("F"),
                    fselect(graph, X, Y) |> flast(g, na.rm = FALSE, use.g.names = FALSE) |> add_stub("T")) |>
           add_vars(g$groups, pos = "front")
  if(is.numeric(digits) && is.finite(digits)) {
    coords <- c("FX", "FY", "TX", "TY")
    get_vars(graph, coords) <- get_vars(graph, coords) |>
      lapply(round, digits = digits)
  }
  graph <- graph |>
    fmutate(from = unattrib(group(FX, FY)),
            to = from[fmatch(list(TX, TY), list(FX, FY))]) |>
    colorder(line, from, FX, FY, to, TX, TY)
  if(anyNA(graph$to)) {
    miss <- whichNA(graph$to)
    setv(graph$to, miss, group(ss(graph, miss, c("TX", "TY"), check = FALSE)) %+=% fmax(graph$from), vind1 = TRUE)
  }
  if(!is.null(keep.cols)) add_vars(graph) <- get_vars(unclass(lines), keep.cols)
  graph
}


#' @title Convert Graph to Linestrings
#' @description Convert a graph data frame with node coordinates to an sf object with LINESTRING geometries.
#'
#' @param graph_df A data frame representing a graph with columns:
#'   \code{FX}, \code{FY}, \code{TX}, \code{TY} (starting and ending node coordinates),
#'   and optionally other columns to preserve.
#' @param crs Numeric or character (default: 4326). Coordinate reference system
#'   to assign to the output sf object.
#'
#' @return An sf data frame with LINESTRING geometry, containing all columns from
#'   \code{graph_df} except \code{FX}, \code{FY}, \code{TX}, and \code{TY}. Each row
#'   represents an edge as a LINESTRING connecting the from-node (\code{FX}, \code{FY})
#'   to the to-node (\code{TX}, \code{TY}).
#'
#' @details
#' This function is the inverse operation of \code{\link{linestrings_to_graph}}. It:
#' \itemize{
#'   \item Creates LINESTRING geometries from node coordinates (\code{FX}, \code{FY}, \code{TX}, \code{TY})
#'   \item Removes the coordinate columns from the output
#'   \item Preserves all other columns from the input graph data frame
#'   \item Returns an sf object suitable for spatial operations and visualization
#' }
#'
#' @seealso \link{linestrings_to_graph} \link{flowr-package}
#'
#' @export
#' @importFrom sf st_linestring st_as_sfc st_as_sf
#' @importFrom collapse seq_row fselect add_vars
linestrings_from_graph <- function(graph_df, crs = 4326) {
  if(!is.data.frame(graph_df)) stop("graph_df needs to be a data frame")
  if(inherits(graph_df, "sf")) stop("graph_df should not be a spatial object/data frame")
  if(!all(c("FX", "FY", "TX", "TY") %in% names(graph_df))) stop("graph_df needs to have columns FX, FY, TX and TY")
  # Create Geometries
  lines_list <- with(graph_df, lapply(seq_row(graph_df), function(i) {
    matrix(c(FX[i], FY[i], TX[i], TY[i]), ncol = 2, byrow = TRUE) |>
    st_linestring()
  })) |> st_sfc(crs = crs)
  # Create sf data frame with all columns
  graph_df |>
    fselect(-FX, -FY, -TX, -TY) |>
    add_vars(list(geometry = lines_list)) |>
    st_sf(sf_column_name = "geometry", crs = crs)
}

#' @title Create Undirected Graph
#' @description Convert a directed graph to an undirected graph by normalizing edges and aggregating duplicate connections.
#'
#' @param graph_df A data frame representing a directed graph with columns:
#'   \code{from}, \code{to}, \code{line}, \code{FX}, \code{FY}, \code{TX}, \code{TY},
#'   and any columns specified in \code{cols.aggregate}.
#' @param cols.aggregate Character vector (default: "cost"). Column names to aggregate
#'   when collapsing duplicate edges.
#' @param fun.aggregate Function (default: \code{fmean}). Aggregation function to apply
#'   to columns specified in \code{cols.aggregate}. Must be a collapse package function
#'   (e.g., \code{fmean}, \code{fsum}, \code{fmin}, \code{fmax}).
#' @param \dots Further arguments to pass to \code{fun.aggregate}.
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
#'     \item Aggregated columns as specified in \code{cols.aggregate}
#'   }
#'
#' @details
#' This function converts a directed graph to an undirected graph by:
#' \itemize{
#'   \item Normalizing edge directions so that \code{from < to} for all edges
#'   \item Collapsing duplicate edges (same \code{from} and \code{to} nodes)
#'   \item For spatial/identifier columns (\code{line}, \code{FX}, \code{FY}, \code{TX}, \code{TY}),
#'     taking the first value from duplicates
#'   \item For aggregation columns (specified in \code{cols.aggregate}), applying the
#'     specified aggregation function (e.g., mean, sum, min, max)
#' }
#'
#' @export
#' @importFrom collapse ftransform GRP BY get_vars add_vars ffirst flast.default fmean colorderv %!in% .FAST_STAT_FUN
create_undirected_graph <- function(graph_df, cols.aggregate = "cost", fun.aggregate = fmean, ...) {
  graph_df <- ftransform(graph_df, from = pmin(from, to), to = pmax(from, to))
  g <- GRP(graph_df, ~ from + to, sort = FALSE)
  if(flast.default(as.character(substitute(fun.aggregate))) %!in% .FAST_STAT_FUN) {
    FUN <- match.fun(fun.aggregate)
    fun.aggregate <- function(x, g, ...) BY(x, g, FUN, ...)
  }
  res <- add_vars(g$groups,
    ffirst(get_vars(graph_df, c("line", "FX", "FY", "TX", "TY")), g, use.g.names = FALSE),
    fun.aggregate(get_vars(graph_df, cols.aggregate), g, use.g.names = FALSE, ...)) |>
    colorderv(c("line", "from", "FX", "FY", "to", "TX", "TY", cols.aggregate))
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
#' @importFrom stats setNames
nodes_from_graph <- function(graph_df) {
  rowbind(graph_df |> fselect(from, FX, FY),
          graph_df |> fselect(to, TX, TY), use.names = FALSE) |>
    setNames(c("node", "X", "Y")) |>
    funique(cols = "node", sort = TRUE)
}

#' @title Compute Distance Matrix from Graph
#' @description Compute a distance matrix for all node pairs in a graph using cppRouting.
#'
#' @param graph_df A data frame representing a graph with columns:
#'   \code{from}, \code{to}, and \code{cost}.
#' @param directed Logical (default: FALSE). If TRUE, treats the graph as directed;
#'   if FALSE, treats it as undirected.
#' @param cost.column Character string (optional). Name of the cost column in \code{graph_df}.
#'   Alternatively, a numeric vector of edge costs with length equal to \code{nrow(graph_df)}.
#' @param \dots Additional arguments passed to \code{\link[igraph]{distances}()}.
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
#' @importFrom collapse fselect fnrow funique.default
#' @importFrom igraph graph_from_data_frame distances
dist_mat_from_graph <- function(graph_df, directed = FALSE, cost.column = "cost", ...) {
  cost <- if(is.character(cost.column) && length(cost.column) == 1L) graph_df[[cost.column]] else
    if(is.numeric(cost.column) && length(cost.column) == fnrow(graph_df)) cost.column else
    stop("cost.column needs to be a column name in graph_df or a numeric vector matching nrow(graph_df)")

  # Create Igraph Graph
  vertices <- data.frame(name = funique.default(c(graph_df$from, graph_df$to), sort = TRUE))
  g <- graph_df |> fselect(from, to) |>
    graph_from_data_frame(directed = directed, vertices = vertices)

  distances(g, mode = "out", weights = cost, ...)
  # graph <- makegraph(graph_df |> fselect(from, to, cost), directed = directed) # directed = FALSE # cpp_simplify()
  # nodes <- graph$dict$ref
  # get_distance_matrix(cpp_contract(graph), from = nodes, to = nodes, algorithm = algorithm, ...)
}


#' @title Simplify Network
#' @description Simplify a network by keeping only edges that are traversed by shortest paths
#'   between origin-destination pairs in the OD matrix.
#'
#' @param x Either an sf object with LINESTRING geometry representing the network, or a
#'   data.frame with columns \code{from} and \code{to} representing the graph edges.
#' @param od_matrix_long A data.frame representing the origin-destination matrix in long format.
#' If \code{x} is a LINETRING geometry, it should have columns \code{FX}, \code{FY}, \code{TX}, \code{TY}
#' representing the origin/destination zone centroids. If \code{x} is a graph data frame, it should have columns
#' \code{from} and \code{to} matching nodes in \code{x}.
#' @param cost.column Character string (optional). Name of the cost column in \code{x}.
#' If \code{NULL} and \code{x} is an sf object, uses \code{st_length(x)} as the cost.
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
#' All shortest paths are computed using edge costs (either from \code{cost.column} or
#' geometric length for sf objects).
#'
#' @export
#' @importFrom collapse fselect fsubset fnrow ss ckmatch
#' @importFrom igraph graph_from_data_frame delete_vertex_attr igraph_options shortest_paths
#' @importFrom sf st_length
#' @useDynLib flowr, .registration = TRUE
simplify_network <- function(x, od_matrix_long, cost.column = NULL) {

  if(inherits(x, "sf")) {
    graph_df <- linestrings_to_graph(x)
    if(is.null(cost.column)) graph_df$cost <- st_length(x)
    names(od_matrix_long) <- tolower(names(od_matrix_long))
    if(!all(c("fx", "fy", "tx", "ty") %in% names(od_matrix_long)))
      stop("od_matrix_long needs to have columns 'FX', 'FY', 'TX' and 'TY' when x is a linestring sf object")
  } else if (all(c("from", "to") %in% names(x))) {
    graph_df <- x
    if(!all(c("from", "to") %in% names(od_matrix_long))) stop("od_matrix_long needs to have columns 'from' and 'to'")

  } else stop("x must be a linestring sf object or a graph data.frame with 'from' and 'to' columns")

  g <- graph_df |> fselect(from, to) |>
    graph_from_data_frame(directed = FALSE) |>
    delete_vertex_attr("name")
  iopt <- igraph_options(return.vs.es = FALSE) # sparsematrices = TRUE
  on.exit(igraph_options(iopt))

  if(length(od_matrix_long[["flow"]])) od_matrix_long <- fsubset(od_matrix_long, is.finite(flow) & flow > 0)
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




