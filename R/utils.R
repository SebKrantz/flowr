
utils::globalVariables(c(
  "from", "to", "line", "FX", "FY", "TX", "TY", "X", "Y", "cost", "flow"
  # Add any other variable names that appear in the notes
  # "." # Often needed if you use the data.table or magrittr pipe syntax
))

#' @title Convert Linestring to Graph
#'
#' @param lines An sf data frame of LINESTRING geometries.
#' @param digits Numeric rounding applied to coordinates (to ensure that matching points across different linestrings is not impaired by numeric precision issues). Set to \code{NA/Inf/FALSE} to disable.
#' @param keep.cols Character vector of column names to keep from the input data frame.
#' @param compute.length Applies \code{st_length()} to and saves it as an additional column named \code{".length"}.
#'
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
#' @importFrom sf st_geometry_type st_coordinates st_length
#' @importFrom collapse qDF GRP get_vars get_vars<- add_vars add_vars<- fselect ffirst flast add_stub fmutate group fmatch %+=% fmax colorder whichNA setv unattrib
linestrings_to_graph <- function(lines, digits = 6, keep.cols = is.atomic, compute.length = TRUE) {
  gt <- st_geometry_type(lines, by_geometry = FALSE)
  if(length(gt) != 1L || gt != "LINESTRING") stop("lines needs to be a sf data frame of LINESTRING's")
  graph <- st_coordinates(lines) |> qDF()
  g <- GRP(list(line = graph$L1), return.order = FALSE)
  graph <- add_vars(fselect(graph, X, Y) |> ffirst(g, na.rm = FALSE, use.g.names = FALSE) |> add_stub("F"),
                    fselect(graph, X, Y) |> flast(g, na.rm = FALSE, use.g.names = FALSE) |> add_stub("T")) |>
           add_vars(g$groups, pos = "front")
  if(compute.length) add_vars(graph) <- list(.length = st_length(lines))
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
#' @importFrom sf st_linestring st_sfc st_sf
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
#' @param sf Logical. If TRUE, returns result as an \code{sf} POINT object. Default: FALSE.
#' @param crs Coordinate reference system for sf output; default is 4326.
#'
#' @return A data frame (or sf object if \code{sf = TRUE}) with unique nodes and coordinates:
#'   \itemize{
#'     \item \code{node} - Node ID
#'     \item \code{X} - Node X-coordinate (typically longitude)
#'     \item \code{Y} - Node Y-coordinate (typically latitude)
#'   }
#'   Result is sorted by node ID.
#'
#' @details
#' This function extracts all unique nodes from both the \code{from} and \code{to}
#' columns of the graph, along with their corresponding coordinates. Duplicate nodes
#' are removed, keeping only unique node IDs with their coordinates.
#'
#' @export
#' @importFrom collapse rowbind fselect funique
#' @importFrom stats setNames
#' @importFrom sf st_as_sf
nodes_from_graph <- function(graph_df, sf = FALSE, crs = 4326) {
  nodes <- rowbind(graph_df |> fselect(from, FX, FY),
                   graph_df |> fselect(to, TX, TY), use.names = FALSE) |>
    setNames(c("node", "X", "Y")) |>
    funique(cols = "node", sort = TRUE)
  if(sf) return(st_as_sf(nodes, coords = c("X", "Y"), crs = crs))
  nodes
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


#' @title Consolidate Graph
#' @description Consolidate a graph by removing intermediate nodes (nodes that occur exactly twice) and optionally dropping loop, duplicate, and singleton edges. This simplifies the network topology while preserving connectivity.
#'
#' @param graph_df A data frame representing a graph with columns:
#'   \code{from} and \code{to} (node IDs), and optionally other columns to preserve.
#'   If coordinate columns (\code{FX}, \code{FY}, \code{TX}, \code{TY}) are present, they will be
#'   preserved and updated based on the consolidated node coordinates.
#' @param directed Logical (default: FALSE). Whether the graph is directed.
#' @param drop.edges Character vector (default: \code{c("loop", "duplicate", "single")}). Types of edges to drop:
#'   \code{"loop"} removes self-loops (edges where from == to),
#'   \code{"duplicate"} removes duplicate edges (same from-to pair),
#'   \code{"single"} removes edges leading to singleton nodes (nodes that occur only once).
#'   Set to \code{NULL} to keep all edges.
#' @param consolidate Logical (default: TRUE). If TRUE, consolidates the graph by removing
#'   intermediate nodes (nodes that occur exactly twice) and merging connecting edges.
#'   If FALSE, only drops edges as specified in \code{drop.edges}.
#' @param keep.nodes Numeric vector (optional). Node IDs to preserve during consolidation,
#'   even if they occur exactly twice. Also used to preserve nodes when dropping singleton edges.
#' @param fun.aggregate Function (default: \code{fmean}). Aggregation function to apply
#'   to columns when consolidating edges. Must be a collapse package function
#'   (e.g., \code{fmean}, \code{fsum}, \code{fmin}, \code{fmax}).
#' @param \dots Further arguments passed to \code{fun.aggregate}.
#' @param recursive Logical (default: TRUE). If TRUE, recursively consolidates the graph
#'   until no further consolidation is possible. This ensures that long chains of intermediate
#'   nodes are fully consolidated in a single call.
#' @param verbose Logical (default: TRUE). Whether to print messages about dropped edges
#'   and consolidation progress.
#'
#' @return A data frame representing the consolidated graph with:
#'   \itemize{
#'     \item \code{line} - Line identifier (added as first column)
#'     \item All columns from \code{graph_df} (aggregated if consolidation occurred),
#'       excluding \code{from}, \code{to}, and optionally \code{FX}, \code{FY}, \code{TX}, \code{TY}
#'       (which are re-added if present in original)
#'     \item \code{from}, \code{to} - Node IDs (updated after consolidation)
#'     \item Coordinate columns (\code{FX}, \code{FY}, \code{TX}, \code{TY}) if present in original
#'     \item Attribute \code{"keep.edges"} - Indices of original edges that were kept
#'     \item Attribute \code{"gid"} - Edge group IDs mapping consolidated edges to original edges
#'   }
#'
#' @details
#' This function simplifies a graph by:
#' \itemize{
#'   \item \strong{Dropping edges}: Optionally removes self-loops, duplicate edges, and edges
#'     leading to singleton nodes (nodes that appear only once in the graph)
#'   \item \strong{Consolidating nodes}: Removes intermediate nodes (nodes that occur exactly twice)
#'     by merging the two edges connected through them into a single longer edge
#'   \item \strong{Aggregating attributes}: When edges are merged, numeric attributes are aggregated
#'     using \code{fun.aggregate} (e.g., mean, sum, min, max)
#'   \item \strong{Recursive consolidation}: If \code{recursive = TRUE}, the function continues
#'     consolidating until no more nodes can be consolidated, ensuring complete simplification
#' }
#'
#' Consolidation is useful for simplifying network topology while preserving connectivity.
#' For example, if node B connects A->B and B->C, it will be removed and replaced with A->C.
#' With \code{recursive = TRUE}, long chains (A->B->C->D) are fully consolidated to A->D in
#' a single call.
#'
#' For undirected graphs, the algorithm also handles cases where a node appears twice
#' as either origin or destination (circular cases).
#'
#' If coordinate columns (\code{FX}, \code{FY}, \code{TX}, \code{TY}) are present in the input,
#' they are preserved and updated based on the consolidated node coordinates from the original graph.
#'
#' @seealso \link{create_undirected_graph} \link{simplify_network} \link{flowr-package}
#'
#' @export
#' @importFrom collapse na_rm alloc fnrow get_vars anyv setv ss seq_row fcountv fduplicated fmatch whichv whichNA allNA ffirst group fmin fmax GRP fsubset collap %!in% %!iin% join colorder GRPN qtab funique.default
#' @importFrom stats setNames
consolidate_graph <- function(graph_df, directed = FALSE,
                              drop.edges = c("loop", "duplicate", "single"),
                              consolidate = TRUE, keep.nodes = NULL,
                              fun.aggregate = fmean, ...,
                              recursive = TRUE,
                              verbose = TRUE) {

  .keep <- seq_row(graph_df)
  gft <- get_vars(graph_df, c("from", "to")) |> unclass()

  if(anyv(drop.edges, "loop") && any(loop <- gft$from == gft$to)) {
    .keep <- .keep[!loop]
    gft <- ss(gft, .keep, check = FALSE)
    if(verbose) cat(sprintf("Dropped %d loop edges\n", sum(loop)))
  }

  if(anyv(drop.edges, "duplicate") && any(dup <- fduplicated(gft))) {
    .keep <- .keep[!dup]
    gft <- ss(gft, !dup)
    if(verbose) cat(sprintf("Dropped %d duplicate edges\n", sum(dup)))
  }

  if(anyv(drop.edges, "single") && fnrow(gft)) {
    degree_table <- compute_degrees(gft$from, gft$to)
    if(fnrow(degree_table)) {
      nodes_rm <- degree_table$node[degree_table$deg_total == 1L]
      if(length(keep.nodes)) nodes_rm <- nodes_rm[!nodes_rm %in% keep.nodes]
      if(length(nodes_rm)) {
        ind <- which(gft$from %!in% nodes_rm & gft$to %!in% nodes_rm)
        if(verbose) {
          dropped <- fnrow(gft) - length(ind)
          if(dropped) cat(sprintf("Dropped %d edges leading to singleton nodes\n", dropped))
        }
        .keep <- .keep[ind]
        gft <- ss(gft, ind, check = FALSE)
      }
    }
  }

  if(!consolidate) {
    gdf <- ss(graph_df, .keep, check = FALSE)
    attr(gdf, "keep.edges") <- .keep
    return(gdf)
  }
  # TODO: How does not dropping loop or duplicate edges affect the algorithm?

  # Get unique ID for each edge
  gid <- seq_row(gft)

  merge_linear_nodes <- function(nodes_rm) {
    if(!length(nodes_rm)) return(FALSE)
    from_ind <- fmatch(nodes_rm, gft$from)
    to_ind <- fmatch(nodes_rm, gft$to)
    valid <- !is.na(from_ind) & !is.na(to_ind)
    if(!all(valid)) {
      nodes_rm <- nodes_rm[valid]
      from_ind <- from_ind[valid]
      to_ind <- to_ind[valid]
    }
    if(!length(nodes_rm)) return(FALSE)
    repeat {
      gid[from_ind] <<- gid[to_ind]
      gft$to[to_ind] <<- gft$to[from_ind]
      gft$from[from_ind] <<- NA
      to_next <- fmatch(nodes_rm, gft$to)
      if(allNA(to_next)) break
      valid_next <- !is.na(to_next)
      if(!any(valid_next)) break
      from_ind <- from_ind[valid_next]
      nodes_rm <- nodes_rm[valid_next]
      to_ind <- to_next[valid_next]
    }
    ffirst(gft$from, gid, "fill", set = TRUE)
    TRUE
  }

  orient_undirected_nodes <- function(nodes) {
    if(!length(nodes)) return(integer())
    updated <- integer()
    for(node in nodes) {
      idx_from <- whichv(gft$from, node)
      idx_to <- whichv(gft$to, node)
      occ_total <- length(idx_from) + length(idx_to)
      if(occ_total != 2L) next
      if(length(idx_from) == 2L) {
        swap_idx <- idx_from[2L]
        tmp <- gft$from[swap_idx]
        gft$from[swap_idx] <<- gft$to[swap_idx]
        gft$to[swap_idx] <<- tmp
        updated <- c(updated, node)
      } else if(length(idx_to) == 2L) {
        swap_idx <- idx_to[2L]
        tmp <- gft$to[swap_idx]
        gft$to[swap_idx] <<- gft$from[swap_idx]
        gft$from[swap_idx] <<- tmp
        updated <- c(updated, node)
      }
    }
    funique.default(updated)
  }

  consolidated_any <- FALSE
  repeat {
    degree_table <- compute_degrees(gft$from, gft$to)
    if(!fnrow(degree_table)) break

    if(anyv(drop.edges, "single") && fnrow(gft)) {
      nodes_single <- degree_table$node[degree_table$deg_total == 1L]
      if(length(keep.nodes)) nodes_single <- nodes_single[nodes_single %!iin% keep.nodes]
      if(length(nodes_single)) {
        ind <- which(gft$from %!in% nodes_single & gft$to %!in% nodes_single)
        dropped <- fnrow(gft) - length(ind)
        if(dropped > 0L) {
          if(verbose) cat(sprintf("Dropped %d edges leading to singleton nodes after consolidation\n", dropped))
          .keep <- .keep[ind]
          gid <- gid[ind]
          gft <- ss(gft, ind, check = FALSE)
          next
        }
      }
    }

    if(!directed) {
      nodes_deg2 <- degree_table$node[degree_table$deg_total == 2L]
      if(length(keep.nodes)) nodes_deg2 <- nodes_deg2[nodes_deg2 %!iin% keep.nodes]
      if(length(nodes_deg2)) {
        idx_deg2 <- fmatch(nodes_deg2, degree_table$node)
        needs_orientation <- nodes_deg2[degree_table$deg_from[idx_deg2] == 2L | degree_table$deg_to[idx_deg2] == 2L]
        if(length(needs_orientation)) {
          oriented_nodes <- orient_undirected_nodes(needs_orientation)
          if(!length(oriented_nodes)) stop("Failed to orient undirected nodes for consolidation; please verify the input graph.")
          degree_table <- compute_degrees(gft$from, gft$to)
          if(!fnrow(degree_table)) break
        }
        nodes_deg2_linear <- degree_table$node[degree_table$deg_total == 2L & degree_table$deg_from == 1L & degree_table$deg_to == 1L]
        if(length(keep.nodes)) nodes_deg2_linear <- nodes_deg2_linear[nodes_deg2_linear %!iin% keep.nodes]
        if(length(nodes_deg2_linear)) {
          if(!merge_linear_nodes(nodes_deg2_linear)) stop("Failed to consolidate oriented undirected nodes; please verify the graph topology.")
          consolidated_any <- TRUE
          if(verbose) cat(sprintf("Consolidated %d intermediate nodes\n", length(nodes_deg2_linear)))
          if(!recursive) break
          next
        } else if(length(needs_orientation)) next
      }
    }

    nodes_linear <- degree_table$node[degree_table$deg_from == 1L & degree_table$deg_to == 1L]
    if(length(keep.nodes)) nodes_linear <- nodes_linear[nodes_linear %!iin% keep.nodes]
    total_nodes <- length(nodes_linear)
    if(total_nodes == 0L) break
    if(!merge_linear_nodes(nodes_linear)) stop("Failed to consolidate eligible nodes; please verify the graph topology.")
    consolidated_any <- TRUE
    if(verbose) cat(sprintf("Consolidated %d intermediate nodes\n", total_nodes))
    if(!recursive) break
  }

  if(!consolidated_any) {
    if(verbose) cat("No nodes to consolidate, returning graph\n")
    gdf <- ss(graph_df, .keep, check = FALSE)
    attr(gdf, "keep.edges") <- .keep
    return(gdf)
  }
  # These edges are now removed
  # setv(gft, from_ind, NA, vind1 = TRUE)
  # Aggregation
  if(anyv(drop.edges, "duplicate")) {
    g <- GRP(gft, sort = TRUE)
    gid <- fmatch(gid, gid[g$group.starts], count = TRUE)
  } else {
    g <- GRP(c(gft, list(.gid = gid)), sort = TRUE)
    gid <- fmatch(gid, g$groups[[3]], count = TRUE)
    # g$groups[[3]] <- group(g$groups[[3]])
    g$groups <- g$groups[1:2]
    g$group.vars <- g$group.vars[1:2]
  }
  nam_rm <- c("from", "to", "FX", "FY", "TX", "TY", "line")
  nam <- names(graph_df)
  gdf <- ss(graph_df, .keep, nam[nam %!iin% nam_rm])
  if(verbose) cat("Aggregating down from", fnrow(gdf), "edges to", g$N.groups, "edges\n")
  gdf <- eval(substitute(collap(gdf, g, fun.aggregate, ...)))

  if(anyv(drop.edges, "single") && fnrow(gdf)) {
    gft_final <- get_vars(gdf, c("from", "to")) |> unclass()
    degree_table_final <- compute_degrees(gft_final$from, gft_final$to)
    if(fnrow(degree_table_final)) {
      nodes_single_final <- degree_table_final$node[degree_table_final$deg_total == 1L]
      if(length(keep.nodes)) nodes_single_final <- nodes_single_final[!nodes_single_final %in% keep.nodes]
      if(length(nodes_single_final)) {
        ind_final <- which(gft_final$from %!in% nodes_single_final & gft_final$to %!in% nodes_single_final)
        dropped_final <- fnrow(gdf) - length(ind_final)
        if(dropped_final > 0L) {
          if(verbose) cat(sprintf("Dropped %d edges leading to singleton nodes after aggregation\n", dropped_final))
          gdf <- ss(gdf, ind_final, check = FALSE)
          gid <- gid[ind_final]
        }
      }
    }
  }

  if(recursive && fnrow(gdf)) {
    prev_fnrow <- fnrow(gdf) + 1L
    while(prev_fnrow > (nrow_gdf <- fnrow(gdf))) {
      prev_fnrow <- nrow_gdf
      gdf <- consolidate_graph(gdf, directed = directed,
                               drop.edges = drop.edges,
                               consolidate = TRUE,
                               keep.nodes = keep.nodes,
                               fun.aggregate = fun.aggregate, ...,
                               recursive = FALSE,
                               verbose = verbose)
    }
  }

  if(verbose) {
    cat("Node Counts Table:\n")
    print(qtab(GRPN(c(gdf$from, gdf$to)), dnn = NULL))
  }

  if(any(nam_rm[3:6] %in% nam)) {
    if(verbose) cat("Joining node coordinates back to consolidated graph\n")
    nodes <- nodes_from_graph(graph_df, sf = FALSE)
    if(any(nam_rm[3:4] %in% nam)) gdf <- join(gdf, setNames(nodes, c("from", "FX", "FY")), on = "from", verbose = 0L) |> colorder(from, FX, FY)
    if(any(nam_rm[5:6] %in% nam)) gdf <- join(gdf, setNames(nodes, c("to", "TX", "TY")), on = "to", verbose = 0L) |> colorder(to, TX, TY, pos = "after")
  }

  add_vars(gdf, pos = "front") <- list(line = seq_row(gdf))
  attr(gdf, "keep.edges") <- .keep
  attr(gdf, "gid") <- gid
  gdf
}

# Helper for consolidate_graph()
compute_degrees <- function(from_vec, to_vec) {
  nodes <- na_rm(funique.default(c(from_vec, to_vec)))
  deg_from <- integer(length(nodes))
  deg_to <- integer(length(nodes))
  if(length(nodes)) {
    if(length(from_vec)) {
      counts_from <- unclass(fcountv(from_vec))
      idx_from <- fmatch(nodes, counts_from[[1L]], nomatch = 0L)
      has_from <- idx_from > 0L
      if(any(has_from)) deg_from[has_from] <- counts_from$N[idx_from[has_from]]
    }
    if(length(to_vec)) {
      counts_to <- unclass(fcountv(to_vec))
      idx_to <- fmatch(nodes, counts_to[[1L]], nomatch = 0L)
      has_to <- idx_to > 0L
      if(any(has_to)) deg_to[has_to] <- counts_to$N[idx_to[has_to]]
    }
  }
  list(
    node = nodes,
    deg_from = deg_from,
    deg_to = deg_to,
    deg_total = deg_from + deg_to
  )
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
#' @param return description
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
simplify_network <- function(x, od_matrix_long, cost.column = NULL,
                             return = c("edges", "edge_counts", "graph_df")) {

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
  edges <- which(edges_traversed > 0L)

  res <- list()
  if(anyv(return, "edges")) res$edges <- edges
  if(anyv(return, "edge_counts")) res$edge_counts <- edges_traversed
  if(anyv(return, "graph_df")) res$graph_df <- ss(graph_df, edges, check = FALSE)
  # if(inherits(x, "sf")) {
  #   return(list(network = x[edges_traversed > 0, ],
  #               graph_df = graph_df |> ss(edges_traversed > 0)))
  # }
  res
}




