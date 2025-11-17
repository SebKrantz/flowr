
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
#' @importFrom collapse qDF GRP get_vars get_vars<- add_vars add_vars<- fselect ffirst flast add_stub fmutate group fmatch %+=% fmax colorder whichNA setv unattrib ss
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
#' @param graph_df A data frame representing a directed graph including columns:
#'   \code{from}, \code{to}, and (optionally) \code{line}, \code{FX}, \code{FY}, \code{TX}, \code{TY}.
#' @param \dots Arguments passed to \code{\link[collapse]{collap}()} for aggregation across duplicated (diretional) edges. The defaults are \code{FUN = fmean} for numeric columns and \code{catFUN = fmode} for categorical columns. Select columns using \code{cols} or use argument \code{custom = list(fmean = cols1, fsum = cols2, fmode = cols3)} to map different columns to specific aggregation functions. You can weight the aggregation (using \code{w = ~ weight_col}).
#'
#' @return A data frame representing an undirected graph with:
#'   \itemize{
#'     \item \code{line} - Line identifier (first value from duplicates)
#'     \item \code{from} - Starting node ID (normalized to be < \code{to})
#'     \item \code{to} - Ending node ID (normalized to be > \code{from})
#'     \item \code{FX} - Starting node X-coordinate (first value from duplicates)
#'     \item \code{FY} - Starting node Y-coordinate (first value from duplicates)
#'     \item \code{TX} - Ending node X-coordinate (first value from duplicates)
#'     \item \code{TY} - Ending node Y-coordinate (first value from duplicates)
#'     \item Aggregated columns
#'   }
#'
#' @details
#' This function converts a directed graph to an undirected graph by:
#' \itemize{
#'   \item Normalizing edge directions so that \code{from < to} for all edges
#'   \item Collapsing duplicate edges (same \code{from} and \code{to} nodes)
#'   \item For spatial/identifier columns (\code{line}, \code{FX}, \code{FY}, \code{TX}, \code{TY}),
#'     taking the first value from duplicates
#'   \item For aggregation columns, \code{\link[collapse]{collap}()} will be applied.
#' }
#'
#' @export
#' @importFrom collapse ftransform GRP get_vars add_vars add_vars<- ffirst colorderv %!in% collap
create_undirected_graph <- function(graph_df, ...) {
  graph_df <- ftransform(graph_df, from = pmin(from, to), to = pmax(from, to))
  g <- GRP(graph_df, ~ from + to, sort = FALSE)
  nam <- names(graph_df)
  agg_first <- c("line", "FX", "FY", "TX", "TY")
  ord <- c("line", "from", "FX", "FY", "to", "TX", "TY")
  res <- g$groups
  if(any(nam %in% agg_first)) {
    add_vars(res) <- ffirst(get_vars(graph_df, nam[nam %in% agg_first]), g, use.g.names = FALSE)
    res <- colorderv(res, ord[ord %in% nam])
  }
  if(any(nam %!in% ord)) {
    add_vars(res) <- collap(get_vars(graph_df, nam[nam %!in% ord]), g, keep.by = FALSE, ...)
  }
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
#' @param \dots Additional arguments passed to \code{\link[igraph]{distances}()}, such as \code{v} (from) and \code{to} to compute paths between specific nodes.
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

#' @title Normalize Graph Node IDs
#' @description Normalize node IDs in a graph to be consecutive integers starting from 1.
#'   This is useful for ensuring compatibility with graph algorithms that require sequential node IDs.
#'
#' @param graph_df A data frame representing a graph with columns:
#'   \code{from} and \code{to} (node IDs).
#'
#' @return A data frame with the same structure as \code{graph_df}, but with \code{from}
#'   and \code{to} columns remapped to consecutive integer IDs starting from 1.
#'   All other columns are preserved unchanged.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Extracts all unique node IDs from both \code{from} and \code{to} columns
#'   \item Sorts them in ascending order
#'   \item Remaps the original node IDs to sequential integers (1, 2, 3, ...)
#'   \item Updates both \code{from} and \code{to} columns with the normalized IDs
#' }
#'
#' Normalization is useful when:
#' \itemize{
#'   \item Node IDs are non-consecutive (e.g., 1, 5, 10, 20)
#'   \item Node IDs are non-numeric or contain gaps
#'   \item Graph algorithms require sequential integer node IDs starting from 1
#' }
#'
#' Note: This function only normalizes the node IDs; it does not modify the graph structure
#' or any other attributes. The mapping preserves the relative ordering of nodes.
#'
#' @seealso \link{nodes_from_graph} \link{flowr-package}
#'
#' @export
#' @importFrom collapse funique.default get_vars get_vars<- fmatch
normalize_graph <- function(graph_df) {
  id_cols <- c("from", "to")
  if(!all(id_cols %in% names(graph_df))) stop("graph_df must have columns 'from' and 'to'")
  nodes <- funique.default(c(graph_df$from, graph_df$to), sort = TRUE)
  get_vars(graph_df, id_cols) <- lapply(get_vars(graph_df, id_cols), fmatch, nodes)
  graph_df
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
#' @param \dots Arguments passed to \code{\link[collapse]{collap}()} for aggregation across consolidated edges. The defaults are \code{FUN = fmean} for numeric columns and \code{catFUN = fmode} for categorical columns. Select columns using \code{cols} or use argument \code{custom = list(fmean = cols1, fsum = cols2, fmode = cols3)} to map different columns to specific aggregation functions. It is highly recommended to weight the aggregation (using \code{w = ~ weight_col}) by the length/cost of the edges.
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
#'   \item \strong{Aggregating attributes}: When edges are merged, attributes/columns are aggregated
#'     using \code{\link[collapse]{collap}()}. The default aggregation is mean for numeric columns and mode for categorical columns.
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
#' @importFrom collapse fnrow get_vars anyv setv ss seq_row fcountv fduplicated fmatch whichv whichNA allNA ffirst GRP collap %!in% %!iin% join colorder funique.default %!=% %==% missing_cases
#' @importFrom stats setNames
consolidate_graph <- function(graph_df, directed = FALSE,
                              drop.edges = c("loop", "duplicate", "single"),
                              consolidate = TRUE, keep.nodes = NULL, ...,
                              recursive = TRUE,
                              verbose = TRUE) {

  if(length(attr(graph_df, "group.starts"))) attr(graph_df, "group.starts") <- NULL
  keep <- seq_row(graph_df) # Global variable tracking utilized edges
  gft <- get_vars(graph_df, c("from", "to")) |> unclass() # Local variable representing the current graph worked on

  if(anyv(drop.edges, "loop") && any(loop <- gft$from == gft$to)) {
    keep <- keep[!loop]
    gft <- ss(gft, keep, check = FALSE)
    if(verbose) cat(sprintf("Dropped %d loop edges\n", sum(loop)))
  }

  if(anyv(drop.edges, "duplicate") && any(dup <- fduplicated(gft))) {
    keep <- keep[!dup]
    gft <- ss(gft, !dup)
    if(verbose) cat(sprintf("Dropped %d duplicate edges\n", sum(dup)))
  }

  if(anyv(drop.edges, "single") && fnrow(gft)) {
    nodes_rm <- unclass(fcountv(do.call(c, gft)))
    if(anyv(nodes_rm$N, 1L)) {
      nodes_rm <- nodes_rm[[1L]][nodes_rm$N == 1L]
      if(length(keep.nodes)) nodes_rm <- nodes_rm[nodes_rm %!iin% keep.nodes]
      if(length(nodes_rm)) {
        ind <- which(gft$from %!in% nodes_rm & gft$to %!in% nodes_rm)
        if(verbose) cat(sprintf("Dropped %d edges leading to singleton nodes\n", fnrow(gft) - length(ind)))
        keep <- keep[ind]
        gft <- ss(gft, ind, check = FALSE)
      }
    }
  }

  if(!consolidate) {
    res <- ss(graph_df, keep, check = FALSE)
    attr(res, "keep.edges") <- keep
    return(res)
  }
  # TODO: How does not dropping loop or duplicate edges affect the algorithm?

  # Early return if no nodes to consolidate
  if(length(nodes_rm) == 0L) {
    if(verbose) cat("No nodes to consolidate, returning graph\n")
    res <- ss(graph_df, keep, check = FALSE)
    attr(res, "keep.edges") <- keep
    return(res)
  }

  consolidate_graph_core <- function(gdf, gft) {

    if(fnrow(gdf) != fnrow(gft)) stop("Internal length mismatch! Please report this as a bug!")
    keepl <- gid <- seq_row(gft)  # Local variable mapping current edges to groups
    consolidated_any <- FALSE

    merge_linear_nodes <- function(nodes) {
      if(!length(nodes)) return(FALSE)
      from_ind <- fmatch(nodes, gft$from)
      to_ind <- fmatch(nodes, gft$to)
      if(anyNA(from_ind) || anyNA(to_ind)) {
        valid <- whichv(missing_cases(list(from_ind, to_ind)), FALSE)
        if(!length(valid)) return(FALSE)
        from_ind <- from_ind[valid]
        to_ind <- to_ind[valid]
        nodes <- nodes[valid]
      }
      gft$from[from_ind] <<- NA
      repeat {
        gid[from_ind] <<- gid[to_ind]
        gft$to[to_ind] <<- gft$to[from_ind]
        to_ind <- fmatch(nodes, gft$to)
        if(allNA(to_ind)) break
        valid <- whichNA(to_ind, invert = TRUE)
        from_ind <- from_ind[valid]
        to_ind <- to_ind[valid]
        nodes <- nodes[valid]
      }
      ffirst(gft$from, gid, "fill", set = TRUE)
      TRUE
    }

    orient_undirected_nodes <- function(nodes) {
      if(!length(nodes)) return(FALSE)
      for(node in nodes) {
        if(length(idx <- whichv(gft$from, node))) {
          idx <- idx[2L]
          tmp <- gft$from[idx]
          gft$from[idx] <<- gft$to[idx]
          gft$to[idx] <<- tmp
        } else if(length(idx <- whichv(gft$to, node))) {
          idx <- idx[2L]
          tmp <- gft$to[idx]
          gft$to[idx] <<- gft$from[idx]
          gft$from[idx] <<- tmp
        }
      }
      TRUE
    }

    repeat {

      degree_table <- compute_degrees(gft$from, gft$to)
      if(!fnrow(degree_table)) break

      if(anyv(drop.edges, "single") && anyv(degree_table$deg_total, 1L)) {
        nodes <- degree_table$node[degree_table$deg_total %==% 1L]
        if(length(keep.nodes)) nodes <- nodes[nodes %!iin% keep.nodes]
        if(length(nodes)) {
          ind <- which(gft$from %!in% nodes & gft$to %!in% nodes)
          dropped <- fnrow(gft) - length(ind)
          if(dropped > 0L) {
            if(verbose) cat(sprintf("Dropped %d edges leading to singleton nodes\n", dropped))
            gft <- ss(gft, ind, check = FALSE)
            keepl <- keepl[ind]
            gid <- gid[ind]
            keep <<- keep[ind] # keep is global
            group.id <<- group.id[ind] # group.id is global
            next
          }
        }
      }

      if(!anyv(degree_table$deg_total, 2L)) break

      if(directed) {
        nodes <- degree_table$node[degree_table$deg_from == 1L & degree_table$deg_to == 1L]
        if(length(keep.nodes)) nodes <- nodes[nodes %!iin% keep.nodes]
        if(!length(nodes)) break
      } else {
        nodes <- degree_table$node[degree_table$deg_total %==% 2L]
        if(length(keep.nodes)) nodes <- nodes[nodes %!iin% keep.nodes]
        if(!length(nodes)) break
        idx <- fmatch(nodes, degree_table$node)
        need_orientation <- nodes[degree_table$deg_from[idx] == 2L | degree_table$deg_to[idx] == 2L]
        if(length(need_orientation)) if(!orient_undirected_nodes(need_orientation)) stop("Failed to orient undirected nodes for consolidation; please verify the input graph.")
      }
      if(!merge_linear_nodes(nodes)) stop("Failed to consolidate oriented undirected nodes; please verify the graph topology.")
      consolidated_any <- TRUE
      if(verbose) cat(sprintf("Consolidated %d intermediate nodes\n", length(nodes)))
      if(!recursive) break
    }

    if(!consolidated_any) return(NULL)

    # Grouping
    if(anyv(drop.edges, "duplicate")) {
      g <- GRP(gft, sort = TRUE)
    } else {
      g <- GRP(c(gft, list(gid = gid)), sort = TRUE)
      g$groups <- g$groups[1:2]
      g$group.vars <- g$group.vars[1:2]
    }
    group.id <<- g$group.id # Global

    if(fnrow(gdf) != length(keepl)) gdf <- ss(gdf, keepl, check = FALSE)
    if(verbose) cat("Aggregating", length(keepl), "edges down to", g$N.groups, "edges\n")
    return(list(gdf = collap(gdf, g, keep.by = FALSE, ...),
                gft = g$groups))
  }

  nam <- names(graph_df)
  nam_rm <- c("from", "to", "FX", "FY", "TX", "TY", "line")
  res <- ss(graph_df, keep, nam[nam %!iin% nam_rm], check = FALSE)
  group.id <- seq_row(res) # For returning attributes if only one iteration
  res <- consolidate_graph_core(res, gft)

  if(is.null(res)) {
    if(verbose) cat("No nodes to consolidate, returning graph\n")
    res <- ss(graph_df, keep, check = FALSE)
    attr(res, "keep.edges") <- keep
    return(res)
  }

  iter <- 1L # For returning attributes if only one iteration
  if(recursive) {
    prev_fnrow <- fnrow(gft)
    while(prev_fnrow > (nrow_gdf <- fnrow(res$gdf))) {
      prev_fnrow <- nrow_gdf
      tmp <- consolidate_graph_core(res$gdf, res$gft)
      if(is.null(tmp)) break
      iter <- iter + 1L # Needs to be here
      res <- tmp
    }
  }

  res <- add_vars(res$gdf, res$gft, pos = "front")
  if(any(nam_rm[3:6] %in% nam)) {
    nodes <- nodes_from_graph(graph_df, sf = FALSE)
    if(any(nam_rm[3:4] %in% nam)) res <- join(res, setNames(nodes, c("from", "FX", "FY")), on = "from", verbose = 0L) |> colorder(from, FX, FY)
    if(any(nam_rm[5:6] %in% nam)) res <- join(res, setNames(nodes, c("to", "TX", "TY")), on = "to", verbose = 0L) |> colorder(to, TX, TY, pos = "after")
  }
  add_vars(res, pos = "front") <- list(line = seq_row(res))
  if(iter == 1L) {
    attr(res, "keep.edges") <- keep
    attr(res, "group.id") <- group.id
  }
  res
}

# Helper for consolidate_graph()
compute_degrees <- function(from_vec, to_vec) {
  nodes <- funique.default(c(from_vec, to_vec))
  deg_from <- integer(length(nodes))
  deg_to <- integer(length(nodes))
  if(length(nodes)) {
    if(length(from_vec)) {
      counts <- unclass(fcountv(from_vec))
      idx <- fmatch(nodes, counts[[1L]], nomatch = 0L)
      has <- idx %!=% 0L
      if(length(has)) setv(deg_from, has, counts$N[idx[has]], vind1 = TRUE)
    }
    if(length(to_vec)) {
      counts <- unclass(fcountv(to_vec))
      idx <- fmatch(nodes, counts[[1L]], nomatch = 0L)
      has <- idx %!=% 0L
      if(length(has)) setv(deg_to, has, counts$N[idx[has]], vind1 = TRUE)
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
#' @importFrom collapse fselect fsubset fnrow ss ckmatch anyv
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




