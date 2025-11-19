
#' @title Run Traffic Assignment
#' @description Assign traffic flows to network edges using path-sized logit (PSL) model.
#'
#' @param graph_df A data.frame with columns \code{from}, \code{to}, and optionally a cost column.
#'   Represents the network graph with edges between nodes.
#' @param od_matrix_long A data.frame with columns \code{from}, \code{to}, and \code{flow}.
#'   Represents the origin-destination matrix in long format with flow values.
#' @param directed Logical (default: FALSE). Whether the graph is directed.
#' @param cost.column Character string (default: "cost") or numeric vector. Name of the cost column
#'   in \code{graph_df}, or a numeric vector of edge costs with length equal to \code{nrow(graph_df)}.
#' @param method Character string (default: "PSL"). Assignment method. Currently only "PSL"
#'   (Path-Sized Logit) is implemented.
#' @param beta Numeric (default: -1). Path-sized logit parameter (beta_PSL).
#' @param \dots Additional arguments (currently ignored).
#' @param detour.max Numeric (default: 1.5). Maximum detour factor for alternative routes (applied to shortest paths cost). This is a key parameter controlling the execution time of the algorithm: considering more routes (higher \code{detour.max}) substantially increases computation time.
#' @param angle.max Numeric (default: 90). Maximum detour angle (in degrees, two sided). I.e., nodes not within this angle measured against a straight line from origin to destination node will not be considered for detours.
#' @param return.extra Character vector specifying additional results to return. Options include:
#'   \code{"graph"}, \code{"dmat"}, \code{"paths"} (most memory intensive), \code{"edges"}, \code{"counts"}, \code{"costs"}, and \code{"weights"}.
#'   Use \code{"all"} to return all available extra results.
#' @param verbose Show progress bar?
#'
#' @param precompute.dmat description
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{call} - The function call
#'     \item \code{final_flows} - Numeric vector of assigned flows for each edge
#'     \item Additional elements as specified in \code{return.extra}
#'   }
#'
#' @details
#' This function performs traffic assignment using a path-sized logit model:
#' \itemize{
#'   \item Creates a graph from \code{graph_df} using igraph
#'   \item Computes shortest path distance matrix for all node pairs
#'   \item For each origin-destination pair in \code{od_matrix_long}:
#'     \itemize{
#'       \item Identifies alternative routes (detours) that are within \code{detour.max} of shortest path cost
#'       \item Finds shortest paths from origin to intermediate nodes and from intermediate nodes to destination
#'       \item Filters paths to remove those with duplicate edges
#'       \item Computes path-sized logit probabilities accounting for route overlap
#'       \item Assigns flows to edges based on probabilities
#'     }
#'   \item Returns results including final flows and optionally additional information
#' }
#'
#' The path-sized logit model accounts for route overlap by adjusting probabilities based on
#' the number of alternative routes using each edge. Flows are assigned proportionally to
#' the computed probabilities.
#'
#' @seealso \link{flowr-package}
#'
#' @export
#' @importFrom collapse fselect frange funique.default ss fnrow seq_row ckmatch anyv whichv all_identical
#' @importFrom igraph graph_from_data_frame delete_vertex_attr igraph_options distances shortest_paths vcount ecount
#' @importFrom progress progress_bar
run_assignment <- function(graph_df, od_matrix_long,
                           directed = FALSE,
                           cost.column = "cost", # mode_col = NULL,
                           method = "PSL", beta = -1,
                           ...,
                           detour.max = 1.5,
                           angle.max = 90,
                           return.extra = NULL,
                           precompute.dmat = TRUE,
                           verbose = TRUE) {

  cost <- if(is.character(cost.column) && length(cost.column) == 1L) graph_df[[cost.column]] else
    if(is.numeric(cost.column) && length(cost.column) == fnrow(graph_df)) cost.column else
    stop("cost.column needs to be a column name in graph_df or a numeric vector matching nrow(graph_df)")

  # Results object
  res <- list(call = match.call())
  if(length(return.extra) == 1L && return.extra == "all")
    return.extra <- c("graph", "dmat", "paths", "edges", "counts", "costs", "weights")

  # Create Igraph Graph
  nodes <- as.integer(funique.default(c(graph_df$from, graph_df$to), sort = TRUE))
  if(nodes[1L] != 1) stop("Missing first node")
  if(diff(frange(nodes)) >= length(nodes)) stop("graph_df is missing some nodes in from/to columns")
  g <- graph_df |> fselect(from, to) |>
    graph_from_data_frame(directed = directed,
                          vertices = data.frame(name = nodes))
  if(anyv(return.extra, "graph")) res$graph <- g

  if(verbose) cat("Created graph with", vcount(g), "nodes and", ecount(g), "edges...\n")

  # Distance Matrix
  if(precompute.dmat) {
    dmat <- distances(g, mode = "out", weights = cost)
    iopt <- igraph_options(return.vs.es = FALSE) # sparsematrices = TRUE
    on.exit(igraph_options(iopt))
    if(nrow(dmat) != ncol(dmat)) stop("Distance matrix must be square")
    if(!all_identical(dimnames(dmat))) stop("Distance matrix dimensions must be equivalent")
    if(!identical(as.integer(rownames(dmat)), nodes)) stop("Distance matrix rows/columns need to match nodes in order. This is an internal bug, please report it.")
    if(anyv(return.extra, "dmat")) res$dmat <- dmat
    dimnames(dmat) <- NULL
    if(verbose) cat("Computed distance matrix of dimensions", nrow(dmat), "x", ncol(dmat), "...\n")
  } else stop("precompute.dmat = FALSE is not implemented yet")
  g <- delete_vertex_attr(g, "name")

  # Edge incidence across selected routes
  delta_ks <- integer(length(cost) + 10L)

  # Final flows vector
  final_flows <- numeric(length(cost))
  res$final_flows <- final_flows

  # Process/Check OD Matrix
  if(!all(c("from", "to", "flow") %in% names(od_matrix_long))) stop("od_matrix_long needs to have columns 'from', 'to' and 'flow'")
  od_pairs <- which(is.finite(od_matrix_long$flow) & od_matrix_long$flow > 0)
  if(length(od_pairs) != fnrow(od_matrix_long)) {
    res$od_pairs_used <- od_pairs
    od_matrix_long <- ss(od_matrix_long, od_pairs, check = FALSE)
  }
  from <- ckmatch(od_matrix_long$from, nodes, e = "Unknown origin nodes in od_matrix:")
  to <- ckmatch(od_matrix_long$to, nodes, e = "Unknown destination nodes in od_matrix:")
  flow <- od_matrix_long[["flow"]]

  # Return block
  retvals <- any(return.extra %in% c("paths", "edges", "counts", "costs", "weights"))
  if(retvals) {
    if(anyv(return.extra, "paths")) {
      pathsl <- TRUE
      paths <- vector("list", length(flow))
    } else pathsl <- FALSE
    if(anyv(return.extra, "edges")) {
      edgesl <- TRUE
      edges <- vector("list", length(flow))
    } else edgesl <- FALSE
    if(anyv(return.extra, "counts")) {
      countsl <- TRUE
      counts <- vector("list", length(flow))
    } else countsl <- FALSE
    if(anyv(return.extra, "costs")) {
      costsl <- TRUE
      costs <- vector("list", length(flow))
    } else costsl <- FALSE
    if(anyv(return.extra, "weights")) {
      weightsl <- TRUE
      weights <- vector("list", length(flow))
    } else weightsl <- FALSE
  }

  # Now iterating across OD-pairs

  if(verbose) {
    pb <- progress_bar$new(
      format = "Processed :current/:total OD-pairs (:percent) at :tick_rate/sec [Elapsed::elapsed | ETA::eta]", # [:bar] :percent eta: :eta",
      total = fnrow(od_matrix_long), clear = FALSE #, # width = 60
    )
  }

  # TODO: could restrict that other nodes must be in the direction of travel and not behind destination node
  for (i in seq_row(od_matrix_long)) {

    if(verbose) pb$tick()

    # if(precompute.dmat) {
    d_ij <- dmat[from[i], to[i]] # Shortest path cost
    d_ikj <- dmat[from[i], ] + dmat[, to[i]] # from i to all other nodes k and from these nodes k to j (basically dmat + t(dmat)?)
    # } else {
    #   d_ij <- get_distance_pair(g, from = from[i], to = to[i], weights = cost)
    #   d_ikj <- get_distance_matrix(g, from = from[i], to = dmat_rn, weights = cost) +
    #            get_distance_matrix(g, from = dmat_rn, to = to[i], weights = cost)
    # }
    short_detour_ij <- d_ikj < detour.max * d_ij
    short_detour_ij[d_ikj < d_ij + .Machine$double.eps*1e3] <- FALSE # Exclude nodes k that are on the shortest path
    # which(d_ij == d_ikj) # These are the nodes on the direct path from i to j which yield the shortest distance.
    ks <- which(short_detour_ij)
    cost_ks <- d_ikj[ks]

    # We add the shortest path at the end of paths1
    # TODO: Could still optimize calls to shortest_paths(), e.g., go to C directly.
    paths1 <- shortest_paths(g, from = from[i], to = c(ks, to[i]), weights = cost,
                             mode = "out", output = "epath", algorithm = "automatic")$epath
    paths2 <- shortest_paths(g, from = to[i], to = ks, weights = cost,
                             mode = "in", output = "epath", algorithm = "automatic")$epath
    shortest_path <- paths1[[length(paths1)]]

    # # Check
    # cost_ks[k] == sum(cost[paths1[[k]]]) + sum(cost[paths2[[k]]])

    # Get indices of paths that do not contain duplicate edges
    no_dups <- .Call(C_check_path_duplicates, paths1, paths2, delta_ks)

    # Now Path-Sized Logit: Need to compute overlap between routes
    # # Number of routes in choice set that use link j
    # for (k in no_dups) {
    #   delta_ks[paths1[[k]]] <- delta_ks[paths1[[k]]] + 1L
    #   delta_ks[paths2[[k]]] <- delta_ks[paths2[[k]]] + 1L
    # }
    # delta_ks[shortest_path] <- delta_ks[shortest_path] + 1L
    #
    # # Correction factors for each route k
    # gamma_ks <- sapply(no_dups, function(k) {
    #   path <- c(paths1[[k]], paths2[[k]])
    #   sum(cost[path] / delta_ks[path]) / cost_ks[k]
    # })
    # gamma_1 <- sum(cost[shortest_path] / delta_ks[shortest_path]) / d_ij
    #
    # # Now the PS-MNL
    # prob_ks <- proportions(exp(-c(cost_ks[no_dups], d_ij) + beta_PSL * log(c(gamma_ks, gamma_1))))
    #
    # # Need to reset delta_ks
    # delta_ks[] <- 0L
    #
    # # Assign result to edges:
    # for (k in no_dups) {
    #   final_flows[paths1[[k]]] <- final_flows[paths1[[k]]] + flow[i] * prob_ks[k]
    # }
    # final_flows[shortest_path] <- final_flows[shortest_path] + flow[i] * prob_ks[length(prob_ks)]
    wi <- .Call(C_compute_path_sized_logit, paths1, paths2, no_dups, shortest_path,
                cost, cost_ks, d_ij, beta, flow[i], delta_ks, final_flows, !retvals)

    if(retvals) {
      if(pathsl) paths[[i]] <- c(list(as.integer(shortest_path)), lapply(no_dups,
                    function(k) c(as.integer(paths1[[k]]), rev.default(as.integer(paths2[[k]])))))
      if(countsl) {
        ei <- whichv(delta_ks, 0L, invert = TRUE)
        if(edgesl) edges[[i]] <- ei
        counts[[i]] <- delta_ks[ei]
      } else if(edgesl) edges[[i]] <- whichv(delta_ks, 0L, invert = TRUE)
      if(costsl) costs[[i]] <- c(d_ij, cost_ks[no_dups])
      if(weightsl) weights[[i]] <- wi
    }
    .Call(C_free_delta_ks, delta_ks, no_dups, paths1, paths2, shortest_path)
  }

  if(verbose) pb$terminate()

  if(retvals) {
    if(pathsl) res$paths <- paths
    if(edgesl) res$edges <- edges
    if(countsl) res$edge_counts <- counts
    if(costsl) res$path_costs <- costs
    if(weightsl) res$path_weights <- weights
  }

  class(res) <- "flowr" # , method)
  return(res)
}

#' @rdname run_assignment
#'
#' @param x An object of class \code{flowr}, typically returned by \code{\link{run_assignment}}.
#'
#' @export
#' @importFrom collapse fmean fsd vlengths
print.flowr <- function(x, ...) {
  cat("Flowr object\n")
  cat("Call:", deparse(x$call), "\n\n")
  if (!is.null(x$dmat) && is.matrix(x$dmat))
    cat("Number of nodes:", nrow(x$dmat), "\n")
  cat("Number of edges:", length(x$final_flows), "\n")
  if (!is.null(x$od_pairs_used) && length(x$od_pairs_used))
    cat("Number of simulations/OD-pairs:", length(x$od_pairs_used), "\n")
  if (!is.null(x$paths) && length(x$paths)) {
    if (is.null(x$od_pairs_used) || !length(x$od_pairs_used))
      cat("Number of simulations/OD-pairs:", length(x$paths), "\n")
  }
  cat("\n")
  if (!is.null(x$paths) && length(x$paths)) {
    pls <- vlengths(x$paths)
    cat("Average number of paths per simulation (SD): ", fmean(pls), "  (", fsd(pls, stable.algo = FALSE), ")\n", sep = "")
  }
  if (!is.null(x$edges) && length(x$edges)) {
    els <- vlengths(x$edges)
    cat("Average number of edges utilized per simulation (SD): ", fmean(els), "  (", fsd(els, stable.algo = FALSE), ")\n", sep = "")
  }
  if (!is.null(x$edge_counts) && length(x$edge_counts))
    cat("Average number of visits per edge (SD): ", fmean(fmean(x$edge_counts)), "  (", fmean(fsd(x$edge_counts, stable.algo = FALSE)), ")\n", sep = "")
  if (!is.null(x$path_costs) && length(x$path_costs))
    cat("Average path cost (SD): ", fmean(fmean(x$path_costs)), "  (", fmean(fsd(x$path_costs, stable.algo = FALSE)), ")\n", sep = "")
  if (!is.null(x$path_weights) && length(x$path_weights))
    cat("Average path weight (SD): ", fmean(fmean(x$path_weights)), "  (", fmean(fsd(x$path_weights, stable.algo = FALSE)), ")\n", sep = "")
}
