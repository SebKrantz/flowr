#' Transport Modeling
#'
#' @description
#'
#' \emph{flowr} provides efficient tools for transportation modeling, specifically route
#' enumeration and traffic assignment tasks. The package implements the path-sized logit model for
#' traffic assignment and provides utilities for network processing.
#'
#'
#' \strong{Network Processing}
#'
#'  \code{\link[=simplify_network]{simplify_network()}} --- Simplify network by keeping only traversed edges\cr
#'  \code{\link[=linestring_to_graph]{linestring_to_graph()}} --- Convert LINESTRING geometries to graph\cr
#'  \code{\link[=create_undirected_graph]{create_undirected_graph()}} --- Convert directed graph to undirected\cr
#'
#'
#' \strong{Traffic Assignment}
#'
#'  \code{\link[=run_assignment]{run_assignment()}} --- Run traffic assignment using path-sized logit model\cr
#'
#'
#' \strong{Graph Utilities}
#'
#'  \code{\link[=nodes_from_graph]{nodes_from_graph()}} --- Extract unique nodes from graph\cr
#'  \code{\link[=dist_mat_from_graph]{dist_mat_from_graph()}} --- Compute distance matrix from graph\cr
#'
#' @details
#' The package uses efficient C implementations for critical path operations and leverages:
#' \itemize{
#'   \item \code{collapse} - Fast data transformations
#'   \item \code{igraph} - Graph operations and shortest path algorithms
#' }
#'
#' @author Sebastian Krantz \email{sebastian.krantz@graduateinstitute.ch} and Kamol Roy \email{kamol.roy08@gmail.com}
#' @name flowr-package
#' @aliases flowr
NULL

