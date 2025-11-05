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
#' @importFrom data.table fread rbindlist
process_od_matrix <- function(od_matrix_directory) {
  stop("Not yet implemented")
}

#' @title Get Unique Columns
#' @description Get columns with unique values in a data.table.
#'
#' @param df A data.table or data.frame.
#'
#' @return Character vector of column names that have unique values.
#'
#' @export
get_unique_cols <- function(df) {
  stop("Not yet implemented")
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

