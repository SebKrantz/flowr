#' Traffic Assignment Functions
#'
#' Functions for assigning traffic to routes using nested logit models.

#' @title Setup Assignment Directories
#' @description Set up directories for assignment process.
#'
#' @param network Character string specifying the network name.
#' @param scenario Character string specifying the scenario name.
#'
#' @return Character string path to the assignment results directory.
#'
#' @export
setup_assignment_directories <- function(network, scenario) {
  stop("Not yet implemented")
}

#' @title Get Assigned Tonnage
#' @description Calculate assigned tonnage for alternative routes using nested logit model.
#'
#' @param alternative_routes_df A data.table with alternative routes.
#' @param mode_col Character string specifying the column name for transportation mode.
#' @param alt_col Character string specifying the column name for alternative route identifier.
#' @param demand_row A data.table row or list with demand values (tons_2019, tons_2030, tons_2040).
#' @param cargo_type Character string specifying the type of cargo.
#' @param gen_lambda_df A data.table with lambda parameters by mode.
#' @param mode_lambda_df A data.table with upper level lambda parameters by mode.
#' @param mode_c_df A data.table with C parameters by mode.
#'
#' @return A data.table with assigned tonnage by link containing columns:
#' \itemize{
#'   \item \code{link_id} - Link identifier
#'   \item \code{dir} - Direction
#'   \item \code{cargo_type} - Cargo type
#'   \item \code{tons_2019} - Assigned tonnage for 2019
#'   \item \code{tons_2030} - Assigned tonnage for 2030
#'   \item \code{tons_2040} - Assigned tonnage for 2040
#' }
#'
#' @details
#' Uses nested logit model to calculate route choice probabilities and assign tonnage
#' based on generalized costs and choice parameters.
#'
#' @export
#' @importFrom data.table data.table
get_assigned_tonnage <- function(alternative_routes_df, mode_col, alt_col, 
                                 demand_row, cargo_type, gen_lambda_df, 
                                 mode_lambda_df, mode_c_df) {
  stop("Not yet implemented")
}

#' @title Run Assignment
#' @description Run traffic assignment for the network.
#'
#' @param network Character string specifying the network name.
#' @param scenario Character string specifying the scenario name.
#' @param year Character string specifying the analysis year (e.g., "2040").
#' @param enumeration_run_directory Optional character string path to enumeration results directory.
#'   If not provided, will use the most recent enumeration run.
#' @param cargo_type_map Optional named character vector mapping from OD matrix cargo types
#'   to enumeration cargo types. Defaults to common mappings (Container->Containerized, etc.).
#'
#' @details
#' This function:
#' \itemize{
#'   \item Loads network, zone nodes, OD matrix, and choice parameters
#'   \item Loads alternative routes from enumeration results
#'   \item Calculates tonnage assignment using nested logit model
#'   \item Aggregates results by link and direction
#'   \item Saves results as shapefile
#' }
#'
#' @return Invisibly returns NULL. Results are saved to the assignment results directory.
#'
#' @export
#' @importFrom data.table data.table
#' @importFrom sf st_write
run_assignment <- function(network, scenario, year, 
                          enumeration_run_directory = NULL,
                          cargo_type_map = NULL) {
  stop("Not yet implemented")
}

