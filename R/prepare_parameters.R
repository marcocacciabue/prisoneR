
#' Prepare parameters
#'
#' Simple helper function to create a list of example parameter
#' to set up the examples simulations.
#'
#' @return list: all neccesary parameters for the examples simulations
#' @export
#'
#' @examples
#' parameters <- prepare_parameters()
prepare_parameters <- function() {
  params <- list(
    mutation1 = 10**-6,
    mutation2 = 10**-6,
    genome1 = 5000,
    genome2 = 8500,
    # (1:1) cooperate/cooperate
    r1_1_1 = 0.9,
    r2_1_1 = 0.19,
    a12_1_1 = 0.012,
    a21_1_1 = -0.2,
    # (1:2) cooperate/defective
    r1_1_2 = 0.1,
    r2_1_2 = 0.0018,
    a12_1_2 = -1,
    a21_1_2 = 0.1,
    # (2:1) defective/cooperate
    r1_2_1 = 0.15,
    r2_2_1 = 0.18,
    a12_2_1 = -1,
    a21_2_1 = -0.001,
    # (2:2) defective/defective
    r1_2_2 = 0.8,
    r2_2_2 = -0.8,
    a12_2_2 = -0.1,
    a21_2_2 = -0.1
  )
  return(params)
}
