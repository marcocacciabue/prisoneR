#' Plot absolute Fst function
#'
#' `plot_absolute()` is a simple helper function that returns a ggplot object
#' visualizing Absolute Fst across the generations. To use after running  the
#' `game()` function
#'
#' @param df dataframe: created with the `game()` function
#'
#' @return ggplot2 object
#' @export
#' @importFrom ggplot2 aes geom_line labs .data
#' @example inst/examples/plot_absolute.R
#'
#'
#'


plot_absolute <- function(df) {
  # if (!requireNamespace("ggplot2", quietly = TRUE)) {
  #   stop(
  #     "Package ggplot2 must be installed to use this function.",
  #     call. = FALSE
  #   )
  # }
  generations <- length(df$Absolute_fst)
  x_time <- 1:generations
  p <- ggplot2::ggplot(df) +
    geom_line(aes(x = x_time, y = .data$Absolute_fst)) +
    labs(x = "Generations", y = "Absolute Fst")
  return(p)
}


#' Plot Relative  Fst function
#'
#' `plot_relative()` is a simple helper function that returns a ggplot object
#' visualizing Absolute Fst across the generations. To use after running  the
#' `game()` function
#'
#' @param df dataframe: created with the `game()` function
#'
#' @return ggplot2 object
#' @export
#' @importFrom ggplot2 aes geom_line labs .data
#' @example inst/examples/plot_relative.R
#'
#'
#'


plot_relative <- function(df) {
  # if (!requireNamespace("ggplot2", quietly = TRUE)) {
  #   stop(
  #     "Package ggplot2 must be installed to use this function.",
  #     call. = FALSE
  #   )
  # }
  generations <- length(df$Relative_fst)
  x_time <- 1:generations
  p <- ggplot2::ggplot(df) +
    geom_line(aes(x = x_time, y = .data$Relative_fst)) +
    labs(x = "Generations", y = "Relative Fst")
  return(p)
}
