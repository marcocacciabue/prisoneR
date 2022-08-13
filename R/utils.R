
#' Check if list is nested
#'
#' Internal function to test if a list is nested. Used for plotting
#' several set of parameters at once.
#'
#' @param l list of lists
#'
#' @return logical: True for nested list.
#'
#'
#'
isNested_list <- function(l) {
  stopifnot(is.list(l))
  for (i in l) {
    if (is.list(i)) return(TRUE)
  }
  return(FALSE)
}
