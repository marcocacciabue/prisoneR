% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interactions.R
\name{interaction_dynamic_LV}
\alias{interaction_dynamic_LV}
\title{Lotka-Volterra dynamic interaction}
\usage{
interaction_dynamic_LV(p1Move, p2Move, parameters)
}
\arguments{
\item{p1Move}{1 or 2 for previous interaction}

\item{p2Move}{1 or 2 for previous interaction}

\item{parameters}{A list of parameters passed from the \code{game()} function}
}
\value{
A list with the modified parameters.
}
\description{
Preloaded function that handles the possible interactions
for a simulation
}
\examples{
#' params <- prepare_parameters()
#'
#'
#' interaction_dynamic_LV(p1Move=!,p2Move=2, parameters=params)
#'
#'
}
