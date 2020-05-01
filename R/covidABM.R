#' Age brackets
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{lower}{Lower bound for bracket}
#'   \item{upper}{Upper bound for bracket}
#'   \item{bracket}{Description of the bracket}
#' }
#' @keywords internal
"age_brackets"

#' Levels for SEIR factors
#'
#' @format A named vector of integers
#' @keywords internal
"seir_levels"

#' Contagion probability arraw
#' @format A six-dimensional array of doubles
#' @keywords internal
"probs"

#' Transition parameters for disease progression
#'
#' @format A named list of five-dimensional arrays of doubles
#' @keywords internal
"trans"

.covidABM <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  assign("trans_df", trans_df, envir = .covidABM)
  assign("prog_df", prog_df, envir = .covidABM)
  assign("seir_levels", seir_levels, envir = .covidABM)
  assign("age_brackets", age_brackets, envir = .covidABM)
  assign("tracing", FALSE, envir = .covidABM)
}

