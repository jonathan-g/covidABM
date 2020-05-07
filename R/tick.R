#' Advance model by one tick
#'
#' Advance model by one tick (i.e., one day). Calculate progress of disease
#' (exposed people become infectious, and infectious people recover) and then
#' calculate transmission of the disease (stochastically, infectious people may
#' infect susceptible people) through network connections.
#'
#' @param model A model object, created by [setup_model()]
#' @param tracing Whether to print informative messages about the results of
#'   the tick. If it's `TRUE` or 1, a message is printed. If it's greater than 1
#'   additional messages from [infect()] are printed.
#'
#' @return An updated model
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
tick <- function(model, tracing = FALSE) {
  tracing <- as.integer(tracing)
  verbose <- max(0, tracing - 1)
  level_s <- get_seir_level("S")
  level_e <- get_seir_level("E")
  level_i <- get_seir_level("I")
  level_r <- get_seir_level("R")

  agents <- model$agts

  if (tracing) {
    n_s <<- sum(agents$seir == level_s)
    n_e <<- sum(agents$seir == level_e)
    n_i <<- sum(agents$seir == level_i)
    n_r <<- sum(agents$seir == level_r)
  }

  agents$ticks <- agents$ticks + 1
  agents <- progress_disease(agents)
  if (tracing) {
    n_s2 <<- sum(agents$seir == level_s)
    n_e2 <<- sum(agents$seir == level_e)
    n_i2 <<- sum(agents$seir == level_i)
    n_r2 <<- sum(agents$seir == level_r)
  }

  agents <- infect(agents, model$neighbors)
  if (tracing) {
    n_s3 <<- sum(agents$seir == level_s)
    n_e3 <<- sum(agents$seir == level_e)
    n_i3 <<- sum(agents$seir == level_i)
    n_r3 <<- sum(agents$seir == level_r)

    d_r <<- n_r2 - n_r
    d_i <<- n_i2 + d_r - n_i
    d_e <<- n_e3 - n_e2

    assertthat::assert_that(n_s2 == n_s,
                            msg = "Susceptible population should not change under disease progression.")
    assertthat::assert_that(n_s3 == n_s - d_e,
                            msg = "Susceptible population should drop by the number of new infections.")
    assertthat::assert_that(n_r2 == n_r - d_i,
                            msg = "Recovered population should increase by the number of recoveries.")
    assertthat::assert_that(n_i3 == n_i2,
                            msg = "Number of infectious cases should not change during infect step.")
    assertthat::assert_that(n_s + n_e + n_i + n_r == n_s2 + n_e2 + n_i2 + n_r2,
                            msg = "Total population should be conserved during disease progression.")
    assertthat::assert_that(n_s2 + n_e2 + n_i2 + n_r2 == n_s3 + n_e3 + n_i3 + n_r3,
                            msg = "Total population should be conserved during infect step.")

    message("Disease progression:\n  ",
            d_e, " new infections (in exposed status),\n  ",
            d_i, " progressed from exposed to infectious,\n  ",
            d_r, " infectious individuals recovered.\n"
            )
  }

  model$agts <- agents
  invisible(model)
}
