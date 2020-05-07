#' Look up a progression probability to the next compartment
#'
#' Look up a probability for an agent in compartment E or I to progress to
#' the next compartment (I or R, respectively), based on the characteristics of
#' agent and subject.
#'
#' @param ticks The number of time steps in which the agent has been in the
#'   current compartment
#' @param shape The shape parameter for the transition probability distribution
#' @param scale The scale parameter for the transition probability distribution
#'
#' @return A vector of progression probabilities.
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
transition_prob <- function(ticks, shape, scale) {
  p <- pgamma(ticks, shape = shape, scale = scale) -
    pgamma(ticks - 1, shape = shape, scale = scale)
  p
}

set_target <- function(n, shape, scale) {
  if (missing(n)) {
    n <- length(shape)
  }
  ticks <- rgamma(n, shape = shape, scale = scale)
  ticks <- as.integer(ceiling(ticks))
  ticks
}

#' Progress agents in the "E" and "I" compartments to the next compartments
#' ("I" and "R", respectively).
#'
#' @param agents A `data.table` containing agents. This `data.table` should have
#'   columns:
#'   * `seir`: The SEIR status (integer index ranging from 1 to 4)
#'   * `ticks`: The number of time steps the agent has been in the compartment.
#'   * `ei_shape`, `ei_scale`, `ir_shape`, `ir_scale`: shape and scale
#'     parameters for the probability distriubtions for transitions from the
#'     current compartment to the next compartment for "E" to "I" and "I" to
#'     "R", respectively.
#'
#' @return Invisibly returns a modified version of the `agents` data table.
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
progress_disease <- function(agents) {
  level_e <- get_seir_level("E")
  level_i <- get_seir_level("I")
  level_r <- get_seir_level("R")

  agents[seir == level_e & target <= ticks,
         c("seir", "ticks", "target") :=
           .(level_i, 0, set_target(.N, shape_ir, scale_ir))]
  agents[seir == level_i & target <= ticks,
         c("seir", "ticks", "target") := list(level_r, 0, 0)]

  invisible(agents)
}
