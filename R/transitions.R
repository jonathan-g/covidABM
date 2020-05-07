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

set_ticks <- function(n, shape, scale) {
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
  agent_e <<- agents[seir == level_e,]
  agent_i <<- agents[seir == level_i,]
  e_to_i <<- with(agent_e,
                  purrr::rbernoulli(transition_prob(ticks, shape = shape_ei,
                                            scale = scale_ei)))
  ids <- agent_e[e_to_i]$id
  agents[id %in% ids, c("seir", "ticks") := .(level_i, 0)]

  i_to_r <<- with(agent_i,
                  purrr::rbernoulli(transition_prob(ticks, shape = shape_ir,
                                               scale = scale_ir)))
  ids <- agent_i[i_to_r]$id
  agents[id %in% ids, c("seir", "ticks") := .(level_r, 0)]

  # agent_e[purrr::rbernoulli(transition_prob(ticks, shape = shape_ei,
  #                                           scale = scale_ei)),
  #         ]$seir <- level_i
  # agent_i[purrr::rbernoulli(transition_prob(ticks, shape = shape_ir,
  #                                           scale = scale_ir)),
  #         ]$seir <- level_r
  invisible(agents)
}
