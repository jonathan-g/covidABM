#' Advance model by one tick
#'
#' Advance model by one tick (i.e., one day). Calculate progress of disease
#' (exposed people become infectious, and infectious people recover) and then
#' calculate transmission of the disease (stochastically, infectious people may
#' infect susceptible people) through network connections.
#'
#' @param model A model object.
#' @param probs A probability array (n-dimensional) for transmission of
#'   infection from infectious to susceptible agents.
#' @param An array of transition probabilities between compartments of the
#'   disease.
#'
#' @return An updated model
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
tick <- function(model, probs, transitions) {
  level_e <- get_seir_level("E")
  level_i <- get_seir_level("I")
  level_r <- get_seir_level("R")
  nw <- model$nw

  vertex_attr(nw, "ticks") <- vertex_attr(nw, "ticks") + 1

  v <- V(nw)
  i_nodes <- v[v$seir == level_i]
  e_nodes <- v[v$seir == level_e]
  i_probs <- get_transition(i_nodes, transitions$i)
  e_probs <- get_transition(e_nodes, transitions$e)


  i_trans_nodes <- i_nodes[rbernoulli(length(i_probs), i_probs)]
  e_trans_nodes <- e_nodes[rbernoulli(length(e_probs), e_probs)]

  message(length(i_nodes), " infected nodes: ", length(i_trans_nodes), " recover.")
  message(length(e_nodes), " exposed nodes: ", length(e_trans_nodes), " become infectious.")
  # message("i_probs = (", str_c(i_probs, collapse = ", "), ")")
  # message("e_probs = (", str_c(e_probs, collapse = ", "), ")")

  vertex_attr(nw, "seir", i_trans_nodes) <- level_r
  vertex_attr(nw, "ticks", i_trans_nodes) <- 0

  vertex_attr(nw, "seir", e_trans_nodes) <- level_i
  vertex_attr(nw, "ticks", e_trans_nodes) <- 0

  nw <- infect(nw, probs)

  model$nw <- nw
  invisible(model)
}
