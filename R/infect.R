#' Propagate infections on the network
#'
#' For each infected node on the network, find susceptible neighbors and
#' probabilistically spread the infection.
#'
#' @param agents A `data.table` containing agent information.
#' @param neighbors A list of `data.table`s, each containing the neighbors on
#'   a different network.
#'   Neighbor data.tables have columns:
#'   * `head`: the head vertex
#'   * `tail`: the tail vertex
#'   * `edge_type`: the kind of network (e.g., "home", "social", or "work")
#'   * `freq`: the frequency of contacts on this edge (times per day the two
#'     agents are in close contact).
#'   * `strength`: the intensity/closeness of the contact (e.g., health-care
#'     visits are very strong, paying a cashier at a store would be rather
#'     weak).
#' @param x0 The unmodified mean infection value..
#'
#' @return A modified network graph after propagating the infections.
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
infect <- function(agents, neighbors, x0 = -2.556) {
  s_level <- get_seir_level("S")
  i_level <- get_seir_level("I")
  e_level <- get_seir_level("E")

  # pacify R CMD Check with non-standard evaluation:
  id <- NULL
  p <- NULL
  scale_ei <- NULL
  seir <- NULL
  shape_ei <- NULL
  strength <- NULL
  tail <- NULL
  x_shed <- NULL
  x_susc <- NULL

  i_agents <- agents[seir == i_level, list(id, x_shed)]
  s_agents <- agents[seir == s_level, list(id, x_susc)]
  for (n in neighbors) {
    dt <- merge(i_agents, n, by.x = "id", by.y = "head")
    dt <- merge(dt, s_agents, by.x = "tail", by.y = "id")
    dt <- dt[, p := stats::plogis(x0 + x_shed + x_susc + strength)]
    dt <- dt[, infect := purrr::rbernoulli(length(p), p)]
    if (any(dt$infect)) {
      infected <- dt[infect == TRUE, list(tail)]
      agents[id %in% infected$tail,
             c("seir", "ticks", "target") :=
               list(e_level, 0, set_target(.N, shape_ei, scale_ei))]
      s_agents <- s_agents[! id %in% infected$tail]
    }
  }
  invisible(agents)
}
