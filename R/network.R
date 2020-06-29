#' Build a network from a set of agents
#'
#' Given a set of agents, build a network and give each vertex the
#' attributes of one of the agents
#'
#' @param agents A data.table where each row is an agent and the columns
#'   include a variable "id" that serves as a unique ID for the agent.
#' @param nw_type A description of the type of network. This
#'   is purely descriptive and doesn't affect the creation of the network.
#' @param nw_frequency The mean frequency of contact (per time step) for this
#'   kind of network contact.
#' @param nw_intensity The intensity of contact for this kind of network
#'   contact. Should be a real number between 0 and 1 indicating the intensity
#'   of close personal contact.
#' @param nei Network distance (around a ring or hyper-torus) that will be
#'   connected by neighbor links in small-world networks.
#'   If `nei` is 2, then the small world
#'   network is initialized with edges to neighbors and neighbors-of-neighbors
#'   in each direction and dimension.
#'   For preferential attachment (or BA) networks, `nei` is the number of edges
#'   to add in each time step while constructing the network.
#' @param p For small world networks, this is the probability of rewiring an
#'   edge. For preferential attachment (or BA) networks, `p` is the power of
#'   preferential attachment (1 for linear preferential attachment).
#'
#'   Small worlds start out with dense connections between neighbors
#'   (up to separation-degree `nei`), and then a random subset, chosen with
#'   robabilty `p` is rewired to random vertices.
#'
#'   Preferential attachment networks, start with no edges and `nei` edges
#'   are added at each step, with the preference for attaching to another
#'   vertex given by
#'   \ifelse{html}{\out{<i>P<sub>i</sub> ~ k<sub>i</sub><sup>p</sup> +
#'   a</i>}}{\deqn{P_i ~ k_i^p + a}}
#'   where \ifelse{html}{\out{<i>k<sub>i</sub>}}{\eqn{k_i}} is the number of
#'   edges originating at other vertices that connect to vertex _i_,
#'   _p_ is the preferential attachment power (1 for linear preference), and
#'   _a_ is the affinity for unconnected vertices (which we set to 1).
#'
#' @return A list of the network and a data table with one row corresponding to
#'   each edge of the network.
#' @examples
#' \dontrun{
#' n_agt <- 20
#' agents <- create_agents(age = runif(n_agt, max = 90),
#'   sex = sample(c("F", "M"), n_agt, replace = TRUE),
#'   med_cond = purrr::rbernoulli(n_agt, 0.20),
#'   seir_status = sample(c("S", "E", "I", "R"), replace = TRUE,
#'     prob = c(0.9, 0.05, 0.05, 0.0)),
#'   sympt = purrr::rbernoulli(n_agt, 0.5))
#' network <- create_network(agents, 1, 5, 0.05)
#' }
#' @keywords internal
create_network <- function(agents, nw_type, nw_frequency, nw_intensity,
                           topology, nei, p,
                           ba_dist = NULL, ba_seq = NULL,
                           ba_zero_appeal = 1, ba_out_pref = FALSE) {
  assertthat::assert_that(is.data.frame(agents))
  agents <- as.data.table(agents)
  # agents <- agents[order(id)]
  topology <- stringr::str_to_lower(topology)
  topology <- stringr::str_replace_all(topology, "[^a-z]", "")
  if (topology %in% c("smallworld", "strogatzwatts", "sw")) {
    nwk <- igraph::sample_smallworld(dim = 1, size = nrow(agents),
                                     nei = nei, p = p)
  } else if (topology %in% c("ba", "barabasialbert", "preferentialattachment",
                             "scaleinvariant")) {
    nwk <- igraph::sample_pa(nrow(agents), power = p, m = nei,
                             out.dist = ba_dist, out.seq = ba_seq,
                             out.pref = ba_out_pref,
                             zero.appeal = ba_zero_appeal,
                             directed = FALSE)
  } else {
    stop("Unknown topology: ", topology)
  }
  nwk <- igraph::set_vertex_attr(nwk, name = "id",
                                 value = base::sample(agents$id, nrow(agents),
                                                      FALSE))
  # nwk <- igraph::set_vertex_attr(nwk, name = "foo", value = agents$id * 100 + 1)
  v <- igraph::V(nwk)
  e <- igraph::E(nwk)
  hid <- igraph::head_of(nwk, e)$id
  tid <- igraph::tail_of(nwk, e)$id
  # hid <- igraph::head_of(nwk, e)$foo
  # tid <- igraph::tail_of(nwk, e)$foo
  # n <- igraph::adjacent_vertices(nwk, v)
  neighbors <- data.table(head = hid, tail = tid,
                          edge_type = rep_len(nw_type, length(hid)),
                          freq = nw_frequency, strength = nw_intensity)
  invisible(list(network = nwk, neighbors = neighbors))
}

