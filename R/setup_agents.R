#' Build a list of parameters for constructing agents
#'
#' Generate parameters for constructing agents. Given distributions for
#' ages, sexes, medical conditions, SEIR status, and symptomatic conditions
#' for infectious individuals, generate a list of parameters for constructing
#' a set of agents.
#'
#' @param agent_count The number of agents to generate parameters for.
#' @param age_dist A function to sample from a probability distribution or
#'   else a vector or list of ages. The vector will be repeated if
#'   necessary to produce enough agents.
#' @param p_female A probability of being female or else a vector or list of
#'   sexes ("M" or "F") or logical (`TRUE` for female, `FALSE` for male).
#'   The vector will be repeated if necessary to produce enough agents.
#' @param seir_dist A function to sample from a probability distribution or
#'   else a vector or list of SEIR statuses ("S", "E", "I", or "R"). The vector
#'   will be repeated if necessary to produce enough agents.
#' @param p_med_cond A probability of having a medical condition or else a
#'   vector or list of medical conditions (logical: `TRUE` or `FALSE`).
#'   The vector will be repeated if necessary to produce enough agents.
#' @param p_sympt A probability of being symptomatic if infectious, or else
#'   a vector or list of symptomatic status if infectious
#'   (logical: `TRUE` or `FALSE`). The vector will be repeated if necessary to
#'   produce enough agents.
#'
#' @return A named list of parameters for `age`, `sex`, `seir`, `med_cond`,
#'  and `sympt`.
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
generate_agent_params <- function(agent_count, age_dist, p_female,
                                  seir_dist, p_med_cond, p_sympt) {
  seir_levels <- get("seir_levels", envir = .covidABM)
  # Set up ages
  if(is.numeric(age_dist)) {
    ages <- rep_len(age_dist, agent_count)
  } else if (is.function(age_dist)) {
    ages <- age_dist(agent_count)
  } else if (rlang::is_formula(age_dist)) {
    ages <- eval(rlang::f_rhs(age_dist), envir = list(.n = agent_count))
  } else {
    stop("Invalid type for age_dist")
  }

  # Set up SEIR status
  if(is.numeric(seir_dist)) {
    assertthat::assert_that(length(seir_dist == length(seir_levels)))
    seir <- sample(names(seir_levels), agent_count, replace = TRUE,
                   prob = seir_dist)
  } else if (is.character(seir_dist) || is.factor(seir_dist)) {
    seir <- rep_len(seir_dist, agent_count)
  } else if (is.function(seir_dist)) {
    seir <- seir_dist(agent_count)
  } else if (rlang::is_formula(seir_dist)) {
    seir <- eval(rlang::f_rhs(seir_dist), envir = list(.n = agent_count))
  } else {
    stop("Invalid type for seir_dist")
  }
  seir <- ordered(seir, levels = names(seir_levels))

  # Set up sex
  if (is.logical(p_female)) {
    sex <- rep_len(p_female, agent_count)
  } else if (is.character(p_female)) {
    sex = rep_len(
      stringr::str_starts(p_female, stringr::fixed("F", ignore_case = TRUE)),
                  agent_count)
  } else if (is.integer(p_female)) {
    sex = rep_len(p_female > 0, agent_count)
  } else if (is.double(p_female)) {
    sex = purrr::rbernoulli(agent_count, p_female)
  } else {
    stop("Invalid type for p_female")
  }
  sex <- ifelse(sex, "F", "M")

  # Set up pre-existing medical conditions
  if (is.logical(p_med_cond)) {
    med_cond <- rep_len(p_med_cond, agent_count)
  } else if (is.character(p_med_cond)) {
    med_cond = rep_len(
      stringr::str_starts(p_med_cond, stringr::fixed("F", ignore_case = TRUE)),
      agent_count)
  } else if (is.integer(p_med_cond)) {
    med_cond = rep_len(p_med_cond > 0, agent_count)
  } else if (is.double(p_med_cond)) {
    med_cond = purrr::rbernoulli(agent_count, p_med_cond)
  } else {
    stop("Invalid type for p_med_cond")
  }

  # Set up symptomatic status for infected agents
  if (is.logical(p_sympt)) {
    sympt <- rep_len(p_sympt, agent_count)
  } else if (is.character(p_sympt)) {
    sympt = rep_len(
      stringr::str_starts(p_sympt, stringr::fixed("F", ignore_case = TRUE)),
                    agent_count)
  } else if (is.integer(p_sympt)) {
    sympt = rep_len(p_sympt > 0, agent_count)
  } else if (is.double(p_sympt)) {
    sympt = purrr::rbernoulli(agent_count, p_sympt)
  } else {
    stop("Invalid type for p_sympt")
  }

  invisible(list(age = ages, sex = sex, seir = seir,
                 med_cond = med_cond, sympt = sympt))
}


#' Set up a model
#'
#' Initialize a model (agents and network).
#'
#' @param agent_count The number of agents to generate parameters for.
#' @param age_dist A function to sample from a probability distribution or
#'   else a vector or list of ages. The vector will be repeated if
#'   necessary to produce enough agents.
#' @param p_female A probability of being female or else a vector or list of
#'   sexes ("M" or "F") or logical (`TRUE` for female, `FALSE` for male).
#'   The vector will be repeated if necessary to produce enough agents.
#' @param seir_dist A function to sample from a probability distribution or
#'   else a vector or list of SEIR statuses ("S", "E", "I", or "R"). The vector
#'   will be repeated if necessary to produce enough agents.
#' @param p_med_cond A probability of having a medical condition or else a
#'   vector or list of medical conditions (logical: `TRUE` or `FALSE`).
#'   The vector will be repeated if necessary to produce enough agents.
#' @param p_symptomatic A probability of being symptomatic if infectious, or
#'   else a vector or list of symptomatic status if infectious
#'   (logical: `TRUE` or `FALSE`). The vector will be repeated if necessary to
#'   produce enough agents.
#' @param neighbors The number of neighbor edges in the ring network graph to
#'   initialize the small world network (2 = connect to both nearest neighbors
#'   on the ring;  4 = connect to both neighbors and their neighbors, etc.).
#' @param p_rewire The probability to rewire an edge from a neighbor to a
#'   random node..
#' @param probs A contagion probability matrix
#' @param trans Disease progression transition parameters
#'
#' @return A named list of the model: agents, network, contagion
#'   probabilities for disease transmission, and transition probabilities for
#'   progress to the next stage of the disease.
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
setup_model <- function(agent_count, age_dist, p_female,
                        seir_dist, p_med_cond, p_symptomatic,
                        neighbors, p_rewire, trans_df = NULL, prog_df = NULL) {
  if (is.null(trans_df)) {
    trans_df <- .covidABM$trans_df
  }
  if (is.null(prog_df)) {
    prog_df <- .covidABM$prog_df
  }


  if (is.integer(trans_df$age_bkt)) {
    trans_df <- trans_df[, age_bkt := .covidABM$age_brackets$bracket[age_bkt]]
  }
  if (is.integer(prog_df$age_bkt)) {
    prog_df <- prog_df[, age_bkt := .covidABM$age_brackets$bracket[age_bkt]]
  }

  distr <- generate_agent_params(agent_count, age_dist, p_female,
                                 seir_dist, p_med_cond, p_symptomatic)
  agents <- create_agents(age = distr$age, sex = distr$sex,
                          seir_status = distr$seir,
                          med_cond = distr$med_cond, sympt = distr$sympt,
                          fix_sympt = TRUE)

  agents <- set_agent_probs(agents, trans_df, prog_df)
  agents <- agents[, seir := as.integer(seir)]
  setkey(agents, id, seir)

  level_e <- get_seir_level("E")
  level_i <- get_seir_level("I")
  agents[seir == level_e, target :=
           set_target(.N, shape = shape_ei, scale = scale_ei)]
  agents[seir == level_i, target :=
           set_target(.N, shape = shape_ir, scale = scale_ir)]

  home <- create_network(agents, nw_type = "home", nw_frequency = 5,
                                nw_intensity = 1.0, topology = "small world",
                                nei = 2, p = 0.05)
  social <- create_network(agents, nw_type = "social", nw_frequency = 1,
                           nw_intensity = 0.25, topology = "Barabasi Albert",
                           nei = 5, p = 0.10)
  work <- create_network(agents, nw_type = "social", nw_frequency = 3,
                           nw_intensity = 0.5, topology = "Barabasi Albert",
                           nei = 5, p = 0.10)
  setkey(home$neighbors,   head, tail)
  setkey(social$neighbors, head, tail)
  setkey(work$neighbors,   head, tail)

  networks <- list(home = home, work = work, social = social)

  invisible(list(agts = agents, nws = networks,
                 neighbors = purrr::map(networks, ~.x$neighbors)))
}


#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param n_agents The number of agents in the model.
#' @param mean_age The mean age of an agent.
#' @param std_age The standard deviation of ages among agents.
#' @param p_female The fraction of agents who are female.
#' @param p_seir A vector of probabilities for "S", "E", "I", and "R" status.
#'   These should add up to 1.0.
#' @param p_med_cond Probability of having a medical condition.
#' @param p_sympt Probability of being symptomatic when infectious.
#' @param neighbors The number of neighbor edges in the ring network graph to
#'   initialize the small world network (2 = connect to both nearest neighbors
#'   on the ring;  4 = connect to both neighbors and their neighbors, etc.).
#' @param p_rewire The probability to rewire an edge from a neighbor to a
#'   random node..
#'
#' @return A model, as returned by [setup_model()]
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
setup_test_model <- function(n_agents = 100, mean_age = 40.0, std_age = 20.0,
                             p_female = 0.50, p_med_cond = 0.20,
                             p_seir = c(0.99, 0.0, 0.01, 0.0),
                             p_sympt = 0.50, neighbors = 5, p_rewire = 0.05) {
  f_age <- ~rnorm(n = .n, mean = mean_age, sd = std_age)
  xp <- rlang::f_rhs(f_age) %>% rlang::call_standardise() %>%
    rlang::call_modify(mean = mean_age, sd = std_age)
  rlang::f_rhs(f_age) <- xp
  f_seir <- ~base::sample(c("S", "E", "I", "R"), .n, TRUE, p_seir)
  xp <- rlang::f_rhs(f_seir) %>% rlang::call_standardise() %>%
    rlang::call_modify(prob = p_seir)
  rlang::f_rhs(f_seir) <- xp

  model <- setup_model(n_agents, f_age, p_female,
                       f_seir, p_med_cond, p_sympt,
                       neighbors, p_rewire)
  invisible(model)
}
