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
#' @param trans_df A data frame of parameters for disease
#'   transmission probability distributions. Columns are:
#'   * `age_bkt`: The age bracket (ordered factor).
#'   * `sex`: The sex (factor with levels "M" and "F").
#'   * `med_cond`: Has a medical condition (logical).
#'   * `sympt`: Has symptoms of COVID-19, if infectious (logical).
#'   * `mu_shed`: Mean of the shedding parameter distribution.
#'   * `sigma_shed`: Scale (standard deviation) of the shedding parameter
#'     distribution.
#'   * `mu_susc`: Mean of the susceptibility parameter distribution
#'   * `sigma_susc`: Scale (standard deviation) of the susceptibility parameter
#'     distribution.
#' @param prog_df A data frame of parameters for disease
#'   progression probability distributions. Columns are:
#'   * `age_bkt`: The age bracket (ordered factor).
#'   * `sex`: The sex (factor with levels "M" and "F").
#'   * `med_cond`: Has a medical condition (logical).
#'   * `sympt`: Has symptoms of COVID-19, if infectious (logical).
#'   * `shape_ei`: Shape parameter for progression from "E" to "I" compartment.
#'   * `scale_ei`: Scale parameter for progression from "E" to "I" compartment.
#'   * `shape_ir`: Shape parameter for progression from "I" to "R" compartment.
#'   * `scale_ir`: Scale parameter for progression from "I" to "R" compartment.
#' @param home_nw_params A named list with parameters for the home-contacts
#'   network. The list should have elements:
#'   * `freq`: The frequency of contacts in the network (contacts per day)
#'   * `intens`: The intensity of contacts in the network
#'   * `topol`: The topology of the network: "smallworld" or "Barabasi Albert".
#'   * `nbrw`: The size of the neighborhood (small world) or the number of edges
#'     to add per time step (BA).
#'   * `p_rewire`: The probability of rewiring an edge (small world) or the
#'     power of the preferential attachments (BA).
#' @param social_nw_params A named list with parameters for social contacts.
#' @param work_nw_params A named list with parameters for work contacts.
#'
#' @return A named list of the model: agents, network, contagion
#'   probabilities for disease transmission, and transition probabilities for
#'   progress to the next stage of the disease.
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
setup_model <- function(agent_count, age_dist, p_female,
                        seir_dist, p_med_cond, p_symptomatic,
                        trans_df = NULL, prog_df = NULL,
                        home_nw_params = NULL, social_nw_params = NULL,
                        work_nw_params = NULL) {
  if (is.null(home_nw_params)) {
    home_nw_params <- list(freq = 5, intens = 1.0,
                           topol = "small world",
                           nbrs = 2, p_rewire = 0.05)
  }
  if (is.null(social_nw_params)) {
    social_nw_params <- list(freq = 1, intens = 0.25,
                             topol = "Barabasi Albert",
                             nbrs = 5, p_rewire = 0.10)
  }
  if (is.null(work_nw_params)) {
    work_nw_params <- list(freq = 3, intens = 0.5,
                           topol = "Barabasi Albert",
                           nbrs = 5, p_rewire = 0.10)
  }
  if (is.null(trans_df)) {
    trans_df <- .covidABM$trans_df
  }
  if (is.null(prog_df)) {
    prog_df <- .covidABM$prog_df
  }

  # Pacify R CMD check for non-standard evaluation
  age_bkt  <- NULL
  id       <- NULL
  scale_ei <- NULL
  scale_ir <- NULL
  seir     <- NULL
  shape_ei <- NULL
  shape_ir <- NULL
  target   <- NULL

  if (is.integer(trans_df$age_bkt)) {
    trans_df <- trans_df[, age_bkt := .covidABM$age_brackets$bracket[age_bkt]]
  }
  if (is.integer(prog_df$age_bkt)) {
    prog_df <- prog_df[, age_bkt := .covidABM$age_brackets$bracket[age_bkt]]
  }

  distr <- generate_agent_params(agent_count, age_dist, p_female,
                                 seir_dist, p_med_cond, p_symptomatic)
  agents <- build_agent_df(age = distr$age, sex = distr$sex,
                           med_cond = distr$med_cond,
                           seir_status = distr$seir,
                           sympt = distr$sympt,
                           transmission_params = trans_df,
                           progression_params = prog_df)

  agents <- agents[, seir := as.integer(seir)]
  setkey(agents, id, seir)

  level_e <- get_seir_level("E")
  level_i <- get_seir_level("I")
  agents[seir == level_e, target :=
           set_target(.N, shape = shape_ei, scale = scale_ei)]
  agents[seir == level_i, target :=
           set_target(.N, shape = shape_ir, scale = scale_ir)]

  home <- create_network(agents, nw_type = "home",
                         nw_frequency = home_nw_params$freq,
                         nw_intensity = home_nw_params$intens,
                         topology = home_nw_params$topol,
                         nei = home_nw_params$nbrw, p = home_nw_params$p_rewire)
  social <- create_network(agents, nw_type = "social",
                           nw_frequency = social_nw_params$freq,
                           nw_intensity = social_nw_params$intens,
                           topology = social_nw_params$topol,
                           nei = social_nw_params$nbrw, p = social_nw_params$p_rewire)
  work <- create_network(agents, nw_type = "social",
                         nw_frequency = work_nw_params$freq,
                         nw_intensity = work_nw_params$intens,
                         topology = work_nw_params$topol,
                         nei = work_nw_params$nbrw, p = work_nw_params$p_rewire)
  setkeyv(home$neighbors,   c("head", "tail"))
  setkeyv(social$neighbors, c("head", "tail"))
  setkeyv(work$neighbors,   c("head", "tail"))

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
#'
#' @return A model, as returned by [setup_model()]
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
setup_test_model <- function(n_agents = 100, mean_age = 40.0, std_age = 20.0,
                             p_female = 0.50, p_med_cond = 0.20,
                             p_seir = c(0.99, 0.0, 0.01, 0.0),
                             p_sympt = 0.50) {
  f_age <- ~stats::rnorm(n = .n, mean = mean_age, sd = std_age)
  xp <- rlang::f_rhs(f_age)
  xp <- rlang::call_standardise(xp)
  xp <- rlang::call_modify(xp, mean = mean_age, sd = std_age)
  rlang::f_rhs(f_age) <- xp
  f_seir <- ~base::sample(c("S", "E", "I", "R"), .n, TRUE, p_seir)
  xp <- rlang::f_rhs(f_seir)
  xp <- rlang::call_standardise(xp)
  xp <- rlang::call_modify(xp, prob = p_seir)
  rlang::f_rhs(f_seir) <- xp

  model <- setup_model(n_agents, f_age, p_female,
                       f_seir, p_med_cond, p_sympt)
  invisible(model)
}
