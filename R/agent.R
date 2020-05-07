#' Translate a letter into an integer SEIR level
#'
#' Translate a character SEIR status into an integer index equivalent to the
#' factor level
#'
#' @param x a character or character vector with values "S", "E", "I", or "R".
#'
#' @return An integer value
#' @examples
#'   get_seir_level("I")
#'
#' @export
get_seir_level <- function(x) {
  assertthat::assert_that(x %in% names(.covidABM$seir_levels))
  .covidABM$seir_levels[x]
}


#' Translate integer index to SEIR level
#'
#' Translate an integer index to an SEIR factor
#'
#' @param x An integer (or vector) with values 1--4.
#'
#' @return A factor with levels "S", "E", "I", or "R"
#' @examples
#' decode_seir(3)
#'
#' @export
decode_seir <- function(x) {
  assertthat::assert_that(all(x %in% .covidABM$seir_levels))
  names(.covidABM$seir_levels)[x] %>%
    ordered(levels = names(.covidABM$seir_levels))
}

#' Get the age bracket
#'
#' Convert an age (in years) to a bracket.
#'
#' @param x Age in years (vector)
#'
#' @return Ordered factor indicating the age bracket
#' @examples
#' get_age_bracket(21)
#' get_age_bracket(c(10, 20, 21, 25, 29, 30, 31, 39, 40, 41))
#'
#' @export
get_age_bracket <- function(x) {
  .covidABM$age_brackets$bracket[
    purrr::map_int(x, ~which(.covidABM$age_brackets$upper > .x)[1])
  ]
}

#' Create one or more agents
#'
#' Create agents and initialize with properties.
#'
#' @param age Age in years.
#' @param sex Sex: "M" or "F" or a factor with levels "M" and "F".
#' @param med_cond Logical: has a pre-existing medical condition
#' @param seir_status SEIR status: "S", "E", "I", "R", or an ordered factor
#'   with those levels.
#' @param sympt Subject is infected and has symptoms (logical).
#' @param fix_sympt Set `sympt` to `FALSE` for agents where `seir_status` is
#'   not "I".
#'
#' @return A data.table of agent characteristics with columns
#'   * `age`: Age in years (numeric).
#'   * `age_bkt`: The age bracket (ordered factor).
#'   * `sex`: The sex (factor with levels "M" and "F").
#'   * `med_cond`: Has a medical condition (logical).
#'   * `sympt`: Has symptoms of COVID-19, if infectious (logical).
#'   * `seir`: SEIR status (ordered factor with levels "S", "E", "I", and "R").
#'   * `id`: Unique ID number (integer)
#'   * `ticks`: days since reaching current disease stage (relevant to "E" and
#'     "I" status) (integer).
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
create_agents <- function(age, sex, med_cond, seir_status,
                          p_sympt,
                          transmission_params,
                          progression_params,
                          ticks = NULL,
                          sympt = NULL,
                          fix_sympt = TRUE) {
  assertthat::assert_that(is.numeric(age),
                          msg = 'age should be a numeric value (years)')
  assertthat::assert_that(all(sex %in% c("M", "F")),
                          msg = 'Sex should be "M" or "F"')
  sex <- factor(sex, levels = c("M", "F"))
  assertthat::assert_that(is.logical(med_cond),
                          msg = 'med_cond should be logical (TRUE or FALSE)')
  assertthat::assert_that(is.logical(sympt),
                          msg = 'sympt should be logical (TRUE or FALSE)')
  assertthat::assert_that(all(seir_status %in% names(seir_levels)),
                          msg = 'seir_status should be "S", "E", "I", or "R"')
  if (is.null(sympt)) {
    sympt <- purrr::pbernoulli(length(seir_status), p_sympt)
  }
  if (is.null(ticks)) ticks <- 0
  seir_status <-  ordered(seir_status, levels= names(seir_levels))
  agents <- list(age = age, age_bkt = get_age_bracket(age),
                 sex = sex, med_cond = med_cond,
                 sympt = sympt,
                 seir = seir_status, id = seq_along(age),
                 ticks = ticks)

  agents <- as.data.table(agents)
  agents$id <- seq(nrow(agents))
  invisible(agents)
}


set_agent_probs <- function(agents, transmission_params, progression_params) {
  agents <- merge(agents, transmission_params,
                 by = c("age_bkt", "sex", "sympt", "med_cond"), all.x = TRUE)
  agents <- merge(agents, progression_params,
                 by = c("age_bkt", "sex", "sympt", "med_cond"), all.x = TRUE)

  # agents <- transmission_params$shed[agents, on = c("age_bkt", "sex", "sympt")]
  # agents <- transmission_params$susc[agents,
  #                  on = c("age_bkt", "sex", "med_cond")]
  # agents <- progression_params$ei[agents, on = c("age_bkt", "sex", "med_cond")]
  # agents <- progression_params$ir[agents,
  #                  on = c("age_bkt", "sex", "med_cond", "sympt")]

  agents <- agents[, c("x_shed", "x_susc") :=
                     list(
                       # rbeta(.N, p_shed_1, p_shed_2),
                       # rbeta(.N, p_susc_1, p_susc_2)
                       rnorm(.N, mu_shed, sigma_shed),
                       rnorm(.N, mu_susc, sigma_susc)
                     )
  ]
  agents <- agents[, ! c("mu_shed", "mu_susc", "sigma_shed", "sigma_susc")]
  invisible(agents)
}
