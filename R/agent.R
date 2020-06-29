#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param agents A data frame describing agents, with columns:
#'   * `age`: Age in years (numeric).
#'   * `age_bkt`: The age bracket (ordered factor).
#'   * `sex`: The sex (factor with levels "M" and "F").
#'   * `med_cond`: Has a medical condition (logical).
#'   * `sympt`: Has symptoms of COVID-19, if infectious (logical).
#'   * `seir`: SEIR status (ordered factor with levels "S", "E", "I", and "R").
#'   * `id`: Unique ID number (integer)
#'   * `ticks`: days since reaching current disease stage (relevant to "E" and
#'     "I" status) (integer).
#'   * `target`: target ticks for transition to next compartment (relevant to
#'     "E" and "I" status only).
#' @param transmission_params A data frame of parameters for disease
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
#' @param progression_params A data frame of parameters for disease
#'   progression probability distributions. Columns are:
#'   * `age_bkt`: The age bracket (ordered factor).
#'   * `sex`: The sex (factor with levels "M" and "F").
#'   * `med_cond`: Has a medical condition (logical).
#'   * `sympt`: Has symptoms of COVID-19, if infectious (logical).
#'   * `shape_ei`: Shape parameter for progression from "E" to "I" compartment.
#'   * `scale_ei`: Scale parameter for progression from "E" to "I" compartment.
#'   * `shape_ir`: Shape parameter for progression from "I" to "R" compartment.
#'   * `scale_ir`: Scale parameter for progression from "I" to "R" compartment.
#'
#' @return A `data.table` with transmission probabilities and progression
#'   parameters for each agent. Columns are:
#'   * `age`: Age in years (numeric).
#'   * `age_bkt`: The age bracket (ordered factor).
#'   * `sex`: The sex (factor with levels "M" and "F").
#'   * `med_cond`: Has a medical condition (logical).
#'   * `sympt`: Has symptoms of COVID-19, if infectious (logical).
#'   * `seir`: SEIR status (ordered factor with levels "S", "E", "I", and "R").
#'   * `id`: Unique ID number (integer)
#'   * `ticks`: days since reaching current disease stage (relevant to "E" and
#'     "I" status) (integer).
#'   * `target`: target ticks for transition to next compartment (relevant to
#'     "E" and "I" status only).
#'   * `x_shed`: Shedding parameter.
#'   * `x_susc`: Susceptibility parameter.
#'   * `shape_ei`: Shape parameter for progression from "E" to "I" compartment.
#'   * `scale_ei`: Scale parameter for progression from "E" to "I" compartment.
#'   * `shape_ir`: Shape parameter for progression from "I" to "R" compartment.
#'   * `scale_ir`: Scale parameter for progression from "I" to "R" compartment.
#'
#' @examples
#' # ADD_EXAMPLES_HERE
#' @keywords internal
set_agent_probs <- function(agents, transmission_params, progression_params) {
  agents <- as.data.table(agents)
  transmission_params <- as.data.table(transmission_params)
  progression_params <- as.data.table(progression_params)
  agents <- merge(agents, transmission_params,
                  by = c("age_bkt", "sex", "sympt", "med_cond"), all.x = TRUE)
  agents <- merge(agents, progression_params,
                  by = c("age_bkt", "sex", "sympt", "med_cond"), all.x = TRUE)

  # Pacify R CMD check for non-standard evaluation
  mu_shed <- NULL
  mu_susc <- NULL
  sigma_shed <- NULL
  sigma_susc <- NULL

  agents <- agents[, c("x_shed", "x_susc") :=
                     list(
                       # stats::rbeta(.N, p_shed_1, p_shed_2),
                       # stats::rbeta(.N, p_susc_1, p_susc_2)
                       stats::rnorm(.N, mu_shed, sigma_shed),
                       stats::rnorm(.N, mu_susc, sigma_susc)
                     )
  ]
  agents <- agents[, ! c("mu_shed", "mu_susc", "sigma_shed", "sigma_susc")]
  invisible(agents)
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
#' @param transmission_params A data frame with transmission parameters for
#'   agents
#' @param progression_params A data frame with parameters for the timing of
#'   disease progression (from "E" to "I" and "I" to "R")
#' @param sympt Subject will be symptomatic if they are infected (logical).
#' @param ticks For exposed or infected agents: Number of ticks since they
#'   entered that state.
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
#'     "I" status only) (integer).
#'   * `target`: target ticks for transition to next compartment (relevant to
#'     "E" and "I" status only).
#'   * `x_shed`: Shedding parameter (relevant to "I" status only)
#'   * `x_susc`: Susceptibility parameter (relevant to "S" status only)
#'   * `shape_ei`: Shape parameter for progression from "E" to "I" compartment.
#'   * `scale_ei`: Scale parameter for progression from "E" to "I" compartment.
#'   * `shape_ir`: Shape parameter for progression from "I" to "R" compartment.
#'   * `scale_ir`: Scale parameter for progression from "I" to "R" compartment.
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
build_agent_df <- function(age, sex, med_cond, seir_status,
                           transmission_params, progression_params,
                           sympt = NULL, ticks = NULL) {
  assertthat::assert_that(is.numeric(age),
                          msg = 'age should be a numeric value (years)')
  assertthat::assert_that(all(sex %in% c("M", "F")),
                          msg = 'Sex should be "M" or "F"')
  sex <- factor(sex, levels = c("M", "F"))
  assertthat::assert_that(is.logical(med_cond),
                          msg = 'med_cond should be logical (TRUE or FALSE)')
  assertthat::assert_that(is.logical(sympt),
                          msg = 'sympt should be logical (TRUE or FALSE)')
  assertthat::assert_that(all(seir_status %in% names(.covidABM$seir_levels)),
                          msg = 'seir_status should be "S", "E", "I", or "R"')
  if (is.null(ticks)) ticks <- 0L
  seir_status <-  ordered(seir_status, levels= names(.covidABM$seir_levels))
  agents <- list(age = age, age_bkt = get_age_bracket(age),
                 sex = sex, med_cond = med_cond,
                 sympt = sympt,
                 seir = seir_status, id = seq_along(age),
                 ticks = as.integer(ticks),
                 target = 0L)

  agents <- as.data.table(agents)
  agents$id <- seq(nrow(agents))
  agents <- set_agent_probs(agents, transmission_params, progression_params)
  invisible(agents)
}
