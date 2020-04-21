#' Translate a letter into an integer SEIR level
#'
#' Translate a character SEIR status into an integer index equivalent to the
#' factor level
#'
#' @param x a character or character vector with values "S", "E", "I", or "R".
#'
#' @return An integer value
#' @examples
#' get_seir_level("I")
get_seir_level <- function(x) {
  assert_that(x %in% names(seir_levels))
  seir_levels[x]
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
decode_seir <- function(x) {
  assert_that(all(x %in% seir_levels))
  names(seir_levels)[x] %>% ordered(levels = names(seir_levels))
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
get_age_bracket <- function(x) {
  age_brackets$bracket[map_int(x, ~which(age_brackets$upper > .x)[1])]
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
#' @param sympt Logical: subject is infected and has symptoms.
#'
#' @return A tibble of agent characteristics with columns
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
create_agents <- function(age, sex, med_cond, seir_status, sympt,
                          fix_sympt = TRUE) {
  assert_that(is.numeric(age), msg = 'age should be a numeric value (years)')
  assert_that(all(sex %in% c("M", "F")), msg = ('Sex should be "M" or "F"'))
  sex <- factor(sex, levels = c("M", "F"))
  assert_that(is.logical(med_cond),
              msg = 'med_cond should be logical (TRUE or FALSE)')
  assert_that(is.logical(sympt),
              msg = 'sympt should be logical (TRUE or FALSE)')
  assert_that(all(seir_status %in% names(seir_levels)),
              msg = 'seir_status should be "S", "E", "I", or "R"' )
  if (fix_sympt) {
    sympt <- ifelse(seir_status == "I", sympt, FALSE)
  }
  assert_that(all(seir_status == "I" | ! sympt),
              msg = "It should not be possible to be sympt and not infected.")
  seir_status <-  ordered(seir_status, levels= names(seir_levels))
  agents <- tibble(age = age, age_bkt = get_age_bracket(age),
                   sex = sex, med_cond = med_cond,
                   sympt = sympt,
                   seir = seir_status, id = seq_along(age),
                   ticks = 0) %>%
    mutate_if(is.logical, ~1 + as.integer(.))
  invisible(agents)
}


