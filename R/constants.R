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
  seir_levels <- names(.covidABM$seir_levels)
  decoded <- seir_levels[x]
  decoded <- ordered(decoded, levels = seir_levels)
  decoded
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



#' Translate text representation of an age bracket into an ordered factor
#'
#' Take a text representation of an age bracket, such as '30-40' and
#' translate it into an ordered factor compatible with the global
#' `age_brackets` variable.
#'
#' @param x A character or factor vector containing one or more age bracket
#'   descriptions equivalent to levels of the `age_brackets` variable.
#'
#' @return An ordered factor
#' @examples
#' decode_age_bracket(c("0-19", "30-39"))
#' @export
decode_age_bracket <- function(x) {
  brackets <- .covidABM$age_brackets$bracket
  bkts <- NULL
  if (is.character(x)) {
    assertthat::assert_that(all(x %in% levels(brackets)),
                            msg = "Argument to decode_age_bracket should match the levels of the brackets")
    bkts <- ordered(x, levels = levels(brackets))
  } else if (is.integer(x)) {
    assertthat::assert_that(all(x < nlevels(brackets)),
                            msg = "Argument to decode_age_bracket is greater than the number of brackets")
    bkts <- .covidABM$age_brackets$bracket[x]
  } else if (is.factor(x)) {
    assertthat::assert_that(all(as.character(x)) %in% levels(brackets),
                            msg = "Factor argument to decode_age_bracket doesn't match levels of brackets.")
    bkts <- ordered(x, levels = levels(brackets))
  } else {
    stop("Argument to decode_age_bracket is of unsupported type ", typeof(x),
         " class ", stringr::str_c(class(x), collapse = ", "), ".")
  }
  bkts
}
