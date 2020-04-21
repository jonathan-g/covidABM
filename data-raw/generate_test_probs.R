#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param r0 DESCRIPTION.
#' @param contacts DESCRIPTION.
#' @param days_contagious DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
generate_probs <- function(r0 = 2.3, contacts = 5, days_contagious = 14) {
  prob_df <- list(agt_sex = c("M", "F"), agt_age_bkt = seq(nrow(age_brackets)),
                  agt_sympt = c(TRUE, FALSE),
                  sub_age_bkt = seq(nrow(age_brackets)),
                  sub_sex = c("M", "F"), sub_med_cond = c(TRUE, FALSE)) %>%
    purrr::map(~rep_len(.x, 8)) %>%tibble::as_tibble() %>%
    tidyr::expand(!!!rlang::syms(names(.))) %>%
    distinct()

  prob_df <- prob_df %>% dplyr::mutate(shed = ifelse(agt_sex == "M", 1.0, 0.75) *
                                  ifelse(agt_sympt, 1.0, 0.52) *
                                  case_when(
                                    agt_age_bkt == 1 ~ 0.50,
                                    agt_age_bkt == 2 ~ 0.60,
                                    agt_age_bkt == 3 ~ 0.75,
                                    agt_age_bkt == 4 ~ 0.90,
                                    agt_age_bkt >= 5 ~ 1.00,
                                    TRUE ~ 1.00
                                  ),
                                suscept = ifelse(sub_sex == "M", 1.0, 0.75) *
                                  ifelse(sub_med_cond, 1.0, 0.75) *
                                  case_when(
                                    sub_age_bkt == 1 ~ 0.50,
                                    sub_age_bkt == 2 ~ 0.75,
                                    sub_age_bkt == 3 ~ 0.80,
                                    sub_age_bkt == 4 ~ 0.85,
                                    sub_age_bkt == 5 ~ 0.90,
                                    sub_age_bkt == 6 ~ 0.96,
                                    sub_age_bkt == 7 ~ 0.98,
                                    sub_age_bkt >= 8 ~ 1.00,
                                    TRUE ~ 1.00
                                  ),
                                prob = shed * suscept)

  p_norm <- r0 / (contacts * days_contagious)

  p_mean <- mean(prob_df$prob)
  prob_df <- prob_df %>% dplyr::select(-shed, -suscept) %>%
    dplyr::mutate(prob = prob * p_norm / p_mean)

  invisible(prob_df)
}


#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param file DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
create_prob_cfg <- function(file = "probabilities.csv") {
  pdf <- generate_probs()
  if (! is.null(file)) {
    readr::write_csv(pdf, file)
  }
  invisible(pdf)
}


#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param min_incubate DESCRIPTION.
#' @param max_incubate DESCRIPTION.
#' @param min_contagious DESCRIPTION.
#' @param max_contagious DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
generate_transitions <- function(min_incubate = 2.5, max_incubate = 11.5,
                                 min_contagious = 10, max_contagious = 18) {
  df <- list(sex = c("M", "F"), age_bkt = seq(nrow(age_brackets)),
             sympt = c(TRUE, FALSE), med_cond = c(TRUE, FALSE)) %>%
    purrr::map(~rep_len(.x, 8)) %>%tibble::as_tibble() %>%
    tidyr::expand(!!!rlang::syms(names(.))) %>%
    distinct()

  x0 <- (min_incubate + max_incubate) / 2
  x_low <- x0 - qlogis(0.025, x0, 1.0)
  s <- (x0 - min_incubate) / x_low

  e_df <- df %>% dplyr::mutate(loc = ifelse(sex == "M", x0, x0) *
                          case_when(
                            age_bkt == 1 ~ 1.3,
                            age_bkt == 2 ~ 1.2,
                            age_bkt == 3 ~ 1.1,
                            age_bkt >= 4 ~ 1.0,
                            TRUE ~ 1.0
                          ) -
                          ifelse(med_cond, 1.0, 0.0),
                        scale = s )

  x0 <- (min_contagious + max_contagious) / 2
  x_low <- x0 - qlogis(0.025, x0, 1.0)
  s <- (x0 - min_contagious) / x_low

  i_df <- df %>% dplyr::mutate(loc = ifelse(sex == "M", x0, x0) *
                          case_when(
                            age_bkt == 1 ~ 0.8,
                            age_bkt == 2 ~ 1.0,
                            age_bkt == 3 ~ 1.0,
                            age_bkt == 4 ~ 1.1,
                            age_bkt >= 5 ~ 1.2,
                            TRUE ~ 1.0
                          ) *
                          ifelse(med_cond, 1.25, 1.00),
                        scale = s)

  e_df <- e_df %>% dplyr::mutate(compartment = "E")

  i_df <- i_df %>% dplyr::mutate(compartment = "I")

  df <- dplyr::bind_rows(e_df, i_df) %>%
    tidyr::pivot_longer(cols = c("loc", "scale"), names_to = "param",
                 values_to = "value")

  invisible(df)
}


#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param file DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
create_transition_cfg <- function(file = "transitions.csv") {
  trans_df <- generate_transitions()
  if (!is.null(file)) {
    readr::write_csv(trans_df, file)
  }
  invisible(trans_df)
}
