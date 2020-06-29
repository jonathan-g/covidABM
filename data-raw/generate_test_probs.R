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
#' @export
generate_transmission_params <- function() {
  trans_lst <- list(sex = c("M", "F"), age_bkt = seq(nrow(age_brackets)),
                    sympt = c(TRUE, FALSE), med_cond = c(TRUE, FALSE))
  list_len <- max(purrr::map_int(trans_lst, length))
  trans_lst <- purrr::map(trans_lst, ~rep_len(.x, list_len))
  trans_df <- tibble::as_tibble(trans_lst)
  trans_df <- tidyr::expand(trans_df, !!!rlang::syms(names(trans_df)))
  trans_df <- dplyr::distinct(trans_df)

  mu_shed_0 <- 2.0
  mu_susc_0 <- 0.0

  trans_df <- dplyr::mutate(trans_df,
                            mu_shed = mu_shed_0 +
                              ifelse(sex == "M", 0.0, -1.6) +
                              ifelse(sympt, 0.0, -1.8) +
                              dplyr::case_when(
                                age_bkt == 1 ~ -0.80,
                                age_bkt == 2 ~ -0.60,
                                age_bkt == 3 ~ -0.40,
                                age_bkt == 4 ~ -0.20,
                                age_bkt >= 5 ~  0.00,
                                TRUE ~ 0.00
                              ),
                            mu_susc = mu_susc_0 +
                              ifelse(sex == "M", 1.0, 0.75) +
                              ifelse(med_cond, 2.0, 0.0) +
                              dplyr::case_when(
                                age_bkt == 1 ~ -2.00,
                                age_bkt == 2 ~ -1.50,
                                age_bkt == 3 ~ -1.00,
                                age_bkt == 4 ~ -0.50,
                                age_bkt == 5 ~ -0.00,
                                age_bkt == 6 ~  0.50,
                                age_bkt == 7 ~  1.50,
                                age_bkt >= 8 ~  2.00,
                                TRUE ~ 0.00
                              ),
                            sigma_shed = 2.0,
                            sigma_susc = 1.0
  )
  invisible(trans_df)
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
create_transmission_cfg <- function(file = "transmission.csv") {
  trans_df <- generate_transmission_params()
  if (! is.null(file)) {
    readr::write_csv(trans_df, file)
  }
  invisible(trans_df)
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
generate_progression_params <- function(pre_symptom_delta = 2,
                                        distr = c("gamma", "weibull"),
                                        mean_incubate = 5.4,
                                        min_incubate = 2.5, max_incubate = 11.5,
                                        mean_contagious = 5.0,
                                        min_contagious = 3, max_contagious = 8) {
  distr <- match.arg(distr)
  #
  # Parameters from
  # Lauer, S. A., et al. (2020).
  # The Incubation Period of Coronavirus Disease 2019 (COVID-19) From Publicly
  # Reported Confirmed Cases: Estimation and Application.
  # Annals of Internal Medicine. https://doi.org/10.7326/M20-0504
  #
  if (distr == "gamma") {
    shape_ei <- 5.807
  } else if (distr == "weibull") {
    shape_ei <- 2.453
  } else {
    stop("Unknown distribution for transmission parameters")
  }

  if (distr == "gamma") {
    scale_ei <- (mean_incubate - pre_symptom_delta) / shape_ei
  } else if (distr == "weibull") {
    scale_ei <- (mean_incubate - pre_symptom_delta) / gamma(1.0 +
                                                              1.0 / shape_ei)
  } else {
    stop("Unknown distribution for transmission parameters")
  }

  prog_lst <- list(sex = c("M", "F"), age_bkt = seq(nrow(age_brackets)),
                   sympt = c(TRUE, FALSE), med_cond = c(TRUE, FALSE))
  prog_lst <- purrr::map(prog_lst, ~rep_len(.x, 8))
  prog_df <- tibble::as_tibble(prog_lst)
  prog_df <- tidyr::expand(prog_df, !!!rlang::syms(names(prog_df)))
  prog_df <- dplyr::distinct(prog_df)

  e_df <- dplyr::mutate(prog_df,
                        shape = shape_ei,
                        scale = scale_ei,
                        compartment = "E"
  )
  e_df <- dplyr::distinct(e_df)


  shape_ir <- shape_ei

  if (distr == "gamma") {
    scale_ir <- mean_contagious / shape_ir
  } else if (distr == "weibull") {
    scale_ir <- mean_contagious / gamma(1.0 + 1.0 / shape_ir)
  } else {
    stop("Unknown distribution for transmission parameters")
  }

  i_df <- dplyr::mutate(prog_df,
                        shape = shape_ir,
                        scale = scale_ir,
                        compartment = "I"
  )
  i_df <- dplyr::distinct(i_df)

  prog_df <- dplyr::bind_rows(e_df, i_df)
  prog_df <- tidyr::pivot_longer(prog_df, cols = c("shape", "scale"),
                                 names_to = "param", values_to = "value")

  invisible(prog_df)
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
create_progression_cfg <- function(file = "progression.csv") {
  prog_df <- generate_progression_params()
  if (!is.null(file)) {
    readr::write_csv(prog_df, file)
  }
  invisible(prog_df)
}
