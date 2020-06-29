build_transmission_params <- function(sex_factors, age_factors, sympt_factor,
                                      med_cond_factor) {
  trans_lst <- list(sex = c("M", "F"), age_bkt = seq(nrow(age_brackets)),
                    sympt = c(TRUE, FALSE), med_cond = c(TRUE, FALSE))
  list_len <- max(purrr::map_int(trans_lst, length))
  trans_lst <- purrr::map(trans_lst, ~rep_len(.x, list_len))
  trans_df <- tibble::as_tibble(trans_lst)
  trans_df <- tidyr::expand(trans_df, !!!rlang::syms(names(trans_df)))
  trans_df <- dplyr::distinct(trans_df)

  trans_df <- dplyr::mutate(trans_df,
                            mu_shed = ifelse(sex == "M", 0.0, -1.6) +
                              ifelse(sympt, 0.0, -1.8) +
                              dplyr::case_when(
                                age_bkt == 1 ~ -0.80,
                                age_bkt == 2 ~ -0.60,
                                age_bkt == 3 ~ -0.40,
                                age_bkt == 4 ~ -0.20,
                                age_bkt >= 5 ~  0.00,
                                TRUE ~ 0.00
                              ),
                            mu_susc = ifelse(sex == "M", 1.0, 0.75) +
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


