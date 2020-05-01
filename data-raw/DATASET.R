## code to prepare `DATASET` dataset goes here
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(rlang)

seir_levels <- c("S" = 1, "E" = 2, "I" = 3, "R" = 4)

age_brackets <- tibble::tibble(lower = c(0, 20, 30, 40, 50, 60, 70, 80),
                       upper = c(lower, Inf) %>% lead() %>% head(-1),
                       bracket = seq_along(lower) %>%
                         ordered(labels = stringr::str_c(lower, upper,
                                                         sep = "-")))
age_brackets <- as.data.frame(age_brackets)

source("generate_test_probs.R")

trans_df <- create_transmission_cfg(NULL)
prog_df <- create_progression_cfg(NULL)
prog_df <- dplyr::mutate(prog_df,
                         transition = str_replace_all(compartment,
                                                      c("E" = "ei", "I" = "ir")))
prog_df <- dplyr::select(prog_df, -compartment)
prog_df <- tidyr::pivot_wider(prog_df, names_from = c(param, transition),
                              values_from = value)

trans_df <- as.data.table(trans_df)
prog_df <- as.data.table(prog_df)

saveRDS(trans_df, "transmission.Rds")
saveRDS(prog_df, "progression.Rds")

usethis::use_data(seir_levels, age_brackets, trans_df, prog_df,
                  internal = TRUE, overwrite = TRUE)
