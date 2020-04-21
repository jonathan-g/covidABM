## code to prepare `DATASET` dataset goes here
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(rlang)

seir_levels <- c("S" = 1, "E" = 2, "I" = 3, "R" = 4)

age_brackets <- tibble(lower = c(0, 20, 30, 40, 50, 60, 70, 80),
                       upper = c(lower, Inf) %>% lead() %>% head(-1),
                       bracket = seq_along(lower) %>%
                         ordered(labels = str_c(lower, upper, sep = "-")))

source("generate_test_probs.R")
source("../R/probs.R")
source("../R/transitions.R")

prob_cfg <- create_prob_cfg(NULL)
trans_cfg <- create_transition_cfg(NULL)

probs <- build_prob_matrix(prob_cfg)

e_df <- trans_cfg %>% filter(compartment == "E") %>% select(-compartment)
i_df <- trans_cfg %>% filter(compartment == "I") %>% select(-compartment)
e_trans <- build_transition_matrix(e_df)
i_trans <- build_transition_matrix(i_df)
trans <- list(e = e_trans, i = i_trans)

saveRDS(probs, "probs.Rds")
saveRDS(trans, "trans.Rds")

usethis::use_data(seir_levels, age_brackets, probs, trans,
                  internal = TRUE, overwrite = TRUE)
