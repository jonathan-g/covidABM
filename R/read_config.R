#' Read probabilities for disease transmission
#'
#' Read disease transmission probabilities from a file and transform to
#' a probability matrix.
#'
#' @param prob_file A `.csv` file with the transmission probabilities.
#'
#' @return An n-dimensional matrix of probabilities
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
read_probs <- function(prob_file = "probabilities.csv") {
  prob_df <- readr::read_csv(prob_file)
  probs <- build_prob_matrix(prob_df)
  invisible(probs)
}


#' Read parameters for disease progression
#'
#' Read parameters for disease progression and create transition probability
#' matrices for transitions from exposed to infectious and from infectious to
#' recovered. The parameters characterize the mean and scale of the
#' distribution (in days).
#'
#' @param trans_file A `.csv` file with the transition probabilities.
#'
#' @return A named list containing two n-dimensional matrices of parameters
#'   (with elements `e` for exposed to infectious transitions and `i` for
#'   infectious to recovered transitions).
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
read_trans <- function(trans_file = "transitions.csv") {
  trans_df <- readr::read_csv(trans_file)
  e_df <- trans_df %>% dplyr::filter(.data$compartment == "E") %>%
    dplyr::select(-.data$compartment)
  i_df <- trans_df %>% dplyr::filter(.data$compartment == "I") %>%
    dplyr::select(-.data$compartment)
  e_trans <- build_transition_matrix(e_df)
  i_trans <- build_transition_matrix(i_df)
  invisible(list(e = e_trans, i = i_trans))
}
