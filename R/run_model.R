
#' Run a model
#'
#' Run a model for a specified number of ticks, where each tick is a day.
#'
#' @param model A model object returned from [setup_model()].
#' @param ticks Number of days to simulate.
#' @param progress Print progress messages for every tick
#'   If it's `TRUE` or 1, a message is printed. If it's greater than 1
#'   additional messages from [tick()] are printed.
#'
#' @return A named list with the resulting model after the ticks, and
#'   a tibble with the counts of agents in "S", "E", "I", and "R" status.
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
run_model <- function(model, ticks, progress = FALSE) {
  progress <- as.integer(progress)
  verbose <- max(0, progress - 1)
  ts <- tibble::tibble(tick = 0,
               S = sum(igraph::V(model$nw)$seir == 1),
               E = sum(igraph::V(model$nw)$seir == 2),
               I = sum(igraph::V(model$nw)$seir == 3),
               R = sum(igraph::V(model$nw)$seir == 4))
  if (progress || .covidABM$tracing) {
    utils::head(ts, 1) %>%
      stringr::str_c(names(.env$.), .env$., sep = " = ", collapse = ", ") %>%
      message()
  }
  for (i in 1:ticks) {
    model <- tick(model, tracing = verbose)
    row <- tibble::tibble(tick = i,
                  S = sum(igraph::V(model$nw)$seir == 1),
                  E = sum(igraph::V(model$nw)$seir == 2),
                  I = sum(igraph::V(model$nw)$seir == 3),
                  R = sum(igraph::V(model$nw)$seir == 4))
    ts <- dplyr::bind_rows(ts, row)
    if (progress || .covidABM$tracing) {
      utils::head(row, 1) %>%
        stringr::str_c(names(.env$.), .env$., sep = " = ", collapse = ", ") %>%
        message()
    }
  }
  invisible(list(model = model, history = ts))
}
