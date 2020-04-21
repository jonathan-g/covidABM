
#' Run a model
#'
#' Run a model for a specified number of ticks, where each tick is a day.
#'
#' @param model A model object returned from [create_model].
#' @param ticks Number of days to simulate.
#'
#' @return A named list with the resulting model after the ticks, and
#'   a tibble with the counts of agents in "S", "E", "I", and "R" status.
#' @examples
#' # ADD_EXAMPLES_HERE
run_model <- function(model, ticks) {
  ts <- tibble(tick = 0,
               S = sum(V(model$nw)$seir == 1), E = sum(V(model$nw)$seir == 2),
               I = sum(V(model$nw)$seir == 3), R = sum(V(model$nw)$seir == 4))
  print(ts)
  for (i in 1:ticks) {
    model <- tick(model, probs, trans)
    row <- tibble(tick = i,
                  S = sum(V(model$nw)$seir == 1),
                  E = sum(V(model$nw)$seir == 2),
                  I = sum(V(model$nw)$seir == 3),
                  R = sum(V(model$nw)$seir == 4))
    ts <- bind_rows(ts, row)
    print(row)
  }
  invisible(list(model = model, history = ts))
}
