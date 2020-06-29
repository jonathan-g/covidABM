
#' Run a model
#'
#' Run a model for a specified number of ticks, where each tick is a day.
#'
#' @param model A model object returned from [setup_model()].
#' @param ticks Number of days to simulate.
#' @param progress Print progress messages for every tick
#'   If it's `TRUE` or 1, a message is printed. If it's greater than 1
#'   additional messages from [tick()] are printed.
#' @param check_term Check populations in compartments and terminate early if
#'   there are no agents in either E or I (so no further infections can happen).
#' @return A named list with the resulting model after the ticks, and
#'   a tibble with the counts of agents in "S", "E", "I", and "R" status.
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
run_model <- function(model, ticks, progress = FALSE, check_term = TRUE) {
  progress <- as.integer(progress)
  verbose <- max(0, progress - 1)

  # Pacify R CMD check for non-standard evaluatino
  seir <- NULL
  age_bkt <- NULL

  ts <- model$agts[, list(tick = 0,
                          S = sum(seir == 1),
                          E = sum(seir == 2),
                          I = sum(seir == 3),
                          R = sum(seir == 4))]
  ts_bkt <-model$agts[, list(S = sum(seir == 1), E = sum(seir == 2),
                             I = sum(seir == 3), R = sum(seir == 4),
                             tick = 0),
                      by = age_bkt][order(age_bkt)]
  data.table::setcolorder(ts_bkt, c("tick", "age_bkt", "S", "E", "I", "R"))

  if (progress || .covidABM$tracing) {
    msg <- stringr::str_c(utils::head(ts, 1), names(.env$.), .env$.,
                          sep = " = ", collapse = ", ")
    message(msg)
  }
  for (i in 1:ticks) {
    model <- tick(model, tracing = verbose)
    row <- model$agts[, list(tick = i,
                             S = sum(model$agts$seir == 1),
                             E = sum(model$agts$seir == 2),
                             I = sum(model$agts$seir == 3),
                             R = sum(model$agts$seir == 4))]
    ts <- rbind(ts, row)
    row_bkt <-model$agts[, list(S = sum(seir == 1), E = sum(seir == 2),
                                I = sum(seir == 3), R = sum(seir == 4),
                                tick = i),
                         by = age_bkt][order(age_bkt)]
    data.table::setcolorder(row_bkt, c("tick", "age_bkt", "S", "E", "I", "R"))
    ts_bkt <- rbind(ts_bkt, row_bkt)
    if (progress || .covidABM$tracing) {
      msg <- stringr::str_c(utils::head(row, 1), names(.env$.), .env$.,
                            sep = " = ", collapse = ", ")
      message(msg)
    }
    if (row$E == 0 && row$I == 0) {
      if (progress || .covidABM$tracing) {
        message("No exposed or infected agents. Stopping.")
      }
      break
    }
  }
  invisible(list(model = model, history = ts, history2 = ts_bkt))
}
