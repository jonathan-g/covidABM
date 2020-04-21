.onLoad <- function(libname, pkgname) {
  .covidABM <- new.env(parent = baseenv())
  assign("probs", probs, envir = .covidABM)
  assign("trans", trans, envir = .covidABM)
}
