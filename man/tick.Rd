% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tick.R
\name{tick}
\alias{tick}
\title{Advance model by one tick}
\usage{
tick(model, tracing = FALSE)
}
\arguments{
\item{model}{A model object, created by \code{\link[=setup_model]{setup_model()}}}

\item{tracing}{Whether to print informative messages about the results of
the tick. If it's \code{TRUE} or 1, a message is printed. If it's greater than 1
additional messages from \code{\link[=infect]{infect()}} are printed.}
}
\value{
An updated model
}
\description{
Advance model by one tick (i.e., one day). Calculate progress of disease
(exposed people become infectious, and infectious people recover) and then
calculate transmission of the disease (stochastically, infectious people may
infect susceptible people) through network connections.
}
\examples{
# ADD_EXAMPLES_HERE
}
