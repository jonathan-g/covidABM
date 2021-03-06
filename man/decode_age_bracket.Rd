% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constants.R
\name{decode_age_bracket}
\alias{decode_age_bracket}
\title{Translate text representation of an age bracket into an ordered factor}
\usage{
decode_age_bracket(x)
}
\arguments{
\item{x}{A character or factor vector containing one or more age bracket
descriptions equivalent to levels of the \code{age_brackets} variable.}
}
\value{
An ordered factor
}
\description{
Take a text representation of an age bracket, such as '30-40' and
translate it into an ordered factor compatible with the global
\code{age_brackets} variable.
}
\examples{
decode_age_bracket(c("0-19", "30-39"))
}
