% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agent.R
\name{set_agent_probs}
\alias{set_agent_probs}
\title{FUNCTION_TITLE}
\usage{
set_agent_probs(agents, transmission_params, progression_params)
}
\arguments{
\item{agents}{A data frame describing agents, with columns:
\itemize{
\item \code{age}: Age in years (numeric).
\item \code{age_bkt}: The age bracket (ordered factor).
\item \code{sex}: The sex (factor with levels "M" and "F").
\item \code{med_cond}: Has a medical condition (logical).
\item \code{sympt}: Has symptoms of COVID-19, if infectious (logical).
\item \code{seir}: SEIR status (ordered factor with levels "S", "E", "I", and "R").
\item \code{id}: Unique ID number (integer)
\item \code{ticks}: days since reaching current disease stage (relevant to "E" and
"I" status) (integer).
\item \code{target}: target ticks for transition to next compartment (relevant to
"E" and "I" status only).
}}

\item{transmission_params}{A data frame of parameters for disease
transmission probability distributions. Columns are:
\itemize{
\item \code{age_bkt}: The age bracket (ordered factor).
\item \code{sex}: The sex (factor with levels "M" and "F").
\item \code{med_cond}: Has a medical condition (logical).
\item \code{sympt}: Has symptoms of COVID-19, if infectious (logical).
\item \code{mu_shed}: Mean of the shedding parameter distribution.
\item \code{sigma_shed}: Scale (standard deviation) of the shedding parameter
distribution.
\item \code{mu_susc}: Mean of the susceptibility parameter distribution
\item \code{sigma_susc}: Scale (standard deviation) of the susceptibility parameter
distribution.
}}

\item{progression_params}{A data frame of parameters for disease
progression probability distributions. Columns are:
\itemize{
\item \code{age_bkt}: The age bracket (ordered factor).
\item \code{sex}: The sex (factor with levels "M" and "F").
\item \code{med_cond}: Has a medical condition (logical).
\item \code{sympt}: Has symptoms of COVID-19, if infectious (logical).
\item \code{shape_ei}: Shape parameter for progression from "E" to "I" compartment.
\item \code{scale_ei}: Scale parameter for progression from "E" to "I" compartment.
\item \code{shape_ir}: Shape parameter for progression from "I" to "R" compartment.
\item \code{scale_ir}: Scale parameter for progression from "I" to "R" compartment.
}}
}
\value{
A \code{data.table} with transmission probabilities and progression
parameters for each agent. Columns are:
\itemize{
\item \code{age}: Age in years (numeric).
\item \code{age_bkt}: The age bracket (ordered factor).
\item \code{sex}: The sex (factor with levels "M" and "F").
\item \code{med_cond}: Has a medical condition (logical).
\item \code{sympt}: Has symptoms of COVID-19, if infectious (logical).
\item \code{seir}: SEIR status (ordered factor with levels "S", "E", "I", and "R").
\item \code{id}: Unique ID number (integer)
\item \code{ticks}: days since reaching current disease stage (relevant to "E" and
"I" status) (integer).
\item \code{target}: target ticks for transition to next compartment (relevant to
"E" and "I" status only).
\item \code{x_shed}: Shedding parameter.
\item \code{x_susc}: Susceptibility parameter.
\item \code{shape_ei}: Shape parameter for progression from "E" to "I" compartment.
\item \code{scale_ei}: Scale parameter for progression from "E" to "I" compartment.
\item \code{shape_ir}: Shape parameter for progression from "I" to "R" compartment.
\item \code{scale_ir}: Scale parameter for progression from "I" to "R" compartment.
}
}
\description{
FUNCTION_DESCRIPTION
}
\examples{
# ADD_EXAMPLES_HERE
}
\keyword{internal}
