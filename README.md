
# covidABM

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![GitHub workflow status: R-CMD-check](https://github.com/jonathan-g/covidABM/workflows/R-CMD-check/badge.svg)](https://github.com/jonathan-g/covidABM/commits/master)
[![GitHub workflow status: pkgdown](https://github.com/jonathan-g/covidABM/workflows/pkgdown/badge.svg)](https://github.com/jonathan-g/covidABM/commits/master)
[![GitLab pipeline status](https://gitlab.jgilligan.org/covid-19-modeling/covidABM/badges/master/pipeline.svg)](https://gitlab.jgilligan.org/covid-19-modeling/covidABM/-/commits/master)
<!-- badges: end -->

covidABM is an R package that encapsulates a simple agent-based model of 
COVID-19 transmission.

The model is under active development and is currently in an early experimental
state. More detailed documnetation will appear as this package matures.

**CAUTION**: This model has not been calibrated or validated. It may have 
programming errors. It may make incorrect assumptions about the dynamics of
contagion. It **should not** be trusted for guiding health-care or public 
policy decisions.

Most importantly, the parameters the model uses by default, in 
`setup_test_model()` are wild guesses and are not based on any reliable 
data anlysis. 

Reference manual is available at 
<https://covid-19-modeling.pages.jgilligan.org/covidABM/reference/>
and
<https://jonathan-g.github.io/covidABM/reference/>
