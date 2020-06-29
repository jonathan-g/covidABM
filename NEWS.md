# CovidABM 0.1.0.9000

Refactor to optimize for speed.

* Move from tibbles to data.tables
* Make primary representation of models a data.table instead of an igraph 
  object.
* Neighbors are now coded as columns in data table.
* Precompute probabilities, whether an agent will get symptoms, when an exposed 
  or infected agent will move to the next compartment

Other changes:

* Add multiple networks (social, home, and work)
* Change statistics of stochastic disease progression from E to I and I to R.

# covidABM 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
