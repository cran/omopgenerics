## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(omopgenerics)
omopTables()

## -----------------------------------------------------------------------------
omopColumns(table = "person", version = "5.3")

## -----------------------------------------------------------------------------
omopColumns(table = "observation_period", version = "5.3")

## -----------------------------------------------------------------------------
cohortColumns(table = "cohort", version = "5.3")
cohortColumns(table = "cohort_set", version = "5.3")
cohortColumns(table = "cohort_attrition", version = "5.3")

## -----------------------------------------------------------------------------
achillesTables()
achillesColumns("achilles_analysis")
achillesColumns("achilles_results")
achillesColumns("achilles_results_dist")

