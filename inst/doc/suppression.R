## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(omopgenerics, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

result <- newSummarisedResult(
  x = tibble(
    result_id = 1L,
    cdm_name = "my_cdm",
    group_name = "cohort_name",
    group_level = "cohort1",
    strata_name = "sex",
    strata_level = "male",
    variable_name = "Age group",
    variable_level = "10 to 50",
    estimate_name = "count",
    estimate_type = "numeric",
    estimate_value = "5",
    additional_name = "overall",
    additional_level = "overall"
  ),
  settings = tibble(
    result_id = 1L,
    package_name = "PatientProfiles",
    package_version = "1.0.0",
    study = "my_characterisation_study",
    result_type = "stratified_by_age_group"
  )
)

suppressedResult <- suppress(result = result, minCellCount = 7)

## -----------------------------------------------------------------------------
glimpse(settings(result))
glimpse(settings(suppressedResult))

## -----------------------------------------------------------------------------
isResultSuppressed(result = result, minCellCount = 5)
isResultSuppressed(result = suppressedResult, minCellCount = 5)
isResultSuppressed(result = suppressedResult, minCellCount = 7)
isResultSuppressed(result = suppressedResult, minCellCount = 10)

