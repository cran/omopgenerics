## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, message = FALSE, warning = FALSE,
  comment = "#>"
)

## ----echo=FALSE---------------------------------------------------------------
dplyr::tibble(
  `Column name` = omopgenerics::resultColumns(),
  `Column type` = c("integer", rep("character", 12)),
  `is NA allowed?` = c(rep("No", 7), "Yes", rep("No", 5)),
  `Requirements` = c(NA, NA, "name1", "level1", "name2", "level2", NA, NA, "snake_case", "estimateTypeChoices()", NA, "name3", "level3")
) |>
  gt::gt()

## -----------------------------------------------------------------------------
library(omopgenerics)
library(dplyr)

x <- tibble(
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
)

result <- newSummarisedResult(x)
result |>
  glimpse()
settings(result)

## -----------------------------------------------------------------------------
result <- newSummarisedResult(
  x = x,
  settings = tibble(
    result_id = 1L,
    package_name = "PatientProfiles",
    study = "my_characterisation_study"
  )
)

result |> glimpse()
settings(result)

## -----------------------------------------------------------------------------
result1 <- newSummarisedResult(
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

result2 <- newSummarisedResult(
  x = tibble(
    result_id = 1L,
    cdm_name = "my_cdm",
    group_name = "overall",
    group_level = "overall",
    strata_name = "overall",
    strata_level = "overall",
    variable_name = "overall",
    variable_level = "overall",
    estimate_name = "count",
    estimate_type = "numeric",
    estimate_value = "55",
    additional_name = "overall",
    additional_level = "overall"
  ),
  settings = tibble(
    result_id = 1L,
    package_name = "PatientProfiles",
    package_version = "1.0.0",
    study = "my_characterisation_study",
    result_type = "overall_analysis"
  )
)

## -----------------------------------------------------------------------------
result <- bind(result1, result2)
result |>
  dplyr::glimpse()
settings(result)

## -----------------------------------------------------------------------------
result |>
  suppress(minCellCount = 7) |>
  glimpse()

## -----------------------------------------------------------------------------
resultSuppressed <- result |> suppress(minCellCount = 10)
settings(result)
settings(resultSuppressed)

## -----------------------------------------------------------------------------
x <- tempdir()
files <- list.files(x)

exportSummarisedResult(result, path = x, fileName = "result.csv")
setdiff(list.files(x), files)

## ----echo=FALSE---------------------------------------------------------------
fil <- file.path(x, "result.csv")
readLines(fil) |>
  cat()

## -----------------------------------------------------------------------------
res <- importSummarisedResult(path = file.path(x, "result.csv"))
class(res)
res |>
  glimpse()
res |>
  settings()

## ----echo = FALSE-------------------------------------------------------------
dplyr::tibble(
  group_name = c("cohort_name", c("cohort_name &&& sex"), c("sex &&& age_group")),
  group_level = c("acetaminophen", c("acetaminophen &&& Female"), c("Male &&& <40"))
) |>
  gt::gt()

## ----echo = FALSE-------------------------------------------------------------
dplyr::tibble(
  group_name = c("cohort_name", c("cohort_name &&& sex"), c("sex &&& age_group")),
  group_level = c("acetaminophen", c("acetaminophen &&& Female"), c("Male &&& <40"))
) |>
  splitGroup() |>
  gt::gt()

## ----echo = FALSE-------------------------------------------------------------
dplyr::tibble(
  result_id = c(1L, 2L),
  my_setting = c(TRUE, FALSE),
  package_name = "omopgenerics"
) |>
  gt::gt()

## ----echo = FALSE-------------------------------------------------------------
dplyr::tibble(
  result_id = c("1", "...", "2", "..."),
  cdm_name = c("omop", "...", "omop", "..."),
  " " = c("..."),
  additional_name = c("overall", "...", "overall", "...")
) |>
  gt::gt()

## ----echo = FALSE-------------------------------------------------------------
dplyr::tibble(
  cdm_name = c("omop", "...", "omop", "..."),
  " " = c("..."),
  additional_name = c("overall", "...", "overall", "..."),
  my_setting = c("TRUE", "...", "FALSE", "..."),
  package_name = c("omopgenerics", "...", "omopgenerics", "...")
) |>
  gt::gt()

## ----echo = FALSE-------------------------------------------------------------
dplyr::tibble(
  variable_name = c("number individuals", "age", "age"),
  estimate_name = c("count", "mean", "sd"),
  estimate_type = c("integer", "numeric", "numeric"),
  estimate_value = c("100", "50.3", "20.7")
) |>
  gt::gt()

## ----echo = FALSE-------------------------------------------------------------
dplyr::tibble(
  variable_name = c("number individuals", "age"),
  count = c(100L, NA),
  mean = c(NA, 50.3),
  sd = c(NA, 20.7)
) |>
  gt::gt()

## -----------------------------------------------------------------------------
result |>
  tidy()

## -----------------------------------------------------------------------------
splitAll(result)

## -----------------------------------------------------------------------------
pivotEstimates(
  result,
  pivotEstimatesBy = c("variable_name", "variable_level", "estimate_name")
)

## -----------------------------------------------------------------------------
addSettings(
  result,
  settingsColumn = "result_type"
)

## -----------------------------------------------------------------------------
result |>
  filterStrata(sex == "male")

## -----------------------------------------------------------------------------
result |>
  filterSettings(result_type == "overall_analysis")

## -----------------------------------------------------------------------------
settingsColumns(result)
groupColumns(result)
strataColumns(result)
additionalColumns(result)
tidyColumns(result)

## -----------------------------------------------------------------------------
# Create and show mock data
data <- tibble(
  denominator_cohort_name = c("general_population", "older_than_60", "younger_than_60"),
  outcome_cohort_name = c("stroke", "stroke", "stroke")
)
head(data)

# Unite into group name-level columns
data |>
  uniteGroup(cols = c("denominator_cohort_name", "outcome_cohort_name"))

