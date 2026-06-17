## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE, message = FALSE, warning = FALSE---------------------------
library(omopgenerics)
library(dplyr)

## -----------------------------------------------------------------------------
person <- tibble(
  person_id = c(1, 2),
  gender_concept_id = 0, year_of_birth = 1990,
  race_concept_id = 0, ethnicity_concept_id = 0
)
observation_period <- dplyr::tibble(
  observation_period_id = c(1, 2), person_id = c(1, 2),
  observation_period_start_date = as.Date("2000-01-01"),
  observation_period_end_date = as.Date("2021-12-31"),
  period_type_concept_id = 0
)
cdm <- cdmFromTables(
  tables = list(
    "person" = person,
    "observation_period" = observation_period
  ),
  cdmName = "example_cdm"
)
cdm

## -----------------------------------------------------------------------------
cohort <- tibble(
  cohort_definition_id = 1, subject_id = 1,
  cohort_start_date = as.Date("2020-01-01"),
  cohort_end_date = as.Date("2020-01-10")
)
cdm <- insertTable(cdm = cdm, name = "cohort", table = cohort)
cdm$cohort <- newCohortTable(cdm$cohort)

## -----------------------------------------------------------------------------
settings(cdm$cohort)

## -----------------------------------------------------------------------------
attrition(cdm$cohort)

## -----------------------------------------------------------------------------
cohortCount(cdm$cohort)

## -----------------------------------------------------------------------------
cdm$cohort <- cdm$cohort |>
  filter(cohort_start_date == as.Date("2019-01-01")) |>
  compute(name = "cohort", temporary = FALSE) |>
  recordCohortAttrition("Require cohort start January 1st 2019")
attrition(cdm$cohort)
cohortCount(cdm$cohort)

## -----------------------------------------------------------------------------
cohortCodelist(cdm$cohort, cohortId = 1, type = "index event")

## -----------------------------------------------------------------------------
cdm$cohort <- newCohortTable(cdm$cohort,
  cohortCodelistRef = dplyr::tibble(
    cohort_definition_id = c(1, 1),
    codelist_name = c("disease X", "disease X"),
    concept_id = c(101, 102),
    type = "index event"
  )
)
cohortCodelist(cdm$cohort, cohortId = 1, type = "index event")

## -----------------------------------------------------------------------------
asthma <- tibble(
  cohort_definition_id = 1, subject_id = 1,
  cohort_start_date = as.Date("2020-01-01"),
  cohort_end_date = as.Date("2020-01-10")
)
cdm <- insertTable(cdm, name = "asthma", table = asthma)
cdm$asthma <- newCohortTable(cdm$asthma,
  cohortSetRef = tibble(
    cohort_definition_id = 1,
    cohort_name = "asthma"
  )
)

copd <- tibble(
  cohort_definition_id = 1, subject_id = 2,
  cohort_start_date = as.Date("2020-01-01"),
  cohort_end_date = as.Date("2020-01-10")
)
cdm <- insertTable(cdm, name = "copd", table = copd)
cdm$copd <- newCohortTable(cdm$copd,
  cohortSetRef = tibble(
    cohort_definition_id = 1,
    cohort_name = "copd"
  )
)

cdm <- bind(cdm$asthma,
  cdm$copd,
  name = "exposures"
)
cdm$exposures

settings(cdm$exposures)
attrition(cdm$exposures)
cohortCount(cdm$exposures)

## -----------------------------------------------------------------------------
summary(cdm$exposures) |>
  glimpse()

