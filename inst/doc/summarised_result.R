## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, message = FALSE, warning = FALSE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(omopgenerics)
library(dplyr)

x <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "my_cdm",
    "group_name" = "sex",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = "Age group",
    "variable_level" = "10 to 50",
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )

result <- newSummarisedResult(x)
result |> 
  dplyr::glimpse()

## -----------------------------------------------------------------------------
result <- newSummarisedResult(x, 
                    settings = dplyr::tibble(result_id = 1,
                                             package = "PatientProfiles",
                                             study = "my_characterisation_study"))

result |> glimpse()
settings(result)

## -----------------------------------------------------------------------------
result_1 <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "my_cdm",
    "group_name" = "sex",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = "Age group",
    "variable_level" = "10 to 50",
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
result_1_settings <- dplyr::tibble(result_id = 1,
                                   package = "PatientProfiles",
                                   study = "my_characterisation_study",
                                   analyis = "stratified by age_group")
result_1 <- newSummarisedResult(result_1, settings = result_1_settings)


result_2 <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "my_cdm",
    "group_name" = "overall",
    "group_level" = "overall",
    "strata_name" = "overall",
    "strata_level" = "overall",
    "variable_name" = "overall",
    "variable_level" = "overall",
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "55",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
result_2_settings <- dplyr::tibble(result_id = 1,
                                   package = "PatientProfiles",
                                   study = "my_characterisation_study",
                                   analyis = "overall analysis")
result_2 <- newSummarisedResult(result_2, settings = result_2_settings)

## -----------------------------------------------------------------------------
result <- bind(list(result_1, result_2))
result |> 
  dplyr::glimpse()
settings(result)

## -----------------------------------------------------------------------------
suppress(result, minCellCount = 7) |> 
  glimpse()

