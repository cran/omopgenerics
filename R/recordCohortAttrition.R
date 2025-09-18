# Copyright 2023 DARWIN EU (C)
#
# This file is part of omopgenerics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Update cohort attrition.
#'
#' @param cohort A cohort_table object.
#' @param reason A character string.
#' @param cohortId Cohort definition id of the cohort to update attrition. If
#' NULL all cohort_definition_id are updated.
#'
#' @export
#'
#' @return cohort_table with updated attrition.
#'
#' @examples
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' person <- tibble(
#'   person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
#'   race_concept_id = 0, ethnicity_concept_id = 0
#' )
#' observation_period <- tibble(
#'   observation_period_id = 1, person_id = 1,
#'   observation_period_start_date = as.Date("2000-01-01"),
#'   observation_period_end_date = as.Date("2023-12-31"),
#'   period_type_concept_id = 0
#' )
#' cohort <- tibble(
#'   cohort_definition_id = c(1, 1, 1, 2),
#'   subject_id = 1,
#'   cohort_start_date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01", "2022-01-01")),
#'   cohort_end_date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01", "2022-01-01")),
#' )
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "my_example_cdm",
#'   cohortTables = list("cohort1" = cohort)
#' )
#'
#' cdm$cohort1
#' attrition(cdm$cohort1)
#'
#' cdm$cohort1 <- cdm$cohort1 |>
#'   group_by(cohort_definition_id, subject_id) |>
#'   filter(cohort_start_date == min(cohort_start_date)) |>
#'   ungroup() |>
#'   compute(name = "cohort1", temporary = FALSE) |>
#'   recordCohortAttrition("Restrict to first observation")
#'
#' cdm$cohort1
#' attrition(cdm$cohort1)
#'
recordCohortAttrition <- function(cohort, reason, cohortId = NULL) {
  # check input
  assertClass(cohort, "cohort_table")
  assertCharacter(reason)
  cohortId <- validateCohortIdArgument(cohortId = cohortId, cohort = cohort)

  # eval reason as glue/cli expression if needed
  .envir <- parent.frame()
  reason <- reason |>
    purrr::map(\(x) {
      cli::cli_text(x, .envir = .envir) |>
        cli::cli_fmt() |>
        paste0(collapse = " ")
    }) |>
    purrr::flatten_chr()

  # validate lengths
  lc <- length(cohortId)
  lr <- length(reason)
  if (lc != lr) {
    if (lr == 1) {
      reason <- rep(reason, lc)
    } else {
      cli::cli_abort(c("x" = "Incompatible length of {.var cohortId} ({lc}) and {.var reason} ({lr})."))
    }
  }

  # updateAttrition
  newAttrition <- updateAttrition(cohort, cohortId, reason)

  # create cohort table
  cohort <- newCohortTable(
    table = cohort,
    cohortAttritionRef = newAttrition,
    .softValidation = TRUE
  )

  return(cohort)
}

updateAttrition <- function(cohort, cohortId, reason) {
  oldAttrition <- attrition(cohort)
  newRow <- oldAttrition |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::filter(.data$reason_id %in% max(.data$reason_id)) |>
    dplyr::select(
      "cohort_definition_id", "previous_records" = "number_records",
      "previous_subjects" = "number_subjects", "reason_id"
    ) |>
    dplyr::left_join(
      cohort |>
        dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
        dplyr::group_by(.data$cohort_definition_id) |>
        dplyr::summarise(
          "number_records" = dplyr::n(),
          "number_subjects" = dplyr::n_distinct(.data$subject_id),
          .groups = "drop"
        ) |>
        dplyr::collect(),
      by = "cohort_definition_id"
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c("number_records", "number_subjects")),
      \(x) dplyr::coalesce(as.integer(x), 0L)
    )) |>
    dplyr::mutate(
      "excluded_records" = .data$previous_records - .data$number_records,
      "excluded_subjects" = .data$previous_subjects - .data$number_subjects,
      "reason_id" = .data$reason_id + 1L
    ) |>
    dplyr::inner_join(
      dplyr::tibble("cohort_definition_id" = cohortId, "reason" = reason),
      by = "cohort_definition_id"
    ) |>
    dplyr::select(dplyr::all_of(cohortColumns("cohort_attrition")))
  newAttrition <- oldAttrition |>
    dplyr::bind_rows(newRow)
  return(newAttrition)
}
