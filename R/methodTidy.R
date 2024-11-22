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

#' @importFrom generics tidy
#' @export
generics::tidy

#' Turn a `<summarised_result>` object into a tidy tibble
#'
#' @param x A `<summarised_result>`.
#' @param ... For compatibility (not used).
#'
#' @return A tibble.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Provides tools for obtaining a tidy version of a `<summarised_result>` object.
#' This tidy version will include the settings as columns, `estimate_value` will
#' be pivotted into columns using `estimate_name` as names, and group, strata,
#' and additional will be splitted.
#'
#' @export
#'
#' @examples {
#'   library(dplyr)
#'   library(omopgenerics)
#'
#'   x <- tibble(
#'     "result_id" = as.integer(c(1, 2)),
#'     "cdm_name" = c("cprd", "eunomia"),
#'     "group_name" = "cohort_name",
#'     "group_level" = "my_cohort",
#'     "strata_name" = "sex",
#'     "strata_level" = "male",
#'     "variable_name" = "Age group",
#'     "variable_level" = "10 to 50",
#'     "estimate_name" = "count",
#'     "estimate_type" = "numeric",
#'     "estimate_value" = "5",
#'     "additional_name" = "overall",
#'     "additional_level" = "overall"
#'   ) |>
#'     newSummarisedResult(settings = tibble(
#'       "result_id" = c(1, 2), "custom" = c("A", "B")
#'     ))
#'
#'   x
#'
#'   x |> tidy()
#' }
#'
tidy.summarised_result <- function(x, ...) {
  # checks
  if (length(list(...)) > 0) {
    "This function only accepts a summarised_result object as an input." |>
      cli::cli_warn()
  }

  resultTypes <- x |>
    settings() |>
    dplyr::filter(.data$result_id %in% unique(.env$x$result_id)) |>
    dplyr::pull("result_type") |>
    unique()
  if (length(resultTypes) > 1) {
    "Tidy is meant to work with a single result_type, but multiple result_types present: {.var {resultTypes}}." |>
      cli::cli_warn()
  }

  setNames <- settingsColumns(x)

  x <- x |>
    addSettings(settingsColumn = setNames) |>
    splitAll() |>
    pivotEstimates() |>
    dplyr::relocate(dplyr::all_of(setNames), .after = dplyr::last_col()) |>
    dplyr::select(!"result_id")

  return(x)
}
