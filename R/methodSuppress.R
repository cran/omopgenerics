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

#' Function to suppress counts in result objects
#'
#' @param result Result object
#' @param minCellCount Minimum count of records to report results.
#'
#' @return Table with suppressed counts
#'
#' @export
#'
suppress <- function(result,
                     minCellCount = 5) {
  UseMethod("suppress")
}

#' Function to suppress counts in result objects
#'
#' @param result summarised_result object.
#' @param minCellCount Minimum count of records to report results.
#'
#' @return summarised_result with suppressed counts.
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' library(omopgenerics)
#'
#' my_result <- tibble(
#'   "result_id" = "1",
#'   "cdm_name" = "mock",
#'   "result_type" = "summarised_characteristics",
#'   "package_name" = "omopgenerics",
#'   "package_version" = as.character(utils::packageVersion("omopgenerics")),
#'   "group_name" = "overall",
#'   "group_level" = "overall",
#'   "strata_name" = c(rep("overall", 6), rep("sex", 3)),
#'   "strata_level" = c(rep("overall", 6), "male", "female", "female"),
#'   "variable_name" = c("number records", "age_group", "age_group",
#'   "age_group", "age_group", "my_variable", "number records", "age_group",
#'   "age_group"),
#'   "variable_level" = c(NA, "<50", "<50", ">=50", ">=50", NA, NA,
#'   "<50", "<50"),
#'   "estimate_name" = c("count", "count", "percentage", "count", "percenatge",
#'   "random", "count", "count", "percentage"),
#'   "estimate_type" = c("integer", "integer", "percentage", "integer",
#'   "percentage", "numeric", "integer", "integer", "percentage"),
#'   "estimate_value" = c("10", "5", "50", "3", "30", "1", "3", "12", "6"),
#'   "additional_name" = "overall",
#'   "additional_level" = "overall"
#' )
#' my_result <- newSummarisedResult(my_result)
#' my_result |> glimpse()
#' my_result <- suppress(my_result, minCellCount = 5)
#' my_result |> glimpse()
#'
suppress.summarised_result <- function(result,
                                       minCellCount = 5) {

  # check if already suppressed
  set <- settings(result)
  if ("min_cell_count" %in% colnames(set)) {
    prevSupp <- unique(set |> dplyr::pull("min_cell_count")) |> as.numeric()
    if (prevSupp > minCellCount) {
      cli::cli_warn("Results passed are already obscured for counts smaller than {prevSupp}.")
      return(result)
    }
  }

  estimateName = "count"
  suppressed <- NA_character_

  # initial checks
  assertClass(
    result, class = c("tbl", "data.frame", "summarised_result"), all = TRUE
  )
  assertNumeric(minCellCount, integerish = TRUE, min = 0, length = 1)

  result <- result |>
    # obscured records
    obscureRecords(minCellCount, estimateName) |>
    # obscured records by group
    obscureGroup(minCellCount, estimateName) |>
    # obscure column
    obscureColumn(suppressed)

  # update settings
  set <- set |>
    dplyr::mutate("min_cell_count" = as.integer(.env$minCellCount))
  result <- newSummarisedResult(x = result, settings = set)

  return(result)
}

obscureRecords <- function(result, minCellCount, estimateName) {
  recordsToObscure <- result |>
    dplyr::filter(grepl(.env$estimateName, .data$estimate_name)) |>
    dplyr::mutate("estimate_value" = as.numeric(.data$estimate_value)) |>
    dplyr::filter(
      .data$estimate_value > 0 & .data$estimate_value < .env$minCellCount
    ) |>
    dplyr::mutate(
      "estimate_value" = as.character(.data$estimate_value),
      "obscure_record" = 1
    )
  result <- result |>
    dplyr::left_join(recordsToObscure, by = colnames(result)) |>
    dplyr::mutate(
      obscure_record = dplyr::if_else(is.na(.data$obscure_record), 0, 1)
    )
  return(result)
}
obscureGroup <- function(result, minCellCount, estimateName) {
  obsLabels <- result |>
    dplyr::select("variable_name") |>
    dplyr::distinct() |>
    dplyr::pull("variable_name")
  obsLabels <- obsLabels[tolower(gsub("_", " ", obsLabels)) %in% groupCount]

  groupsToObscure0 <- result |>
    dplyr::filter(
      .data$obscure_record == 1 & .data$estimate_name %in% .env$estimateName
    ) |>
    dplyr::select(!c(
      "estimate_name", "estimate_type", "estimate_value", "obscure_record"
    )) |>
    dplyr::mutate("obscure_group_0" = 1)
  cols0 <- colnames(groupsToObscure0)[colnames(groupsToObscure0) != "obscure_group_0"]
  groupsToObscure1 <- result |>
    dplyr::filter(
      .data$obscure_record == 1 & grepl(.env$estimateName, .data$estimate_name)
    ) |>
    dplyr::mutate("estimate_name" = gsub("count", "percentage", .data$estimate_name)) |>
    dplyr::select(-c("estimate_type", "estimate_value", "obscure_record")) |>
    dplyr::mutate("obscure_group_1" = 1)
  cols1 <- colnames(groupsToObscure1)[colnames(groupsToObscure1) != "obscure_group_1"]
  groupsToObscure2 <- result |>
    dplyr::filter(
      .data$variable_name %in% .env$obsLabels &
        grepl(.env$estimateName, .data$estimate_name) &
        .data$obscure_record == 1
    ) |>
    dplyr::select(!c(
      "variable_name", "variable_level", "estimate_name", "estimate_type",
      "estimate_value", "obscure_record"
    )) |>
    dplyr::distinct() |>
    dplyr::mutate("obscure_group_2" = 1)
  cols2 <- colnames(groupsToObscure2)[colnames(groupsToObscure2) != "obscure_group_2"]
  result <- result |>
    dplyr::left_join(groupsToObscure0, by = cols0) |>
    dplyr::left_join(groupsToObscure1, by = cols1) |>
    dplyr::left_join(groupsToObscure2, by = cols2) |>
    dplyr::mutate(obscure_group = dplyr::case_when(
      obscure_group_2 == 1 & !.data$variable_name %in% .env$groupCount ~ 1,
      obscure_group_1 == 1 & !grepl(.env$estimateName, .data$estimate_name) ~ 1,
      obscure_group_0 == 1 & !.data$estimate_name %in% .env$estimateName ~ 1,
      .default = 0
    )) |>
    dplyr::select(-c("obscure_group_0", "obscure_group_1", "obscure_group_2"))
  return(result)
}
obscureColumn <- function(result, suppressed) {
  result |>
    dplyr::mutate("estimate_value" = dplyr::if_else(
      .data$obscure_group == 1 | .data$obscure_record == 1,
      .env$suppressed,
      .data$estimate_value
    )) |>
    dplyr::select(-c("obscure_record", "obscure_group"))
}
