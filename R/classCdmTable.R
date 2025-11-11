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

#' Create an cdm table.
#'
#' @param table A table that is part of a cdm.
#' @param src The source of the table.
#' @param name The name of the table.
#'
#' @return A cdm_table object
#'
#' @export
#'
newCdmTable <- function(table, src, name) {
  assertClass(src, class = "cdm_source")
  assertCharacter(name, length = 1, na = TRUE)

  table <- structure(.Data = table, tbl_source = src, tbl_name = name) |>
    addClass("cdm_table")

  validateCdmTable(table)

  return(table)
}

#' Validate if a table is a valid cdm_table object.
#'
#' @param table Object to validate.
#' @param name If we want to validate that the table has a specific name.
#' @param call Call argument that will be passed to `cli`.
#'
#' @return The table or an error message.
#' @export
#'
validateCdmTable <- function(table,
                             name = NULL,
                             call = parent.frame()) {
  # class
  if (!inherits(table, "cdm_table")) {
    cli::cli_abort("`table` must be a {.cls cdm_table} object.", call = call)
  }

  # attributes
  notPresent <- c("tbl_name", "tbl_source")
  notPresent <- notPresent[!notPresent %in% names(attributes(table))]
  if (length(notPresent) > 0) {
    "The following attributes were not found in cdm table: {.pkg {notPresent}}." |>
      cli::cli_abort(call = call)
  }

  # columns
  colUpper <- setdiff(colnames(table), tolower(colnames(table)))
  if (length(colUpper) > 0) {
    "The following columns are upper case which is not permited: {.var {colUpper}}." |>
      cli::cli_abort(call = call)
  }

  # name
  if (!is.null(name)) {
    nm <- attr(table, "tbl_name")
    if (!identical(nm, name)) {
      "Wrong name attribute for {.cls cdm_table}, expected: {name}, is: {nm}." |>
        cli::cli_abort(call = call)
    }
  }

  return(table)
}

#' Get the `cdm_reference` of a `cdm_table`.
#'
#' @param table A cdm_table.
#'
#' @return A cdm_reference.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' cdmReference(cdm$person)
#' }
cdmReference <- function(table) {
  assertClass(table, "cdm_table")
  attr(table, "cdm_reference")
}

#' Get the table name of a `cdm_table`.
#'
#' @param table A cdm_table.
#'
#' @return A character with the name.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' tableName(cdm$person)
#' }
tableName <- function(table) {
  assertClass(table, "cdm_table",
    msg = "`table` does not have the class: cdm_table"
  )
  attr(table, "tbl_name")
}

#' Get the table source of a `cdm_table`.
#'
#' @param table A cdm_table.
#'
#' @return A cdm_source object.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' tableSource(cdm$person)
#' }
tableSource <- function(table) {
  assertClass(table, "cdm_table",
    msg = "`table` does not have the class: cdm_table"
  )
  attr(table, "tbl_source")
}

#' @export
#' @importFrom dplyr collect
collect.cdm_table <- function(x, ...) {
  x <- removeClass(x, "cdm_table")
  if (any(colnames(x) != tolower(colnames(x)))) {
    # TO CHANGE TO ERROR IN NEW RELEASE
    cli::cli_warn(c("!" = "A cdm_table must have lowercase column names."))
  }
  x <- dplyr::collect(x)
  attr(x, "tbl_name") <- NULL
  attr(x, "tbl_source") <- NULL
  attr(x, "cdm_reference") <- NULL

  # check character columns
  x <- x |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(is.character),
      .fns = \(x) iconv(x = x, from = "", to = "UTF-8", sub = "")
    ))

  return(x)
}

noReference <- function(x) {
  attr(x, "cdm_reference") <- NULL
  return(x)
}
