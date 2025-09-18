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

#' Create an achilles table from a cdm_table.
#'
#' @param table A cdm_table.
#' @param version version of the cdm.
#' @param cast Whether to cast columns to the correct type.
#'
#' @return An achilles_table object
#'
#' @export
#'
newAchillesTable <- function(table, version = "5.3", cast = FALSE) {
  # create the structure
  assertClass(table, class = "cdm_table")
  assertLogical(cast, length = 1)
  table <- addClass(table, "achilles_table")

  .validateAchillesTable(table = table, version = version, cast = cast)
}

#' Create an empty achilles table
#'
#' @param cdm A cdm_reference to create the table.
#' @param name Name of the table to create.
#'
#' @export
#'
#' @return The cdm_reference with an achilles empty table
#'
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' cdm <- emptyCdmReference("my_example_cdm")
#' emptyAchillesTable(cdm = cdm, name = "achilles_results")
#' }
emptyAchillesTable <- function(cdm, name) {
  assertChoice(name, achillesTables(), length = 1)
  assertClass(cdm, "cdm_reference")
  table <- omopTableFields(cdmVersion(cdm)) |>
    dplyr::filter(
      .data$cdm_table_name == .env$name & .data$type == "achilles"
    ) |>
    emptyTable()
  cdm <- insertTable(cdm = cdm, name = name, table = table, overwrite = FALSE)
  cdm[[name]] <- cdm[[name]] |> newAchillesTable()
  return(cdm)
}

castAchillesColumns <- function(table, name, version) {
  cols <- omopTableFields(version) |>
    dplyr::filter(
      .data$type == "achilles" & .data$cdm_table_name == .env$name
    ) |>
    dplyr::select("cdm_field_name", "cdm_datatype") |>
    dplyr::mutate("cdm_datatype" = dplyr::case_when(
      stringr::str_detect(.data$cdm_datatype, "varchar") ~ "character",
      .data$cdm_datatype == "float" ~ "numeric",
      .data$cdm_datatype == "datetime" ~ "date",
      .default = .data$cdm_datatype
    )) |>
    # logical not working well in db currently
    dplyr::filter(.data$cdm_datatype != "logical")
  cols <- cols |>
    split(f = as.factor(cols$cdm_field_name)) |>
    lapply(dplyr::pull, "cdm_datatype")
  cast <- "local_cdm" %in% class(cdmSource(table))
  table <- castColumns(table, cols, name, cast)
  return(table)
}

#' Validate if a cdm_table is a valid achilles table.
#'
#' @param table A cdm_table to validate.
#' @param version The cdm vocabulary version.
#' @param cast Whether to cast columns to required type.
#' @param call Passed to cli call.
#'
#' @return invisible achilles table
#' @export
#'
validateAchillesTable <- function(table,
                                  version = NULL,
                                  cast = FALSE,
                                  call = parent.frame()) {
  assertClass(table, c("achilles_table", "cdm_table"))
  assertChoice(version, choices = supportedCdmVersions, null = TRUE, length = 1)
  assertLogical(cast, length = 1)
  if (is.null(version)) {
    version <- cdmVersion(table)
  }

  invisible(.validateAchillesTable(
    table = table, version = version, cast = cast, call = call
  ))
}

.validateAchillesTable <- function(table, version, cast, call) {
  name <- attr(table, "tbl_name")

  # validation
  if (!name %in% achillesTables(version = version)) {
    cli::cli_abort("{name} is not one of the achilles cdm tables.", call = call)
  }

  # check columns
  cols <- getColumns(
    table = name, version = version, type = "achilles", required = TRUE
  )
  checkColumnsCdm(table, name, cols, call = call)

  # cast colums
  if (cast) table <- castAchillesColumns(table, name, version)

  return(table)
}
