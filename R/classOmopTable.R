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

#' Create an omop table from a cdm table.
#'
#' @param table A cdm_table.
#' @param version version of the cdm.
#' @param cast Whether to cast columns to the correct type.
#'
#' @return An omop_table object
#'
#' @export
#'
newOmopTable <- function(table, version = "5.3", cast = FALSE) {
  # create the structure
  assertClass(table, class = "cdm_table", msg = "table must be a cdm_table")
  table <- addClass(table, "omop_table")

  .validateOmopTable(table = table, version = version, cast = cast)
}

#' Validate an omop_table
#'
#' @param omopTable An omop_table to check.
#' @param version The version of the cdm.
#' @param cast Whether to cast columns to the correct type.
#' @param call Call argument that will be passed to `cli` error message.
#'
#' @return An omop_table object.
#' @export
#'
validateOmopTable <- function(omopTable,
                              version = NULL,
                              cast = FALSE,
                              call = parent.frame()) {
  assertClass(omopTable, c("omop_table", "cdm_table"))
  assertChoice(version, choices = supportedCdmVersions, null = TRUE, length = 1)
  assertLogical(cast, length = 1)
  if (is.null(version)) {
    version <- cdmVersion(omopTable)
  }

  invisible(.validateOmopTable(
    table = omopTable, version = version, cast = cast, call = call
  ))
}

.validateOmopTable <- function(table, version, cast, call = parent.frame()) {
  name <- attr(table, "tbl_name")

  # validation
  if (!attr(table, "tbl_name") %in% tableChoice(version = version, type = "cdm_table")) {
    cli::cli_abort("{name} is not one of the omop cdm standard tables.", call = call)
  }

  cols <- getColumns(
    table = name, version = version, type = "cdm_table", required = TRUE
  )
  checkColumnsCdm(table, name, cols, call = call)
  if (cast) table <- castOmopColumns(table, name, version)

  return(table)
}

#' Create an empty omop table
#'
#' @param cdm A cdm_reference to create the table.
#' @param name Name of the table to create.
#'
#' @export
#'
#' @return The cdm_reference with an empty cohort table
#'
#' @examples
#' library(omopgenerics)
#'
#' person <- dplyr::tibble(
#'   person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
#'   race_concept_id = 0, ethnicity_concept_id = 0
#' )
#' observation_period <- dplyr::tibble(
#'   observation_period_id = 1, person_id = 1,
#'   observation_period_start_date = as.Date("2000-01-01"),
#'   observation_period_end_date = as.Date("2023-12-31"),
#'   period_type_concept_id = 0
#' )
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "test"
#' )
#'
#' cdm <- emptyOmopTable(cdm, "drug_exposure")
#'
#' cdm$drug_exposure
#'
emptyOmopTable <- function(cdm, name) {
  assertChoice(name, omopTables(), length = 1)
  assertClass(cdm, "cdm_reference")
  table <- emptyOmopTableInternal(name = name, version = cdmVersion(cdm))
  cdm <- insertTable(cdm = cdm, name = name, table = table, overwrite = FALSE)
  cdm[[name]] <- newOmopTable(cdm[[name]])
  return(cdm)
}

castOmopColumns <- function(table, name, version) {
  cols <- omopTableFields(version) |>
    dplyr::filter(
      .data$type == "cdm_table" & .data$cdm_table_name == .env$name
    ) |>
    dplyr::select("cdm_field_name", "cdm_datatype") |>
    dplyr::mutate("cdm_datatype" = dplyr::case_when(
      stringr::str_detect(.data$cdm_datatype, "varchar") ~ "character",
      .data$cdm_datatype == "float" ~ "numeric",
      .data$cdm_datatype == "datetime" ~ "date",
      .default = .data$cdm_datatype
    ))
  cols <- cols |>
    split(f = as.factor(cols$cdm_field_name)) |>
    lapply(dplyr::pull, "cdm_datatype")
  table <- castColumns(table, cols, name)
  return(table)
}
