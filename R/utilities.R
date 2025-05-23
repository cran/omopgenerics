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

addClass <- function(x, value) {
  if (any(value %in% class(x))) x <- removeClass(x, value)
  base::class(x) <- c(value, base::class(x))
  return(x)
}
removeClass <- function(x, value) {
  base::class(x) <- base::class(x)[!(base::class(x) %in% value)]
  return(x)
}
getVocabularyVersion <- function(x) {
  vocabVersion <- NULL
  if ("vocabulary" %in% names(x) && "vocabulary_version" %in% colnames(x)) {
    vocabVersion <- x[["vocabulary"]] |>
      dplyr::filter(.data$vocabulary_id == "None") |>
      dplyr::pull(.data$vocabulary_version)
  }
  if (length(vocabVersion) == 0) {
    vocabVersion <- NA_character_
  }
  return(vocabVersion)
}

#' Convert a character vector to snake case
#'
#' @param x Character vector to convert
#'
#' @return A snake_case vector
#'
#' @export
#'
#' @examples
#' toSnakeCase("myVariable")
#'
#' toSnakeCase(c("cohort1", "Cohort22b"))
#'
toSnakeCase <- function(x) {
  iconv(x, from = "", to = "UTF-8",sub="") |>
  snakecase::to_snake_case(numerals = "asis")
}

#' Get the cohort definition id of a certain name
#'
#' @param cohort A cohort_table object.
#' @param cohortName Names of the cohort of interest. If NULL all cohort names
#' are shown.
#'
#' @return Cohort definition ids
#'
#' @export
#'
getCohortId <- function(cohort, cohortName = NULL) {
  # check inputs
  assertClass(cohort, "cohort_table")
  assertCharacter(cohortName, null = TRUE)

  set <- settings(cohort) |>
    dplyr::select("cohort_definition_id", "cohort_name")

  if (is.null(cohortName)) cohortName <- set$cohort_name

  notPresent <- cohortName[!cohortName %in% set$cohort_name]
  if (length(notPresent) > 0) {
    cli::cli_warn(c(
      "!" = "Cohorts names not found: {paste0(notPresent, collapse = ', ')}."
    ))
  }
  x <- dplyr::tibble("cohort_name" = cohortName) |>
    dplyr::inner_join(set, by = "cohort_name")
  x$cohort_definition_id |> rlang::set_names(x$cohort_name)
}

#' Get the cohort name of a certain cohort definition id
#'
#' @param cohort A cohort_table object.
#' @param cohortId Cohort definition id of interest. If NULL all cohort ids are
#' shown.
#'
#' @return Cohort names
#'
#' @export
#'
getCohortName <- function(cohort, cohortId = NULL) {
  # check inputs
  assertClass(cohort, "cohort_table")
  assertNumeric(cohortId, integerish = TRUE, null = TRUE)

  set <- settings(cohort) |>
    dplyr::select("cohort_definition_id", "cohort_name")

  if (is.null(cohortId)) cohortId <- set$cohort_definition_id

  notPresent <- cohortId[!cohortId %in% set$cohort_definition_id]
  if (length(notPresent) > 0) {
    cli::cli_warn(c(
      "!" = "Cohorts definition ids not found: {paste0(notPresent, collapse = ', ')}."
    ))
  }
  x <- dplyr::tibble("cohort_definition_id" = as.integer(cohortId)) |>
    dplyr::inner_join(set, by = "cohort_definition_id")
  x$cohort_name |> rlang::set_names(x$cohort_definition_id)
}

#' Get the column name with the person identifier from a table (either
#' subject_id or person_id), it will throw an error if it contains both or
#' neither.
#'
#' @param x A table.
#' @param call A call argument passed to cli functions.
#'
#' @export
#'
#' @return Person identifier column.
#'
getPersonIdentifier <- function(x, call = parent.frame()) {
  cols <- colnames(x)
  id <- c("person_id", "subject_id")
  id <- id[id %in% cols]
  if (length(id) == 2) {
    cli::cli_abort(
      message = "The table contains both person_id and subjet_id as columns",
      call = call
    )
  }
  if (length(id) == 0) {
    cli::cli_abort(
      message = "The table does not contain neither person_id nor subjet_id as columns",
      call = call
    )
  }
  return(id)
}

#' Get a unique Identifier with a certain number of characters and a prefix.
#'
#' @param n Number of identifiers.
#' @param exclude Columns to exclude.
#' @param nChar Number of characters.
#' @param prefix A prefix for the identifiers.
#'
#' @export
#'
#' @return A character vector with n unique identifiers.
#'
uniqueId <- function(n = 1, exclude = character(), nChar = 3, prefix = "id_") {
  # input check
  assertNumeric(n, integerish = TRUE, min = 0)
  assertCharacter(exclude)
  assertNumeric(n, integerish = TRUE, min = 1)
  assertCharacter(prefix, length = 1)

  if (nChar >= 5) {
    cli::cli_warn(c("!" = "if nChar >= 5 (nChar = {nChar}) it can be quite computationaly expensive"))
  }

  # get options for identifiers
  idOptions <- do.call(tidyr::expand_grid, rep(list(letters), nChar)) |>
    tidyr::unite(col = "id", dplyr::everything(), sep = "") |>
    dplyr::mutate("id" = paste0(.env$prefix, .data$id)) |>
    dplyr::filter(!.data$id %in% .env$exclude) |>
    dplyr::pull()

  if (length(idOptions) < n) {
    cli::cli_abort("There are not enough options with the current input parameters. {length(idOptions)} option{?s} and {n} requested id{?s}.")
  } else if (length(idOptions) == n) {
    x <- idOptions
  } else {
    x <- sample(idOptions, size = n)
  }

  return(x)
}

#' Check if a table is empty or not
#'
#' @param table a table
#'
#' @return Boolean to indicate if a cdm_table is empty (TRUE or FALSE).
#' @export
#'
isTableEmpty <- function(table) {
  assertClass(table, class = "cdm_table")

  x <- table |>
    dplyr::ungroup() |>
    utils::head(1) |>
    dplyr::tally() |>
    dplyr::pull() == 0

  return(x)
}

#' Return a table of omop cdm fields informations
#'
#' @param cdmVersion cdm version of the omop cdm.
#'
#' @return a tibble contain informations on all the different fields in omop cdm.
#' @export
#'
omopTableFields <- function(cdmVersion = "5.3") {
  assertChoice(cdmVersion, choices = names(fieldsTables))
  fieldsTables[[cdmVersion]]
}

#' Check if different packages version are used for summarise_results object
#'
#' @param result a summarised results object
#'
#' @return a summarised results object
#' @export
#'
resultPackageVersion <- function(result) {
  # initial checks
  validateResultArgument(result)

  # get sets
  x <- settings(result) |>
    dplyr::select("package_name", "package_version") |>
    dplyr::mutate(package_name = dplyr::if_else(
      .data$package_name == "", "no package associated", .data$package_name
    )) |>
    dplyr::distinct() |>
    dplyr::group_by(.data$package_name) |>
    dplyr::summarise(
      versions = paste0(.data$package_version, collapse = "; "),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      sym = dplyr::if_else(.data$n == 1, "v", "x"),
      msg = paste0("{.pkg ", .data$package_name, "}: ", .data$versions),
    )

  # warn if multiple versions
  if (max(x$n) > 1) {
    cli::cli_warn(c(
      "!" = "Multiple versions used for package{?s} {.pkg {x$package_name[x$n>1]}}.",
      "i" = "You can check the package_version with:",
      " " = "settings({.cls summarised_result})"
    ))
  }
  x$msg |>
    rlang::set_names(x$sym) |>
    cli::cli_inform()

  return(invisible(result))
}

#' Count the number of records that a `cdm_table` has.
#'
#' @param x A cdm_table.
#'
#' @return An integer with the number of records in the table.
#' @export
#'
#' @examples
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
#' numberRecords(cdm$observation_period)
#'
numberRecords <- function(x) {
  assertClass(x, "cdm_table")
  x |>
    dplyr::ungroup() |>
    dplyr::tally() |>
    dplyr::pull() |>
    as.integer()
}

#' Count the number of subjects that a `cdm_table` has.
#'
#' @param x A cdm_table.
#'
#' @return An integer with the number of subjects in the table.
#' @export
#'
#' @examples
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
#' numberSubjects(cdm$observation_period)
#'
numberSubjects <- function(x) {
  assertClass(x, "cdm_table")
  id <- getPersonIdentifier(x)
  x |>
    dplyr::ungroup() |>
    dplyr::summarise(n = dplyr::n_distinct(.data[[id]])) |>
    dplyr::pull() |>
    as.integer()
}
