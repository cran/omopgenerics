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
#' @inheritParams cohortDoc
#' @param cohortName Names of the cohort of interest or a tidyselect expression
#' referring to cohort names. If NULL all cohort names are shown.
#'
#' @return Cohort definition ids
#'
#' @export
#'
getCohortId <- function(cohort, cohortName = NULL) {
  cohortName <- rlang::enquo(cohortName)
  # check inputs
  assertClass(cohort, "cohort_table")

  set <- settings(cohort) |>
    dplyr::select("cohort_definition_id", "cohort_name")

  type <- selectType(cohortName)
  if (type == "tidy") {
    cohortName <- tidySelect(set$cohort_name, cohortName)
  } else if (type == "character") {
    cohortName <- rlang::eval_tidy(cohortName)
  } else if (type == "NULL") {
    cohortName <- set$cohort_name
  } else {
    cli::cli_abort(c(x = "`cohortName` can not be numeric."))
  }

  notPresent <- cohortName[!cohortName %in% set$cohort_name]
  if (length(notPresent) > 0) {
    cli::cli_warn(c(
      "!" = "Cohorts names not found: {paste0(notPresent, collapse = ', ')}."
    ))
  }
  cohortName <- cohortName[cohortName %in% set$cohort_name]
  x <- dplyr::tibble("cohort_name" = cohortName) |>
    dplyr::inner_join(set, by = "cohort_name")
  x$cohort_definition_id |> rlang::set_names(x$cohort_name)
}

#' Get the cohort name of a certain cohort definition id
#'
#' @inheritParams cohortDoc
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
#' @inheritParams cliCallDoc
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
      message = "The table contains both person_id and subject_id as columns",
      call = call
    )
  }
  if (length(id) == 0) {
    cli::cli_abort(
      message = "The table contains neither person_id nor subject_id as columns",
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
  assertNumeric(n, integerish = TRUE, min = 0, length = 1)
  assertCharacter(exclude, empty = TRUE)
  assertNumeric(nChar, integerish = TRUE, min = 1, length = 1)
  assertCharacter(prefix, length = 1)

  if (n == 0) {
    return(character())
  }

  if (nChar >= 5) {
    cli::cli_warn(c("!" = "if nChar >= 5 (nChar = {nChar}) it can be quite computationally expensive"))
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
  assertClass(table, class = "cdm_table", empty = TRUE)

  x <- table |>
    dplyr::ungroup() |>
    utils::head(1) |>
    dplyr::tally() |>
    dplyr::pull() == 0

  return(x)
}

joinCohortNameFromSettings <- function(x, cohort) {
  return(x)

  # TODO reverse dependencies needs to be fixed before applying this

  cohortNames <- settings(cohort) |>
    dplyr::select("cohort_definition_id", "cohort_name")

  x |>
    dplyr::select(-dplyr::any_of("cohort_name")) |>
    dplyr::left_join(cohortNames, by = "cohort_definition_id") |>
    dplyr::relocate("cohort_name", .after = "cohort_definition_id")
}

#' Return a table of omop cdm field information
#'
#' @param cdmVersion cdm version of the omop cdm.
#'
#' @return A tibble containing information on all the different fields in omop cdm.
#' @export
#'
omopTableFields <- function(cdmVersion = "5.3") {
  assertChoice(cdmVersion, choices = omopgenerics::supportedCdmVersions, length = 1)
  fieldsTables[[cdmVersion]]
}

#' Compare the fields of two different OMOP CDM versions
#'
#' @param cdmVersionReference An OMOP CDM version, it can either be "5.3" or
#' "5.4".
#' @param cdmVersionComparator An OMOP CDM version, it can either be "5.3" or
#' "5.4".
#'
#' @returns A tibble with two columns: `field` and `change`.
#' @export
#'
#' @examples
#' library(omopgenerics)
#'
#' compareOmopTableFields("5.3", "5.4")
#'
compareOmopTableFields <- function(cdmVersionReference = "5.3",
                                   cdmVersionComparator = "5.4") {
  assertChoice(cdmVersionReference, choices = omopgenerics::supportedCdmVersions, length = 1)
  assertChoice(cdmVersionComparator, choices = omopgenerics::supportedCdmVersions, length = 1)

  # get changes
  if (cdmVersionReference == cdmVersionComparator) {
    change <- "same"
  } else {
    change <- omopgenerics::supportedCdmVersions |>
      purrr::keep(\(x) x %in% c(cdmVersionReference, cdmVersionComparator)) |>
      paste0(collapse = " to ")
  }
  changes <- fieldsChanges[[change]]

  # invert if needed
  if (change != paste0(cdmVersionReference, " to ", cdmVersionComparator)) {
    changes <- invertChanges(changes)
  }

  # add class for the print
  changes <- addClass(changes, "compare_table_fields")
  attr(changes, "cdm_version_reference") <- cdmVersionReference
  attr(changes, "cdm_version_comparator") <- cdmVersionComparator

  return(changes)
}
invertChanges <- function(changes) {
  changes |>
    dplyr::mutate(
      original_change = .data$change,
      change = dplyr::case_when(
        .data$original_change == "eliminated table" ~ "new table",
        .data$original_change == "eliminated field" ~ "new field",
        .data$original_change == "new table" ~ "eliminated table",
        .data$original_change == "new field" ~ "eliminated field",
        .default = paste0("changed from: ", .data$field)
      ),
      field = dplyr::if_else(
        stringr::str_starts(string = .data$original_change, pattern = "changed from: "),
        stringr::str_sub(string = .data$original_change, start = 15, end = -1),
        .data$field
      )
    ) |>
    dplyr::select("field", "change")
}

#' @export
print.compare_table_fields <- function(x, ...) {
  ref <- attr(x, "cdm_version_reference") %||% "unknown"
  com <- attr(x, "cdm_version_comparator") %||% "unknown"
  eliminatedTables <- x |>
    dplyr::filter(.data$change == "eliminated table") |>
    getFields()
  eliminatedFields <- x |>
    dplyr::filter(.data$change == "eliminated field") |>
    getFields()
  newTables <- x |>
    dplyr::filter(.data$change == "new table") |>
    getFields()
  newFields <- x |>
    dplyr::filter(.data$change == "new field") |>
    getFields()
  changed <- x |>
    dplyr::filter(!.data$change %in% c(
      "new field", "new table", "eliminated field", "eliminated table"
    )) |>
    dplyr::mutate(change = stringr::str_replace(
      string = .data$change, pattern = "changed from: ", replacement = ""
    ))
  renamed <- changed |>
    dplyr::filter(.data$change != .data$field) |>
    dplyr::mutate(
      table = stringr::str_extract(string = .data$field, pattern = "^[^-]+"),
      from = stringr::str_extract(string = .data$field, pattern = "(?<=-).+"),
      to = stringr::str_extract(string = .data$change, pattern = "(?<=-).+")
    ) |>
    dplyr::group_by(.data$table) |>
    dplyr::group_split() |>
    purrr::map_chr(\(x) {
      nm <- unique(x$table)
      ch <- paste0(x$from, "` -> `", x$to, collapse = "`; `")
      paste0("{.strong ", nm, "} (`", ch, "`)")
    }) |>
    nameBullet()
  spec <- changed |>
    dplyr::filter(.data$change == .data$field) |>
    getFields()

  cli::cli_inform("{.strong Changes between cdm version: {.pkg {ref}} and {.pkg {com}}:}")
  writeChanges(
    eliminatedTables, eliminatedFields, newTables, newFields, renamed, spec
  )

  NextMethod()
}
getFields <- function(x) {
  x |>
    dplyr::filter(stringr::str_detect(string = .data$field, pattern = "-")) |>
    dplyr::mutate(
      table = stringr::str_extract(string = .data$field, pattern = "^[^-]+"),
      field = stringr::str_extract(string = .data$field, pattern = "(?<=-).+")
    ) |>
    dplyr::group_by(.data$table) |>
    dplyr::group_split() |>
    purrr::map_chr(\(x) {
      nm <- unique(x$table)
      cols <- paste0(x$field, collapse = "`, `")
      paste0("{.strong ", nm, "} (`", cols, "`)")
    }) |>
    nameBullet()
}
nameBullet <- function(x) {
  if (length(x) > 0) {
    names(x) <- rep("*", length(x))
  }
  return(x)
}
writeChanges <- function(eliminatedTables,
                         eliminatedFields,
                         newTables,
                         newFields,
                         renamed,
                         spec) {
  change <- FALSE
  if (length(eliminatedTables) > 0) {
    cli::cli_inform(c(x = "Eliminated tables:"))
    cli::cli_inform(eliminatedTables)
    change <- TRUE
  }
  if (length(eliminatedFields) > 0) {
    cli::cli_inform(c(x = "Eliminated fields:"))
    cli::cli_inform(eliminatedFields)
    change <- TRUE
  }
  if (length(newTables) > 0) {
    cli::cli_inform(c(v = "New tables:"))
    cli::cli_inform(newTables)
    change <- TRUE
  }
  if (length(newFields) > 0) {
    cli::cli_inform(c(v = "New fields:"))
    cli::cli_inform(newFields)
    change <- TRUE
  }
  if (length(renamed) > 0) {
    cli::cli_inform(c("!" = "Renamed fields:"))
    cli::cli_inform(renamed)
    change <- TRUE
  }
  if (length(spec) > 0) {
    cli::cli_inform(c("!" = "Specifications changed for fields:"))
    cli::cli_inform(spec)
    change <- TRUE
  }
  if (!change) {
    cli::cli_inform("{.emph -None-}")
  }
  cli::cli_inform("")
}

#' Check if different package versions are used for a summarised_result object
#'
#' @param result A summarised_result object.
#'
#' @return A summarised_result object.
#' @export
#'
resultPackageVersion <- function(result) {
  # initial checks
  validateResultArgument(result, empty = TRUE)

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
