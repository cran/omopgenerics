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

#' `cohort_table` objects constructor.
#'
#' @param table cdm_table object with at least: cohort_definition_id,
#' subject_id, cohort_start_date, cohort_end_date.
#' @param cohortSetRef Table with at least: cohort_definition_id, cohort_name
#' @param cohortAttritionRef Table with at least: cohort_definition_id,
#' number_subjects, number_records, reason_id, reason, excluded_subjects,
#' excluded_records.
#' @param cohortCodelistRef Table with at least: cohort_definition_id, codelist_name,
#' concept_id and codelist_type.
#' @param .softValidation Whether to perform a soft validation of consistency.
#' If set to FALSE four additional checks will be performed: 1) a check that
#' cohort end date is not before cohort start date,  2) a check that there
#' are no missing values in required columns, 3) a check that cohort duration is
#' all within observation period, and 4) that there are no overlapping
#' cohort entries
#'
#' @return A cohort_table object
#'
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
#' cohort1 <- dplyr::tibble(
#'   cohort_definition_id = 1, subject_id = 1,
#'   cohort_start_date = as.Date("2020-01-01"),
#'   cohort_end_date = as.Date("2020-01-10")
#' )
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = person,
#'     "observation_period" = observation_period,
#'     "cohort1" = cohort1
#'   ),
#'   cdmName = "test"
#' )
#' cdm
#' cdm$cohort1 <- newCohortTable(table = cdm$cohort1)
#' cdm
#' settings(cdm$cohort1)
#' attrition(cdm$cohort1)
#' cohortCount(cdm$cohort1)
#'
newCohortTable <- function(table,
                           cohortSetRef = attr(table, "cohort_set"),
                           cohortAttritionRef = attr(table, "cohort_attrition"),
                           cohortCodelistRef = attr(table, "cohort_codelist"),
                           .softValidation = FALSE) {
  # initial checks
  assertClass(table, "cdm_table")
  assertChoice(.softValidation, choices = c(TRUE, FALSE), length = 1)

  if (is.na(tableName(table))) {
    missingCohortTableNameError(cdmReference(table), validation = "error")
  }
  if (!is.null(cohortSetRef)) {
    cohortSetRef <- cohortSetRef |> dplyr::as_tibble()
  }
  if (!is.null(cohortAttritionRef)) {
    cohortAttritionRef <- cohortAttritionRef |> dplyr::as_tibble()
  }
  if (!is.null(cohortCodelistRef)) {
    cohortCodelistRef <- cohortCodelistRef |> dplyr::as_tibble()
    if ("type" %in% colnames(cohortCodelistRef)) {
      cohortCodelistRef <- cohortCodelistRef |>
        dplyr::rename(codelist_type = "type")
    }
  }

  # 'clean' table
  table <- table |> removeClass("cohort_table")
  attr(table, "cohort_set") <- NULL
  attr(table, "cohort_attrition") <- NULL
  attr(table, "cohort_codelist") <- NULL

  # populate
  cohortSetRef <- populateCohortSet(table, cohortSetRef)
  cohortAttritionRef <- populateCohortAttrition(
    table, cohortSetRef, cohortAttritionRef
  )
  cohortCodelistRef <- populateCohortCodelist(table, cohortCodelistRef)

  # constructor
  cohort <- constructGeneratedCohortSet(
    table = table,
    cohortSetRef = cohortSetRef,
    cohortAttritionRef = cohortAttritionRef,
    cohortCodelistRef = cohortCodelistRef
  )

  # validate
  cohort <- validateGeneratedCohortSet(cohort, soft = .softValidation)

  # return
  return(cohort)
}

#' To collect a `cohort_table` object.
#'
#' @param x `cohort_table` object.
#' @param ... Not used (for compatibility).
#'
#' @return A data frame with the `cohort_table`
#'
#' @export
#'
#' @importFrom dplyr collect
#'
collect.cohort_table <- function(x, ...) {
  x <- removeClass(x, "cohort_table")
  y <- dplyr::collect(x)

  if (all(cohortColumns("cohort") %in% colnames(y))) {
    y <- addClass(y, "cohort_table")

    if (!is.null(attr(x, "cohort_set"))) {
      attr(y, "cohort_set") <- attr(x, "cohort_set") |> dplyr::collect()
    } else {
      cli::cli_abort("Table has class cohort_table but is missing cohort set attribute")
    }

    if (!is.null(attr(x, "cohort_attrition"))) {
      attr(y, "cohort_attrition") <- attr(x, "cohort_attrition") |> dplyr::collect()
    } else {
      cli::cli_abort("Table has class cohort_table but is missing cohort attrition attribute")
    }

    if (!is.null(attr(x, "cohort_codelist"))) {
      attr(y, "cohort_codelist") <- attr(x, "cohort_codelist") |> dplyr::collect()
    } else {
      cli::cli_abort("Table has class cohort_table but is missing cohort codelist attribute")
    }
  }

  return(y)
}

constructGeneratedCohortSet <- function(table,
                                        cohortSetRef,
                                        cohortAttritionRef,
                                        cohortCodelistRef) {
  table <- structure(
    .Data = table,
    "cohort_set" = noReference(cohortSetRef),
    "cohort_attrition" = noReference(cohortAttritionRef),
    "cohort_codelist" = noReference(cohortCodelistRef)
  ) |>
    addClass(c("cohort_table", "GeneratedCohortSet"))
  return(table)
}
validateGeneratedCohortSet <- function(cohort, soft = FALSE) {
  # get attributes
  cohort_set <- attr(cohort, "cohort_set")
  cohort_attrition <- attr(cohort, "cohort_attrition")
  cohort_codelist <- attr(cohort, "cohort_codelist")

  # assertClass
  assertClass(cohort, "cdm_table")
  assertClass(cohort_set, "cdm_table")
  assertClass(cohort_attrition, "cdm_table")
  assertClass(cohort_codelist, "cdm_table")

  # check cdm reference
  if (!"cdm_reference" %in% names(attributes(cohort))) {
    cli::cli_abort("cohort must be part of a cdm_reference")
  }

  # check name
  assertCharacter(tableName(cohort), length = 1, na = TRUE)
  assertCharacter(tableName(cohort_set), length = 1, na = TRUE)
  assertCharacter(tableName(cohort_attrition), length = 1, na = TRUE)
  assertCharacter(tableName(cohort_codelist), length = 1, na = TRUE)
  consistentNaming(
    cohortName = tableName(cohort),
    cohortSetName = tableName(cohort_set),
    cohortAttritionName = tableName(cohort_attrition),
    cohortCodelistName = tableName(cohort_codelist)
  )

  # check source
  srcCohort <- tableSource(cohort)
  srcCohortSet <- tableSource(cohort_set)
  srcCohortAttrition <- tableSource(cohort_attrition)
  srcCohort_codelist <- tableSource(cohort_codelist)
  if (!equal(srcCohort, srcCohortSet, srcCohortAttrition, srcCohort_codelist)) {
    cli::cli_abort(
      "The source must be the same for cohort, cohort_set, and cohort_attrition."
    )
  }

  # assert columns
  checkColumnsCohort <- function(x, nam) {
    cols <- cohortColumns(nam)
    if (!all(cols %in% colnames(x))) {
      cli::cli_abort(paste0(
        "`", paste0(cols, collapse = "`, `"), "` must be column names of the ",
        nam, " of a cohort_table object."
      ))
    }
    invisible(NULL)
  }
  checkColumnsCohort(cohort, "cohort")
  checkColumnsCohort(cohort_set, "cohort_set")
  checkColumnsCohort(cohort_attrition, "cohort_attrition")
  checkColumnsCohort(cohort_codelist, "cohort_codelist")

  # cast cohort columns
  cohort <- castCohortColumns(cohort, tableName(cohort), "cohort")

  # check cohort_codelist type colum
  checkCodelistType(cohort_codelist)

  # cohort_definition_id is coherent
  cdiCohort <- cdi(cohort)
  cdiCohortSet <- cdi(cohort_set)
  cdiCohortAttrition <- cdi(cohort_attrition)
  cdiCohortCodelist <- cdi(cohort_codelist)
  if (!all(cdiCohortSet == cdiCohortAttrition)) {
    cli::cli_abort(c(
      "Present cohort_definition_id must be the same:",
      "*" = "cohort_set: {cdiCohortSet}",
      "*" = "cohort_attrition: {cdiCohortAttrition}"
    ))
  }
  if (!all(cdiCohort %in% cdiCohortSet)) {
    cli::cli_abort(c(
      "There are cohort_definition_id that appear in cohort and not in cohort_set:",
      "*" = "cohort: {paste0(cdiCohort, collapse = ', ')}",
      "*" = "cohort_set: {paste0(cdiCohortSet, collapse = ', ')}"
    ))
  }
  if (!all(cdiCohortCodelist %in% cdiCohortSet)) {
    cli::cli_abort(c(
      "There are cohort_definition_id that appear in cohort and not in cohort_codelist:",
      "*" = "cohort_codelist: {paste0(cdiCohortCodelist, collapse = ', ')}",
      "*" = "cohort_set: {paste0(cdiCohortSet, collapse = ', ')}"
    ))
  }

  if (!soft) {
    cohort <- validateCohortArgument(
      cohort = cohort,
      checkEndAfterStart = TRUE, # check start before end
      checkMissingValues = TRUE, # check NA
      checkOverlappingEntries = TRUE, # check overlap
      checkInObservation = TRUE, # check within observation period
      checkPermanentTable = TRUE, # check it is a permanent table
      validation = "error"
    )
  }

  return(cohort)
}
equal <- function(...) {
  x <- list(...)
  flag <- TRUE
  for (k in 2:length(x)) {
    flag <- flag & all(x[[1]] == x[[k]])
  }
  return(flag)
}
cl <- function(x) {
  x <- class(x)
  x <- x[stringr::str_detect(stringr::str_to_lower(x), "cohort", negate = TRUE)]
  paste0(x, collapse = ", ")
}
cdi <- function(x) {
  x |>
    dplyr::select("cohort_definition_id") |>
    dplyr::distinct() |>
    dplyr::pull() |>
    sort()
}
defaultCohortSet <- function(cohort) {
  cohortName <- tableName(cohort)
  name <- ifelse(is.na(cohortName), cohortName, paste0(cohortName, "_set"))
  cohort |>
    dplyr::select("cohort_definition_id") |>
    dplyr::distinct() |>
    dplyr::mutate(
      "cohort_definition_id" = as.integer(.data$cohort_definition_id),
      "cohort_name" = paste0("cohort_", as.character(.data$cohort_definition_id))
    ) |>
    collect()
}
defaultCohortAttrition <- function(cohort, set) {
  cohortName <- tableName(cohort)
  name <- ifelse(is.na(cohortName), cohortName, paste0(cohortName, "_attrition"))
  x <- cohort |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id)
    ) |>
    collect() |>
    dplyr::right_join(
      set |>
        dplyr::select("cohort_definition_id") |>
        dplyr::collect(),
      by = "cohort_definition_id"
    ) |>
    dplyr::mutate(
      "cohort_definition_id" = as.integer(.data$cohort_definition_id),
      "number_records" = dplyr::if_else(
        is.na(.data$number_records), 0L, as.integer(.data$number_records)
      ),
      "number_subjects" = dplyr::if_else(
        is.na(.data$number_subjects), 0L, as.integer(.data$number_subjects)
      ),
      "reason_id" = 1L,
      "reason" = "Initial qualifying events",
      "excluded_records" = 0L,
      "excluded_subjects" = 0L
    )
  return(x)
}
defaultCohortCodelist <- function(cohort) {
  dplyr::tibble(
    cohort_definition_id = as.integer(),
    codelist_name = as.character(),
    concept_id = as.integer(),
    codelist_type = as.character()
  )
}

#' Check whether a cohort table satisfies requirements
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param cohort `cohort_table` object.
#' @param checkEndAfterStart If TRUE a check that all cohort end dates come on or
#' after cohort start date will be performed.
#' @param checkOverlappingEntries If TRUE a check that no individuals have overlapping
#' cohort entries will be performed.
#' @param checkMissingValues If TRUE a check that there are no missing values in
#' required fields will be performed.
#' @param checkInObservation If TRUE a check that cohort entries are within
#' the individuals observation periods will be performed.
#' @param type Can be either "error" or "warning". If "error" any check
#' failure will result in an error, whereas if "warning" any check failure
#' will result in a warning.
#' @param call The call for which to return the error message.
#'
#' @return An error will be returned if any of the selected checks fail.
#'
#' @export
#'
checkCohortRequirements <- function(cohort,
                                    checkEndAfterStart = TRUE,
                                    checkOverlappingEntries = TRUE,
                                    checkMissingValues = TRUE,
                                    checkInObservation = TRUE,
                                    type = "error",
                                    call = parent.frame()) {
  lifecycle::deprecate_stop(
    when = "0.3.0",
    what = "checkCohortRequirements()",
    with = "validateCohortArgument()"
  )
}

checkStartEnd <- function(cohort, validation, call) {
  x <- cohort |>
    dplyr::mutate(end_before_start = dplyr::if_else(.data$cohort_end_date < .data$cohort_start_date,
                                         1L, 0L)) |>
    dplyr::select(c("subject_id", "end_before_start")) |>
    dplyr::filter(.data$end_before_start == 1L) |>
    dplyr::collect()

  if (nrow(x) > 0) {
    x5 <- x |>
      dplyr::ungroup() |>
      dplyr::select("subject_id") |>
      utils::head(5) |>
      dplyr::pull()
    if (validation == "error") {
      cli::cli_abort(
        message = c(
          "!" = "cohort_start_date must be <= tham cohort_end_date. There is
          not the case for  {nrow(x)} entries where cohort_end_date < cohort_start_date
          for subject_id {x5}"
        ),
        call = call
      )
    } else if (validation == "warning") {
      cli::cli_warn(
        message =  c(
          "!" = "cohort_start_date must be <= tham cohort_end_date. There is
          not the case for  {nrow(x)} entries where cohort_end_date < cohort_start_date
          for subject_id {x5}"
        )
      )
    }
  }
  return(cohort)
}
checkOverlap <- function(cohort, validation, call) {
  x <- cohort |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::arrange(.data$cohort_start_date) |>
    dplyr::mutate(
      "next_cohort_start_date" = dplyr::lead(.data$cohort_start_date)
    ) |>
    dplyr::mutate(overlap = dplyr::if_else(
      .data$cohort_end_date >= .data$next_cohort_start_date,
      1L, 0L)) |>
    dplyr::ungroup() |>
    dplyr::select(c("subject_id", "overlap")) |>
    dplyr::filter(.data$overlap == 1L) |>
    dplyr::collect()
  if (nrow(x) > 0) {
    x5 <- x |>
      utils::head(5) |>
      dplyr::pull("subject_id")

    if (validation == "error") {
      cli::cli_abort(
        message = c(
          "!" = "There is overlap between entries in the cohort, {nrow(x)} overlap{?s}
        detected for subject_id {x5}"),
        call = call
      )
    } else if (validation == "warning") {
      cli::cli_warn(
        message = c(
          "!" = "There is overlap between entries in the cohort, {nrow(x)} overlap{?s}
        detected for subject_id {x5}")
      )
    }
  }
  return(cohort)
}
checkNaCohort <- function(cohort, validation, call) {
  x <- cohort |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::mutate(missing = dplyr::if_else(
      is.na(.data$cohort_definition_id) |
      is.na(.data$subject_id) |
      is.na(.data$cohort_start_date) |
      is.na(.data$cohort_end_date),
      1L, 0L)) |>
    dplyr::select(dplyr::all_of(c("subject_id", "missing"))) |>
    dplyr::filter(.data$missing == 1L) |>
    dplyr::collect()
  if (nrow(x) > 0) {
    x5 <- x |>
      utils::head(5) |>
      dplyr::pull("subject_id")

    if (validation == "error") {
      cli::cli_abort(
        c("!" = "Cohort can't have NA values, there are NA values in {nrow(x)} column{?s}:
          see subject_id {x5}"),
        call = call
      )
    } else if (validation == "warning") {
      cli::cli_warn(
        c("!" = "Cohort can't have NA values, there are NA values in {nrow(x)} column{?s}:
          see subject_id {x5}")
      )
    }
  }
  return(cohort)
}
checkObservationPeriod <- function(cohort, validation, call) {
  cdm <- cdmReference(cohort)
  x <- cohort |>
    dplyr::anti_join(
      cohort |>
        dplyr::select(dplyr::all_of(cohortColumns("cohort"))) |>
        dplyr::inner_join(
          cdm[["observation_period"]] |>
            dplyr::select(
              "subject_id" = "person_id", "observation_period_start_date",
              "observation_period_end_date"
            ),
          by = "subject_id"
        ) |>
        dplyr::filter(
          .data$cohort_start_date >= .data$observation_period_start_date &
            .data$cohort_start_date <= .data$observation_period_end_date &
            .data$cohort_end_date >= .data$observation_period_start_date &
            .data$cohort_end_date <= .data$observation_period_end_date
        ),
      by = cohortColumns("cohort")
    ) |>
    dplyr::tally() |>
    dplyr::pull("n")

  if (x > 0) {
    mes <- c("!" = "{x} observation{?s} outside observation period.")
    if (validation == "error") {
      cli::cli_abort(message = mes, call = call)
    } else if (validation == "warning") {
      cli::cli_warn(message = mes)
    }
  }
  return(cohort)
}
checkCodelistType <- function(cohort_codelist) {
  codelist_types <- cohort_codelist |>
    dplyr::pull("codelist_type") |>
    unique()
  assertChoice(
    codelist_types,
    c("index event", "inclusion criteria", "exit criteria")
  )
}
checkCohortAttributes <- function(cohort, validation, call) {
  toCheck <- c("cohort_attrition", "cohort_set", "cohort_codelist")

  # check attributes exist
  notPresent <- toCheck[!toCheck %in% names(attributes(cohort))]
  if (length(notPresent) > 0) {
    "{.var cohort} does not have the following required attributes: {notPresent}." |>
      cli::cli_abort(call = call)
  }

  # check columns of attributes
  for (at in toCheck) {
    cols <- colnames(attr(cohort, at))
    required <- cohortColumns(at)
    notPresent <- required[!required %in% cols]
    if (length(notPresent) > 0) {
      "The following columns are missing: {notPresent} in {.pkg {at}} attribute" |>
        cli::cli_abort(call = call)
    }
  }

  # check that cohort_definition_ids are present
  cohortIds <- cohort |>
    dplyr::distinct(.data$cohort_definition_id) |>
    dplyr::pull()
  for (at in c("cohort_set", "cohort_attrition")) {
    presentIds <- attr(cohort, at) |>
      dplyr::distinct(.data$cohort_definition_id) |>
      dplyr::pull()
    notPresent <- cohortIds[!cohortIds %in% presentIds]
    if (length(notPresent) > 0) {
      "{.var cohort_definition_id}: {notPresent}, present in {.pkg cohort}, but not present in {.pkg {at}}." |>
        cli::cli_abort(call = call)
    }
  }

  return(cohort)
}
consistentNaming <- function(cohortName,
                             cohortSetName,
                             cohortAttritionName,
                             cohortCodelistName) {
  if (is.na(cohortName)) {
    if (!is.na(cohortSetName) | !is.na(cohortAttritionName)) {
      cli::cli_abort("cohort is a temp table, cohort_set and cohort_attrition should be a temp table too")
    }
  } else {
    errorMessage <- character()
    if (cohortSetName != paste0(cohortName, "_set")) {
      errorMessage <- c(errorMessage, "cohort_set name must be {paste0(cohortName, '_set')} but is {cohortSetName}")
    }
    if (cohortAttritionName != paste0(cohortName, "_attrition")) {
      errorMessage <- c(errorMessage, "cohort_attrition name must be {paste0(cohortName, '_attrition')} but is {cohortAttritionName}")
    }
    if (cohortCodelistName != paste0(cohortName, "_codelist")) {
      errorMessage <- c(errorMessage, "cohort_codelist name must be {paste0(cohortName, '_codelist')} but is {cohortCodelistName}")
    }
    if (length(errorMessage) > 0) {
      cli::cli_abort(errorMessage)
    }
  }
  return(invisible(TRUE))
}
populateCohortSet <- function(table, cohortSetRef) {
  if (is.null(cohortSetRef)) {
    cohortSetRef <- defaultCohortSet(table)
  }
  cohortName <- tableName(table)

  assertClass(cohortSetRef, "data.frame", null = TRUE)
  cohortSetRef <- dplyr::as_tibble(cohortSetRef)

  # cohort_name column
  if ("cohort_name" %in% colnames(cohortSetRef)) {
    cohortSetRef <- updateCohortNames(cohortSetRef)
  }

  cohortSetRef <- cohortSetRef |>
    dplyr::relocate(dplyr::any_of(cohortColumns("cohort_set")))

  name <- ifelse(is.na(cohortName), cohortName, paste0(cohortName, "_set"))
  cohortSetRef <- castCohortColumns(cohortSetRef, cohortName, "cohort_set")
  cohortSetRef <- insertTable(
    cdm = tableSource(table), name = name, table = cohortSetRef,
    overwrite = TRUE
  )
  return(cohortSetRef)
}
populateCohortAttrition <- function(table, cohortSetRef, cohortAttritionRef) {
  if (is.null(cohortAttritionRef)) {
    cohortAttritionRef <- defaultCohortAttrition(table, cohortSetRef)
  }
  cohortName <- tableName(table)
  assertClass(cohortAttritionRef, "data.frame", null = TRUE)
  cohortAttritionRef <- dplyr::as_tibble(cohortAttritionRef) |>
    dplyr::relocate(dplyr::any_of(cohortColumns("cohort_attrition")))
  name <- ifelse(is.na(cohortName), cohortName, paste0(cohortName, "_attrition"))
  cohortAttritionRef <- castCohortColumns(
    cohortAttritionRef, cohortName, "cohort_attrition"
  )
  cohortAttritionRef <- insertTable(
    cdm = tableSource(table), name = name, table = cohortAttritionRef,
    overwrite = TRUE
  )
  return(cohortAttritionRef)
}
populateCohortCodelist <- function(table, cohortCodelistRef) {
  if (is.null(cohortCodelistRef)) {
    cohortCodelistRef <- defaultCohortCodelist(table)
  }
  cohortName <- tableName(table)
  assertClass(cohortCodelistRef, "data.frame", null = TRUE)
  cohortCodelistRef <- dplyr::as_tibble(cohortCodelistRef) |>
    dplyr::relocate(dplyr::any_of(cohortColumns("cohort_codelist")))
  name <- ifelse(is.na(cohortName), cohortName, paste0(cohortName, "_codelist"))
  cohortCodelistRef <- castCohortColumns(
    cohortCodelistRef, cohortName, "cohort_codelist"
  )
  cohortCodelistRef <- insertTable(
    cdm = tableSource(table), name = name, table = cohortCodelistRef,
    overwrite = TRUE
  )
  return(cohortCodelistRef)
}

#' Create an empty cohort_table object
#'
#' @param cdm A cdm_reference to create the table.
#' @param name Name of the table to create.
#' @param overwrite Whether to overwrite an existent table.
#'
#' @export
#'
#' @return The cdm_reference with an empty cohort table
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
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "test"
#' )
#'
#' cdm <- emptyCohortTable(cdm, "my_empty_cohort")
#'
#' cdm
#' cdm$my_empty_cohort
#' settings(cdm$my_empty_cohort)
#' attrition(cdm$my_empty_cohort)
#' cohortCount(cdm$my_empty_cohort)
#'
emptyCohortTable <- function(cdm, name, overwrite = TRUE) {
  assertCharacter(name, length = 1)
  assertClass(cdm, "cdm_reference")
  table <- omopTableFields(cdmVersion(cdm)) |>
    dplyr::filter(.data$cdm_table_name == "cohort" & .data$type == "cohort") |>
    emptyTable()
  cdm <- insertTable(cdm = cdm, name = name, table = table, overwrite = overwrite)
  cdm[[name]] <- newCohortTable(cdm[[name]], .softValidation = TRUE)
  return(cdm)
}

castCohortColumns <- function(table, tName, name) {
  cols <- omopTableFields() |>
    dplyr::filter(.data$type == "cohort" & .data$cdm_table_name == .env$name) |>
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
  if (name != "cohort") {
    cast <- TRUE
    tName <- paste0(tName, " (", name, ")")
  } else {
    cast <- FALSE
  }
  table <- castColumns(table, cols, tName, cast)
  return(table)
}
emptyTable <- function(fields) {
  lapply(fields$cdm_datatype, getEmptyField) |>
    rlang::set_names(fields$cdm_field_name) |>
    dplyr::as_tibble()
}
getEmptyField <- function(datatype) {
  datatype[stringr::str_detect(datatype, "varchar")] <- "varchar"
  empty <- switch(datatype,
    "integer" = integer(),
    "datetime" = as.Date(integer()),
    "date" = as.Date(integer()),
    "float" = numeric(),
    "varchar" = character(),
    "logical" = logical()
  )
  return(empty)
}
missingCohortTableNameError <- function(cdm, validation = "error") {
  if (sourceType(cdm) == "local") {
    mes <- c(
      "x" = "Table name for cohort could not be inferred.",
      "i" = "Did you use insertTable() when adding the table to the cdm reference?"
    )
  } else {
    mes <- c(
      "x" = "Table name for cohort could not be inferred.",
      "i" = "The cohort table must be a permanent table when working with databases.",
      "i" = "Use dplyr::compute(temporary = FALSE, ...) to create a permanent table from a temporary table."
    )
  }
  if (validation == "error") {
    cli::cli_abort(mes)
  } else if (validation == "warning") {
    cli::cli_warn(mes)
  } else {
    return(invisible())
  }
}
updateCohortNames <- function(cohortSetRef) {
  cohortNames <- cohortSetRef |> dplyr::pull("cohort_name")
  limChar <- 100
  newNames <- substr(toSnakeCase(cohortNames), 1, limChar)
  different <- cohortNames != newNames
  if (any(different)) {
    oldName <- cohortNames[different]
    newName <- newNames[different]
    x <- paste0(oldName, " -> ", newName)
    names(x) <- rep("*", length(x))
    cli::cli_warn(c(
      "cohort_name must be snake case and have less than {limChar} characters,
      the following cohorts will be renamed:",
      x
    ))
    id <- uniqueId(exclude = colnames(cohortSetRef), prefix = "id_")
    cohortSetRef <- cohortSetRef |>
      dplyr::left_join(
        dplyr::tibble(!!id := newNames, cohort_name = cohortNames),
        by = "cohort_name"
      ) |>
      dplyr::select(-"cohort_name") |>
      dplyr::rename("cohort_name" = dplyr::all_of(id))
  }

  if (length(newNames) != length(unique(newNames))) {
    cli::cli_abort("cohort_name in the cohort_set must be unique")
  }

  return(cohortSetRef)
}
