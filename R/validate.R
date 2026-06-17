# Copyright 2024 DARWIN EU (C)
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

#' Validate name argument. It must be a snake_case character vector. You can add
#' a cdm object to check that `name` is not already used in that cdm.
#'
#' @param name Name of a new table to be added to a cdm object.
#' @param cdm A `<cdm_reference>` object. If provided, the name will be checked
#' to ensure it does not already exist in the cdm.
#' @param null If TRUE, name can be NULL
#' @inheritParams emptyDoc
#' @inheritParams validationDoc
#' @param nm Name to use in error messages. Defaults to the expression supplied
#' to `name`.
#' @inheritParams cliCallDoc
#'
#' @export
#' @examples
#' # this is a validate name
#' name <- "my_new_table"
#' validateNameArgument(name)
#'
#' # this is not
#' name <- "myTableNAME"
#' validateNameArgument(name, validation = "warning")
#'
validateNameArgument <- function(name,
                                 cdm = NULL,
                                 validation = "error",
                                 null = FALSE,
                                 empty = TRUE,
                                 nm = deparse1(substitute(name), backtick = TRUE),
                                 call = parent.frame()) {
  assertValidation(validation, call = call)
  if (isTRUE(null) && (is.null(name) | isTRUE(is.na(name)))) {
    return(NULL)
  }

  assertCharacter(
    name, length = 1, null = null, empty = empty, nm = nm, call = call
  )
  if (length(name) == 0) {
    return(name)
  }
  newName <- toSnakeCase(name)
  if (newName != name) {
    if (validation == "error") {
      cli::cli_abort(c("!" = "`{nm}` is not snake_case it should be modified to: {newName}"), call = call)
    } else if (validation == "warning") {
      cli::cli_warn(c("!" = "`{nm}` was modified: {name} -> {newName}"), call = call)
    }
  }
  if (!is.null(cdm)) {
    if (newName %in% names(cdm)) {
      if (validation == "error") {
        cli::cli_abort(c("!" = "There already exist a table named {.var {newName}}."), call = call)
      } else if (validation == "warning") {
        cli::cli_warn(c("!" = "There already exist a table named {.var {newName}}. It will be overwritten."), call = call)
      }
    }
  }
  return(newName)
}

#' Validate a cohort table input.
#'
#' @param cohort Object to be validated as a valid cohort input.
#' @inheritParams cohortValidationChecksDoc
#' @param checkAttributes Whether to check if attributes are present and
#' populated correctly.
#' @param checkPermanentTable Whether to check if the table has to be a
#' permanent table.
#' @param dropExtraColumns Whether to drop extra columns that are not the
#' required ones.
#' @inheritParams emptyDoc
#' @inheritParams validationDoc
#' @param nm Name to use in error messages. Defaults to the expression supplied
#' to `cohort`.
#' @inheritParams cliCallDoc
#'
#' @export
#' @examples
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = dplyr::tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = dplyr::tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'    ),
#'    cohortTables = list(
#'     cohort = dplyr::tibble(
#'       cohort_definition_id = 1L,
#'       subject_id = 1L,
#'       cohort_start_date = as.Date("2020-01-01"),
#'       cohort_end_date = as.Date("2021-02-10")
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' validateCohortArgument(cdm$cohort)
#'
validateCohortArgument <- function(cohort,
                                   checkEndAfterStart = FALSE,
                                   checkOverlappingEntries = FALSE,
                                   checkMissingValues = FALSE,
                                   checkInObservation = FALSE,
                                   checkAttributes = FALSE,
                                   checkPermanentTable = FALSE,
                                   dropExtraColumns = FALSE,
                                   empty = TRUE,
                                   validation = "error",
                                   nm = deparse1(substitute(cohort), backtick = TRUE),
                                   call = parent.frame()) {
  assertValidation(validation, call = call)
  assertLogical(checkEndAfterStart, length = 1, call = call)
  assertLogical(checkOverlappingEntries, length = 1, call = call)
  assertLogical(checkMissingValues, length = 1, call = call)
  assertLogical(checkInObservation, length = 1, call = call)
  assertLogical(checkAttributes, length = 1, call = call)
  assertLogical(dropExtraColumns, length = 1, call = call)
  assertLogical(checkPermanentTable, length = 1, call = call)

  assertClass(cohort, class = c("cohort_table", "cdm_table"), all = TRUE, nm = nm, call = call)
  assertTable(cohort, empty = empty, nm = nm, call = call)

  if (is.na(tableName(cohort)) & checkPermanentTable) {
    missingCohortTableNameError(cdmReference(cohort), validation = validation)
  }

  # columns
  notPresent <- cohortColumns("cohort")[!cohortColumns("cohort") %in% colnames(cohort)]
  if (length(notPresent) > 0) {
    if (validation == "error") {
      cli::cli_abort(c("!" = "columns: {.var {notPresent}} not present in `{nm}`"), call = call)
    } else if (validation == "warning") {
      cli::cli_warn(c("!" = "columns: {.var {notPresent}} not present in `{nm}`"), call = call)
    }
  }
  if (isTRUE(checkEndAfterStart)) {
    cohort <- checkStartEnd(cohort = cohort, validation = validation, call = call)
  }
  if (isTRUE(checkOverlappingEntries)) {
    cohort <- checkOverlap(cohort = cohort, validation = validation, call = call)
  }
  if (isTRUE(checkMissingValues)) {
    cohort <- checkNaCohort(cohort = cohort, validation = validation, call = call)
  }
  if (isTRUE(checkInObservation)) {
    cohort <- checkObservationPeriod(cohort = cohort, validation = validation, call = call)
  }
  if (isTRUE(checkAttributes)) {
    cohort <- checkCohortAttributes(cohort = cohort, validation = validation, call = call)
  }
  if (dropExtraColumns) {
    cols <- colnames(cohort)
    extraColumns <- cols[!cols %in% cohortColumns("cohort")]
    if (length(extraColumns) > 0) {
      cli::cli_warn(c("!" = "Extra columns dropped: {.var {extraColumns}}."), call = call)
    }
    cohort <- cohort |>
      dplyr::select(dplyr::any_of(cohortColumns("cohort")))
  } else {
    cohort <- cohort |>
      dplyr::relocate(dplyr::any_of(cohortColumns("cohort")))
  }

  return(cohort)
}

#' Validate cohortId argument. CohortId can either be a cohort_definition_id
#' value, a cohort_name or a tidyselect expression referring to cohort_names. If
#' you want to support tidyselect expressions please use the function as:
#' `validateCohortIdArgument({{cohortId}}, cohort)`.
#'
#' @param cohortId A cohortId vector to be validated.
#' @inheritParams cohortDoc
#' @param null Whether `NULL` is accepted. If NULL all `cohortId` will be
#' returned.
#' @inheritParams emptyDoc
#' @inheritParams validationDoc
#' @param nm Name to use in error messages. Defaults to the expression supplied
#' to `cohortId`.
#' @inheritParams cliCallDoc
#'
#' @export
#' @examples
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = dplyr::tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = dplyr::tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'    ),
#'    cohortTables = list(
#'     cohort = dplyr::tibble(
#'       cohort_definition_id = 1L,
#'       subject_id = 1L,
#'       cohort_start_date = as.Date("2020-01-01"),
#'       cohort_end_date = as.Date("2021-02-10")
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' validateCohortIdArgument(NULL, cdm$cohort)
#' validateCohortIdArgument(1L, cdm$cohort)
#' validateCohortIdArgument(2L, cdm$cohort, validation = "warning")
#'
validateCohortIdArgument <- function(cohortId,
                                     cohort,
                                     null = TRUE,
                                     empty = TRUE,
                                     validation = "error",
                                     nm = deparse1(substitute(cohortId), backtick = TRUE),
                                     call = parent.frame()) {
  cohortId <- rlang::enquo(cohortId)
  assertValidation(validation, call = call)
  assertClass(cohort, class = "cohort_table", call = call)
  assertLogical(null, length = 1, call = call)

  # settings
  set <- settings(cohort)

  # type of input
  type <- selectType(cohortId)

  if (type == "NULL") {
    if (!null) {
      cli::cli_abort(c(x = "`NULL` is not allowed for `{nm}`"), call = call)
    } else {
      cohortId <- set$cohort_definition_id
    }
  } else if (type %in% c("tidy", "character")) {
    if (type == "tidy") {
      cohortName <- tidySelect(set$cohort_name, cohortId)
    } else {
      cohortId <- rlang::eval_tidy(cohortId)
      areIn <- cohortId %in% set$cohort_name
      notPresent <- cohortId[!areIn]
      if (length(notPresent) > 0) {
        report(
          message = "cohort name: {notPresent} not defined in settings.",
          validation = validation,
          call = call
        )
      }
      cohortName <- cohortId[areIn]
    }
    cohortId <- cohortName |>
      purrr::map_int(\(x) set$cohort_definition_id[set$cohort_name == x])
  } else if (type == "numeric") {
    cohortId <- as.integer(rlang::eval_tidy(cohortId))
    areIn <- cohortId %in% set$cohort_definition_id
    notPresent <- cohortId[!areIn]
    if (length(notPresent) > 0) {
      report(
        message = "cohort definition id: {notPresent} not defined in settings.",
        validation = validation,
        call = call
      )
    }
    cohortId <- cohortId[areIn]
  } else {
    cli::cli_abort(
      c(x = "`{nm}` must be NULL, a numeric vector, a character vector, or a tidyselect expression."),
      call = call
    )
  }

  if (length(cohortId) == 0 && !empty) {
    report(message = "`{nm}` is empty.", validation = validation, call = call)
  }

  return(cohortId)
}
selectType <- function(q) {
  # NULL, numeric or character
  maybe <- tryCatch(
    rlang::eval_tidy(q),
    error = function(e) e
  )
  if (!inherits(maybe, "error")) {
    if (is.null(maybe)) {
      return("NULL")
    }
    if (is.numeric(maybe)) {
      return("numeric")
    }
    if (is.character(maybe)) {
      return("character")
    }
    return("other")
  }

  # tidy select
  return("tidy")
}

#' Validate conceptSet argument. It can either be a list, a codelist, a
#' concept set expression or a codelist with details. The output will always be a
#' codelist.
#'
#' @param conceptSet It can be either a named list of concepts or a codelist,
#' codelist_with_details or concept_set_expression object.
#' @param cdm A `<cdm_reference>` object. If provided, concept IDs in the
#' resulting codelist will be checked against `cdm$concept`. It is also needed
#' if a concept_set_expression with descendants is provided.
#' @inheritParams emptyDoc
#' @inheritParams validationDoc
#' @param nm Name to use in error messages. Defaults to the expression supplied
#' to `conceptSet`.
#' @inheritParams cliCallDoc
#'
#' @return A codelist object.
#'
#' @export
#' @examples
#' conceptSet <- list(disease_x = c(1L, 2L))
#' validateConceptSetArgument(conceptSet)
#'
validateConceptSetArgument <- function(conceptSet,
                                       cdm = NULL,
                                       empty = TRUE,
                                       validation = "error",
                                       nm = deparse1(substitute(conceptSet), backtick = TRUE),
                                       call = parent.frame()) {
  assertValidation(validation, call = call)
  if (is.null(conceptSet)) {
    if (!empty) {
      report(message = "`{nm}` is empty.", validation = validation, call = call)
    }
    return(emptyCodelist())
  }
  if (inherits(conceptSet, "codelist")) {
    assertList(
      conceptSet, named = TRUE, class = c("numeric", "integer", "integer64"),
      empty = TRUE, nm = nm, call = call
    )
    conceptSet <- validateCodelist(conceptSet, nm = nm, cdm = cdm, call = call)
  } else if (inherits(conceptSet, "codelist_with_details")) {
    assertList(
      conceptSet, named = TRUE, class = c("data.frame", "tbl_df"),
      empty = TRUE, nm = nm, call = call
    )
    conceptSet <- validateCodelistWithDetails(conceptSet, nm = nm, cdm = cdm, call = call) |>
      purrr::map(\(x) dplyr::pull(x, "concept_id")) |>
      newCodelist()
  } else if (inherits(conceptSet, "concept_set_expression")) {
    assertList(
      conceptSet, named = TRUE, class = c("tbl"), empty = TRUE, nm = nm,
      call = call
    )
    concepts <- validateConceptSetExpression(conceptSet, nm = nm, cdm = cdm, call = call) |>
      dplyr::as_tibble() |>
      dplyr::select("concept_set_expression_name", "concept_id",
                    "descendants", "excluded")
    descendants <- concepts |>
      dplyr::filter(.data$descendants == TRUE) |>
      dplyr::select(-"descendants")
    if (nrow(descendants) > 0) {
      assertClass(cdm, "cdm_reference", call = call)
      tmpName <- uniqueTableName()
      cdm <- insertTable(cdm = cdm, name = tmpName, table = descendants)
      descendants <- cdm$concept_ancestor |>
        dplyr::rename("to_join" = "ancestor_concept_id") |>
        dplyr::inner_join(
          cdm[[tmpName]] |>
            dplyr::rename("to_join" = "concept_id"),
          by = "to_join"
        ) |>
        dplyr::select("concept_id" = "descendant_concept_id",
                      "excluded", "concept_set_expression_name") |>
        dplyr::collect()
      dropSourceTable(cdm = cdm, name = tmpName)
      concepts <- concepts |>
        dplyr::filter(.data$descendants == FALSE) |>
        dplyr::select(-"descendants") |>
        dplyr::union_all(descendants)
    } else {
      concepts <- concepts |> dplyr::select(-"descendants")
    }
    conceptSet <- concepts |>
      dplyr::filter(.data$excluded == FALSE) |>
      dplyr::select(-"excluded") |>
      dplyr::anti_join(
        concepts |>
          dplyr::filter(.data$excluded == TRUE),
        by = c("concept_id", "concept_set_expression_name")
      ) |>
      dplyr::select("codelist_name" = "concept_set_expression_name", "concept_id") |>
      newCodelist(cdm = cdm)
  } else {
    assertList(conceptSet, empty = empty, nm = nm, call = call)
    conceptSet <- newCodelist(conceptSet, cdm = cdm)
  }
  if (length(conceptSet) == 0 && !empty) {
    report(message = "`{nm}` is empty.", validation = validation, call = call)
  }
  return(conceptSet)
}
assertValidation <- function(validation,
                             call = parent.frame()) {
  assertChoice(
    x = validation,
    choices = c("error", "warning"),
    length = 1,
    nm = "validation",
    call = call
  )
}

#' Validate a window argument. It must be a list of two elements (window start
#' and window end), both must be numeric, integerish by default, and window
#' start must be lower or equal than window end.
#'
#' @param window time window
#' @param snakeCase return default window  name in snake case if TRUE
#' @param integerish Whether window values must be integerish.
#' @inheritParams emptyDoc
#' @param nm Name to use in error messages. Defaults to the expression supplied
#' to `window`.
#' @inheritParams cliCallDoc
#'
#' @return time window
#' @export
#' @examples
#' validateWindowArgument(list(c(0, 15), c(-Inf, Inf)))
#' validateWindowArgument(list(c(0, 15), c(-Inf, Inf)), snakeCase = FALSE)
#'
validateWindowArgument <- function(window,
                                   snakeCase = TRUE,
                                   integerish = TRUE,
                                   empty = TRUE,
                                   nm = deparse1(substitute(window), backtick = TRUE),
                                   call = parent.frame()) {
  assertLogical(snakeCase, length = 1, call = call)
  assertLogical(integerish, length = 1, call = call)

  if (length(window) == 0) {
    assertList(list(), empty = empty, nm = nm, call = call)
    return(list())
  }
  if (!is.list(window)) {
    window <- list(window)
  }

  # Find if any NA, throw warning that it will be changed to Inf, change it later
  if (any(unlist(lapply(window, is.na)))) {
    cli::cli_abort("NA found in `{nm}`, please use Inf or -Inf instead", call = call)
  }

  assertList(window, empty = empty, nm = nm, call = call)
  elements <- window |>
    unlist() |>
    purrr::keep(\(x) !is.infinite(x)) |>
    unique()
  assertNumeric(
    elements,
    integerish = integerish,
    empty = TRUE,
    nm = paste0("elements of ", nm),
    call = call
  )

  # if any element of window list has length over 2, throw error
  if (any(lengths(window) > 2)) {
    "`{nm}` can only contain two values: windowStart and windowEnd" |>
      cli::cli_abort(call = call)
  }

  # eg if list(1,2,3), change to list(c(1,1), c(2,2), c(3,3))
  if (length(window) > 1 && any(lengths(window) == 1)) {
    window[lengths(window) == 1] <- lapply(
      window[lengths(window) == 1],
      function(x) {
        c(
          unlist(x[lengths(x) == 1]),
          unlist(x[lengths(x) == 1])
        )
      }
    )
    cli::cli_warn(
      "Window list contains element with only 1 value provided,
          use it as both window start and window end"
    )
  }

  assertWindowName(window, snakeCase, label = nm, call = call)
}
getWindowNames <- function(window, snakeCase) {
  # snakecase
  getname <- function(element) {
    element <- tolower(as.character(element))
    element <- stringr::str_replace_all(
      string = element,
      pattern = "-",
      replacement = "m"
    )
    paste0(element[1], "_to_", element[2])
  }
  # snakecase False
  getname2 <- function(element) {
    element <- tolower(as.character(element))
    paste0(element[1], " to ", element[2])
  }

  windowNames <- names(window)

  if (isTRUE(snakeCase)) {
    if (is.null(windowNames)) {
      windowNames <- purrr::map_chr(window, getname)
    } else {
      id <- windowNames == ""
      windowNames[id] <- purrr::map_chr(window[id], getname)
      newNames <- toSnakeCase(windowNames)
      differentNames <- which(windowNames != newNames)
      if (length(differentNames) > 0) {
        newName <- newNames[differentNames]
        oldName <- windowNames[differentNames]
        changes <- paste0("`", oldName, "` -> `", newName, "`") |>
          rlang::set_names(rep("*", length(newName)))
        cli::cli_inform(c("window names cast to snake_case: ", changes))
      }
      windowNames <- newNames
    }
  } else {
    if (is.null(windowNames)) {
      windowNames <- purrr::map_chr(window, getname2)
    } else {
      id <- windowNames == ""
      windowNames[id] <- purrr::map_chr(window[id], getname2)
    }
  }
  windowNames
}
assertWindowName <- function(window, snakeCase, label = "window", call = parent.frame()) {
  names(window) <- getWindowNames(window, snakeCase = snakeCase)
  lower <- purrr::map_dbl(window, \(x) x[1])
  upper <- purrr::map_dbl(window, \(x) x[2])

  if (any(lower > upper)) {
    "First element in `{label}` must be smaller or equal to the second one" |>
      cli::cli_abort(call = call)
  }
  if (any(is.infinite(lower) & lower == upper & sign(upper) == 1)) {
    cli::cli_abort("Not both elements in `{label}` can be +Inf", call = call)
  }
  if (any(is.infinite(lower) &
          lower == upper & sign(upper) == -1)) {
    cli::cli_abort("Not both elements in `{label}` can be -Inf", call = call)
  }

  window
}

#' Validate the ageGroup argument. It must be a list of two integerish numbers
#' lower age and upper age, both of the must be greater or equal to 0 and lower
#' age must be lower or equal to the upper age. If not named automatic names
#' will be given in the output list.
#'
#' @param ageGroup age group in a list.
#' @param multipleAgeGroup Allow multiple age groups.
#' @param overlap allow overlapping ageGroup.
#' @param null null age group allowed true or false.
#' @inheritParams emptyDoc
#' @param ageGroupName Name of the default age group.
#' @param nm Name to use in error messages. Defaults to the expression supplied
#' to `ageGroup`.
#' @inheritParams cliCallDoc
#'
#' @return validate ageGroup
#' @export
#' @examples
#' validateAgeGroupArgument(list(c(0, 39), c(40, Inf)))
#'
validateAgeGroupArgument <- function(ageGroup,
                                     multipleAgeGroup = TRUE,
                                     overlap = FALSE,
                                     null = TRUE,
                                     empty = TRUE,
                                     ageGroupName = "age_group",
                                     nm = deparse1(substitute(ageGroup), backtick = TRUE),
                                     call = parent.frame()) {
  # initial checks
  assertLogical(null, length = 1, call = call)
  assertLogical(multipleAgeGroup, length = 1, call = call)
  assertLogical(overlap, length = 1, call = call)
  assertCharacter(ageGroupName, length = 1, call = call)

  if (is.null(ageGroup)) {
    if (null) {
      return(NULL)
    } else {
      cli::cli_abort("`{nm}` argument can not be NULL.", call = call)
    }
  }
  if (length(ageGroup) == 0) {
    if (empty) {
      return(NULL)
    } else {
      cli::cli_abort("`{nm}` argument can not be empty.", call = call)
    }
  }

  # convert to list of lists if it is not
  if (is.numeric(ageGroup)) {
    ageGroup <- list(list(ageGroup))
  } else if (rlang::is_bare_list(ageGroup)) {
    if (length(ageGroup) == 0) {
      if (empty) {
        return(NULL)
      } else {
        cli::cli_abort("`{nm}` argument can not be empty.", call = call)
      }
    } else if (is.numeric(ageGroup[[1]])) {
      ageGroup <- list(ageGroup)
    }
  } else {
    cli::cli_abort("`{nm}` must be a list of age groups.", call = call)
  }

  len <- length(ageGroup)

  # check multiple age group
  if (!multipleAgeGroup & len > 1) {
    cli::cli_abort("Multiple `{nm}` groups are not allowed", call = call)
  }

  # correct individual age groups
  ageGroup <- ageGroup |>
    purrr::map(\(x) correctAgeGroup(x, overlap = overlap, label = nm, call = call))

  # correct age group names
  nms <- names(ageGroup)
  if (is.null(nms)) nms <- rep("", len)
  if (len == 1 & identical(nms, "")) {
    names(ageGroup) <- ageGroupName
  } else {
    for (k in seq_len(len)) {
      if (nms[k] == "") nms[k] <- paste0(ageGroupName, "_", k)
    }
    names(ageGroup) <- nms
  }

  return(ageGroup)
}
correctAgeGroup <- function(ageGroup,
                            overlap,
                            label,
                            call) {
  len <- length(ageGroup)

  # assert numeric
  isNumeric <- purrr::map_lgl(ageGroup, is.numeric) |>
    all()
  if (!isNumeric) {
    "Elements of `{label}` argument are not numeric." |>
      cli::cli_abort(call = call)
  }

  # correct length 1
  ageGroup <- purrr::map(ageGroup, \(x) {
    if (length(x) == 1) rep(x, 2) else x
  })

  # length 2
  if (any(lengths(ageGroup) != 2)) {
    "Elements of `{label}` must have length 2." |>
      cli::cli_abort(call = call)
  }

  allValues <- unlist(ageGroup)

  # no NA
  if (any(is.na(allValues))) {
    "Elements of `{label}` argument can not contain NA." |>
      cli::cli_abort(call = call)
  }

  # assert integerish
  if (!isIntegerish(allValues)) {
    "Elements of `{label}` argument must be integerish." |>
      cli::cli_abort(call = call)
  }

  # convert to numeric as Inf can not be integer
  ageGroup <- purrr::map(ageGroup, as.numeric)

  # positive
  if (any(unlist(ageGroup) < 0L)) {
    "Elements of `{label}` argument must be greater or equal to {.val 0}." |>
      cli::cli_abort(call = call)
  }

  # min <= max
  isMinBigger <- purrr::map_lgl(ageGroup, \(x) x[1] > x[2]) |>
    any()
  if (isMinBigger) {
    "First element of `{label}` argument must be smaller or equal than the second one." |>
      cli::cli_abort(call = call)
  }

  # overlap
  if (!overlap & len > 1) {
    for (i in 1:(len - 1)) {
      for (j in (i + 1):len) {
        if (thereIsOverlap(ageGroup[[i]], ageGroup[[j]])) {
          "`{label}` must not contain overlap between groups." |>
            cli::cli_abort(call = call)
        }
      }
    }
  }

  # add names
  if (is.null(names(ageGroup))) {
    nms <- rep("", len)
  } else {
    nms <- names(ageGroup)
  }
  for (k in seq_len(len)) {
    if (nms[k] == "") nms[k] <- nameAgeGroup(ageGroup[[k]])
  }
  names(ageGroup) <- nms

  return(ageGroup)
}
isIntegerish <- function(x) {
  if (is.integer(x)) {
    return(TRUE)
  }
  xInt <- x[!is.infinite(x)]
  err <- max(abs(xInt - round(xInt)))
  err < 0.0001
}
thereIsOverlap <- function(x, y) {
  if (x[1] < y[1] & x[2] < y[1]) {
    return(FALSE)
  }
  if (y[1] < x[1] & y[2] < x[1]) {
    return(FALSE)
  }
  TRUE
}
nameAgeGroup <- function(x) {
  if (x[1] == 0L & is.infinite(x[2])) {
    return("overall")
  }
  if (is.infinite(x[2])) {
    return(paste(x[1], "or above"))
  }
  paste(x[1], "to", x[2])
}

#' Validate if an object in a valid cdm_reference.
#'
#' @inheritParams cdmDoc
#' @param checkOverlapObservation TRUE to perform check on no overlap
#' observation period
#' @param checkStartBeforeEndObservation TRUE to perform check on correct
#' observational start and end date
#' @param checkPlausibleObservationDates TRUE to perform check that there are
#' no implausible observation period start dates (before 1800-01-01) or end
#' dates (after the current date)
#' @param checkPerson TRUE to perform check on person id in all clinical table
#' are in person table
#' @param requiredTables Name of tables that are required to be part of the
#' cdm_reference object.
#' @inheritParams emptyDoc
#' @inheritParams validationDoc
#' @param nm Name to use in error messages. Defaults to the expression supplied
#' to `cdm`.
#' @inheritParams cliCallDoc
#'
#' @return A cdm_reference object
#' @export
#' @examples
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = dplyr::tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = dplyr::tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' validateCdmArgument(cdm)
#'
validateCdmArgument <- function(cdm,
                                checkOverlapObservation = FALSE,
                                checkStartBeforeEndObservation = FALSE,
                                checkPlausibleObservationDates = FALSE,
                                checkPerson = FALSE,
                                requiredTables = character(),
                                empty = TRUE,
                                validation = "error",
                                nm = deparse1(substitute(cdm), backtick = TRUE),
                                call = parent.frame()) {
  # input validation
  assertValidation(validation, call = call)
  assertLogical(checkOverlapObservation, length = 1, call = call)
  assertLogical(checkStartBeforeEndObservation, length = 1, call = call)
  assertLogical(checkPlausibleObservationDates, length = 1, call = call)
  assertLogical(checkPerson, length = 1, call = call)
  assertCharacter(requiredTables, empty = TRUE, call = call)

  # assert class
  assertClass(
    cdm, class = c("cdm_reference"), all = TRUE, empty = empty, nm = nm,
    call = call
  )

  # not overlapping periods
  if (isTRUE(checkOverlapObservation)) {
    checkOverlapObservation(cdm$observation_period)
  }

  # no start observation before end
  if (isTRUE(checkStartBeforeEndObservation)) {
    checkStartBeforeEndObservation(cdm$observation_period)
  }

  if (isTRUE(checkPlausibleObservationDates)) {
    checkPlausibleObservationDates(cdm$observation_period)
  }

  if (isTRUE(checkPerson)) {
    checkPerson(cdm = cdm, call = call)
  }

  notPresent <- requiredTables[!requiredTables %in% names(cdm)]
  if (length(notPresent) > 0) {
    "Required tables not present in `{nm}`: {.pkg {notPresent}}." |>
      cli::cli_abort(call = call)
  }

  return(cdm)
}

#' Validate whether an object is a valid 'summarised_result' object.
#'
#' @param result summarised_result object to validate.
#' @param checkNoDuplicates Whether duplicates are not allowed in the
#' result object.
#' @param checkNameLevel Whether the name-level paired columns can be
#' correctly split.
#' @param checkSuppression Whether the suppression in the result object is well
#' defined.
#' @inheritParams emptyDoc
#' @param validation Only error is supported at the moment.
#' @param nm Name to use in error messages. Defaults to the expression supplied
#' to `result`.
#' @inheritParams cliCallDoc
#'
#' @return A summarised_result object.
#' @export
#' @examples
#' x <- dplyr::tibble(
#'   "result_id" = 1L,
#'   "cdm_name" = "eunomia",
#'   "group_name" = "cohort_name",
#'   "group_level" = "my_cohort",
#'   "strata_name" = c("sex", "sex &&& age_group", "sex &&& year"),
#'   "strata_level" = c("Female", "Male &&& <40", "Female &&& 2010"),
#'   "variable_name" = "number subjects",
#'   "variable_level" = NA_character_,
#'   "estimate_name" = "count",
#'   "estimate_type" = "integer",
#'   "estimate_value" = c("100", "44", "14"),
#'   "additional_name" = "overall",
#'   "additional_level" = "overall"
#' ) |>
#'   newSummarisedResult()
#'
#' validateResultArgument(x)
#'
validateResultArgument <- function(result,
                                   checkNoDuplicates = FALSE,
                                   checkNameLevel = FALSE,
                                   checkSuppression = FALSE,
                                   empty = TRUE,
                                   validation = "error",
                                   nm = deparse1(substitute(result), backtick = TRUE),
                                   call = parent.frame()) {
  assertTrue(validation == "error", call = call)
  assertTable(
    result, class = "summarised_result", empty = empty, nm = nm, call = call
  )

  validateResultSettings(attr(result, "settings"), call = call)

  result <- result |>
    validateSummarisedResultTable(
      duplicates = checkNoDuplicates,
      pairs = checkNameLevel,
      duplicateEstimates = checkNoDuplicates,
      suppressPossibility = checkSuppression,
      call = call
    )

  return(result)
}

#' Validate a new column of a table
#'
#' @param table The table to check if the column already exists.
#' @param column Character vector with the name(s) of the new column(s).
#' @inheritParams emptyDoc
#' @inheritParams validationDoc
#' @param nm Name to use in error messages. Defaults to the expression supplied
#' to `column`.
#' @inheritParams cliCallDoc
#'
#' @return table without conflicting columns.
#' @export
#' @examples
#' x <- dplyr::tibble(
#'   column1 = c(1L, 2L),
#'   column2 = c("a", "b")
#' )
#' validateNewColumn(x, "not_exiting_column")
#' validateNewColumn(x, "column1")
#'
validateNewColumn <- function(table,
                              column,
                              empty = TRUE,
                              validation = "warning",
                              nm = deparse1(substitute(column), backtick = TRUE),
                              call = parent.frame()) {
  # input check
  cols <- colnames(table)
  assertCharacter(column, empty = empty, nm = nm, call = call)
  assertValidation(validation, call = call)

  # assert if they exist
  eliminate <- column[column %in% cols]
  if (length(eliminate) > 0) {
    if (validation == "error") {
      cli::cli_abort(c("x" = "columns {.var {eliminate}} already exist in the table. Remove or rename new columns."), call = call)
    } else if (validation == "warning") {
      cli::cli_warn(c("!" = "columns {.var {eliminate}} already exist in the table. They will be overwritten."), call = call)
      table <- table |>
        dplyr::select(!dplyr::all_of(eliminate))
    }
  }

  # output table or sc_column
  return(table)
}

#' Validate whether a variable points to a certain existing column in a table.
#'
#' @param column Name of a column that you want to check exists in `x`
#' table.
#' @param x Table to check if the column exists.
#' @param type Type of the column.
#' @inheritParams validationDoc
#' @param null Whether `NULL` is accepted.
#' @inheritParams emptyDoc
#' @param nm Name to use in error messages. Defaults to the expression supplied
#' to `column`.
#' @inheritParams cliCallDoc
#'
#' @return the validated name
#' @export
#'
#' @examples
#' x <- dplyr::tibble(a = 1, b = "xxx")
#'
#' validateColumn("a", x, validation = "warning")
#' validateColumn("a", x, type = "character", validation = "warning")
#' validateColumn("a", x, type = "numeric", validation = "warning")
#' validateColumn("not_existing", x, type = "numeric", validation = "warning")
#'
validateColumn <- function(column,
                           x,
                           type = c("character", "date", "logical", "numeric", "integer"),
                           validation = "error",
                           null = FALSE,
                           empty = TRUE,
                           nm = deparse1(substitute(column), backtick = TRUE),
                           call = parent.frame()) {
  assertLogical(null, length = 1, call = call)
  assertValidation(validation, call = call)
  assertChoice(
    type, c("character", "date", "logical", "numeric", "integer"),
    call = call
  )

  if (isTRUE(null) && is.null(column)) {
    return(NULL)
  }

  assertCharacter(
    column, length = 1, nm = nm, null = null, empty = empty, call = call
  )
  if (length(column) == 0) {
    return(column)
  }
  assertTable(x, empty = TRUE, call = call)

  if (!column %in% colnames(x)) {
    report("{column} column does not exist.", validation = validation, call = call)
    return(column)
  }

  types <- x |>
    dplyr::select(dplyr::all_of(column)) |>
    utils::head(1) |>
    dplyr::pull() |>
    dplyr::type_sum() |>
    assertClassification()
  if (isFALSE(any(type %in% types))) {
    report("{column} type must be a choice of: {.var {type}}; but it is {.pkg {types}}.", validation = validation, call = call)
    return(column)
  }

  return(column)
}

#' To check whether an object is already suppressed to a certain min cell count.
#'
#' @param result The suppressed result to check
#' @param minCellCount  Minimum count of records used when suppressing
#'
#' @return Warning or message with check result
#' @export
#' @examples
#' x <- dplyr::tibble(
#'   "result_id" = 1L,
#'   "cdm_name" = "eunomia",
#'   "group_name" = "cohort_name",
#'   "group_level" = "my_cohort",
#'   "strata_name" = c("sex", "sex &&& age_group", "sex &&& year"),
#'   "strata_level" = c("Female", "Male &&& <40", "Female &&& 2010"),
#'   "variable_name" = "number subjects",
#'   "variable_level" = NA_character_,
#'   "estimate_name" = "count",
#'   "estimate_type" = "integer",
#'   "estimate_value" = c("100", "44", "14"),
#'   "additional_name" = "overall",
#'   "additional_level" = "overall"
#' ) |>
#'   newSummarisedResult()
#'
#' isResultSuppressed(x)
#'
isResultSuppressed <- function(result, minCellCount = 5) {
  # initial checks
  validateResultArgument(result, empty = TRUE)
  assertNumeric(minCellCount, length = 1, integerish = TRUE)

  # retrieve settings
  set <- settings(result)
  if (nrow(set) == 0) {
    return(TRUE)
  }
  if (!"min_cell_count" %in% colnames(set)) {
    cli::cli_warn("Column {.var min_cell_count} is missing in settings, result is not suppressed.")
    return(FALSE)
  }

  set <- set |>
    dplyr::select("result_id", "min_cell_count") |>
    dplyr::mutate("min_cell_count" = as.integer(.data$min_cell_count))

  if (all(minCellCount == unique(set$min_cell_count))) {
    cli::cli_inform(c(
      "v" = "The {.cls summarised_result} is suppressed with minCellCount = {minCellCount}."
    ))
    return(TRUE)
  } else {
    idSup <- set$result_id[set$min_cell_count == minCellCount]
    idNotSup <- set$result_id[set$min_cell_count == 0 | is.na(set$min_cell_count)]
    idSupLow <- set$result_id[set$min_cell_count > 0 & set$min_cell_count < minCellCount]
    idSupUpp <- set$result_id[set$min_cell_count > minCellCount]
    addMesSup(character(), idSup, result, "v", glue::glue("suppressed minCellCount = {minCellCount}")) |>
      addMesSup(idNotSup, result, "x", "not suppressed") |>
      addMesSup(idSupLow, result, "x", glue::glue("suppressed with minCellCount < {minCellCount}")) |>
      addMesSup(idSupUpp, result, "!", glue::glue("suppressed with minCellCount > {minCellCount}")) |>
      cli::cli_warn()
    return(FALSE)
  }
}
addMesSup <- function(mes, ids, result, lab, err) {
  if (length(ids) == 0) {
    return(mes)
  }
  ncounts <- sum(result$result_id %in% ids)
  ms <- "{length(ids)} set{?s} ({ncounts} row{?s}) {err}." |>
    cli::cli_text() |>
    cli::cli_fmt() |>
    as.character()
  c(mes, rlang::set_names(ms, lab))
}

#' Validate `nameStyle` argument. If any of the element in `...` has length
#' greater than 1 it must be contained in nameStyle. Note that snake case
#' notation is used.
#'
#' @param nameStyle A character vector. It must contain all the `...` elements
#' in snake_case format and between `{}`.
#' @param ... Elements to be included.
#' @inheritParams emptyDoc
#' @param nm Name to use in error messages. Defaults to the expression supplied
#' to `nameStyle`.
#' @inheritParams cliCallDoc
#'
#' @export
#' @return invisible nameStyle.
#' @examples
#' validateNameStyle(
#'   nameStyle = "hi_{cohort_name}",
#'   cohortName = c("cohort1", "cohort2"),
#'   otherVariable = c("only 1 value")
#' )
#'
#' \dontrun{
#' validateNameStyle(
#'   nameStyle = "hi_{cohort_name}",
#'   cohortName = c("cohort1", "cohort2"),
#'   otherVariable = c("value1", "value2")
#' )
#' }
#' validateNameStyle(
#'   nameStyle = "{other_variable}_hi_{cohort_name}",
#'   cohortName = c("cohort1", "cohort2"),
#'   otherVariable = c("value1", "value2")
#' )
#'
validateNameStyle <- function(nameStyle,
                              ...,
                              empty = TRUE,
                              nm = deparse1(substitute(nameStyle), backtick = TRUE),
                              call = parent.frame()) {
  assertCharacter(nameStyle, length = 1, empty = empty, nm = nm, call = call)
  if (length(nameStyle) == 0) {
    return(invisible(nameStyle))
  }
  elementsInDots <- list(...)
  assertList(elementsInDots, class = "character", empty = TRUE, call = call)

  elementsInDots <- elementsInDots |>
    purrr::keep(\(x) length(x) > 1) |>
    names() |>
    toSnakeCase()
  elementsInNameStyle <- stringr::str_extract_all(nameStyle, "\\{([^}]+)\\}") |>
    dplyr::first() |>
    purrr::map_chr(\(x) substr(x, 2, nchar(x) - 1))
  missingElements <- elementsInDots[!elementsInDots %in% elementsInNameStyle]
  if (length(missingElements) > 0) {
    c("!" = "{missingElements} must be included in `{nm}`.",
      "*" = "elements in `...`: {.var {elementsInDots}}.",
      "*" = "elements in `{nm}`: {.var {elementsInNameStyle}}.") |>
      cli::cli_abort(call = call)
  }
  return(invisible(nameStyle))
}

#' To validate a strata list. It makes sure that elements are unique and point
#' to columns in table.
#'
#' @param strata A list of characters that point to columns in table.
#' @param table A table with columns.
#' @inheritParams emptyDoc
#' @param nm Name to use in error messages. Defaults to the expression supplied
#' to `strata`.
#' @inheritParams cliCallDoc
#'
#' @return The same strata input or an error if the input is incorrect.
#' @export
#'
#' @examples
#' strata <- list("age", "sex", c("age", "sex"))
#' x <- dplyr::tibble(age = 30L, sex = "Female")
#'
#' validateStrataArgument(strata, x)
#'
validateStrataArgument <- function(strata,
                                   table,
                                   empty = TRUE,
                                   nm = deparse1(substitute(strata), backtick = TRUE),
                                   call = parent.frame()) {
  if (length(strata) == 0) {
    assertList(list(), class = "character", empty = empty, nm = nm, call = call)
    assertTable(table, empty = TRUE, call = call)
    return(list())
  }
  if (is.character(strata)) {
    strata <- list(strata)
  }
  assertList(strata, class = "character", empty = empty, nm = nm, call = call)
  assertTable(table, empty = TRUE, call = call)
  cols <- colnames(table)

  strataUnique <- unique(strata)
  n <- length(strata) - length(strataUnique)
  if (n > 0) {
    cli::cli_warn("{n} elements of strata eliminated because they were repeated.", call = call)
  }

  strataCols <- unique(purrr::flatten_chr(strataUnique))
  notPresent <- strataCols[!strataCols %in% cols]
  if (length(notPresent) > 0) {
    "Elements in `{nm}` not present as columns in table: {.var {notPresent}}." |>
      cli::cli_abort(call = call)
  }

  return(strataUnique)
}
