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
#' the a cdm object to check `name` is not already used in that cdm.
#'
#' @param name Name of a new table to be added to a cdm object.
#' @param cdm A cdm_reference object. It will check if a table named name
#' already exists in the cdm.
#' @param null If TRUE, name can be NULL
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
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
                                 call = parent.frame()) {
  assertValidation(validation)
  if (isTRUE(null) && (is.null(name) | isTRUE(is.na(name)))) {
    return(NULL)
  }

  nm <- substitute(name) |> utils::capture.output()
  assertCharacter(name, length = 1, call = call)
  newName <- toSnakeCase(name)
  if (newName != name) {
    if (validation == "error") {
      cli::cli_abort(c("!" = "`{nm}` is not snake_case it should be modified to: {newName}"))
    } else if (validation == "warning") {
      cli::cli_warn(c("!" = "`{nm}` was modified: {name} -> {newName}"))
    }
  }
  if (!is.null(cdm)) {
    if (newName %in% names(cdm)) {
      if (validation == "error") {
        cli::cli_abort(c("!" = "There already exist a table named {.var {newName}}."))
      } else if (validation == "warning") {
        cli::cli_warn(c("!" = "There already exist a table named {.var {newName}}. It will be overwritten."))
      }
    }
  }
  return(newName)
}

#' Validate a cohort table input.
#'
#' @param cohort Object to be validated as a valid cohort input.
#' @param checkEndAfterStart If TRUE a check that all cohort end dates come on
#' or after cohort start date will be performed.
#' @param checkOverlappingEntries If TRUE a check that no individuals have
#' overlapping cohort entries will be performed.
#' @param checkMissingValues If TRUE a check that there are no missing values in
#' required fields will be performed.
#' @param checkInObservation If TRUE a check that cohort entries are within
#' the individuals observation periods will be performed.
#' @param checkAttributes Whether to check if attributes are present and
#' populated correctly.
#' @param checkPermanentTable Whether to check if the table has to be a
#' permanent table.
#' @param dropExtraColumns Whether to drop extra columns that are not the
#' required ones.
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
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
                                   validation = "error",
                                   call = parent.frame()) {
  assertValidation(validation)
  assertLogical(checkEndAfterStart, length = 1)
  assertLogical(checkOverlappingEntries, length = 1)
  assertLogical(checkMissingValues, length = 1)
  assertLogical(checkInObservation, length = 1)
  assertLogical(checkAttributes, length = 1)
  assertLogical(dropExtraColumns, length = 1)
  assertLogical(checkPermanentTable, length = 1)

  assertClass(cohort, class = c("cohort_table", "cdm_table"), all = TRUE, call = call)

  if (is.na(tableName(cohort)) & checkPermanentTable) {
    missingCohortTableNameError(cdmReference(cohort), validation = validation)
  }

  # columns
  notPresent <- cohortColumns("cohort")[!cohortColumns("cohort") %in% colnames(cohort)]
  if (length(notPresent) > 0) {
    if (validation == "error") {
      cli::cli_abort(c("!" = "columns: {.var {notPresent}} not present in cohort object"), call = call)
    } else if (validation == "warning") {
      cli::cli_warn(c("!" = "columns: {.var {notPresent}} not present in cohort object"), call = call)
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
      cli::cli_warn(c("!" = "Extra columns dropped: {.var {extraColumns}}."))
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
#' value, a cohort_name or a tidyselect expression referinc to cohort_names. If
#' you want to support tidyselect expressions please use the function as:
#' `validateCohortIdArgument({{cohortId}}, cohort)`.
#'
#' @param cohortId A cohortId vector to be validated.
#' @param cohort A cohort_table object.
#' @param null Whether `NULL` is accepted. If NULL all `cohortId` will be
#' returned.
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
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
                                     validation = "error",
                                     call = parent.frame()) {
  assertValidation(validation)
  assertClass(cohort, class = "cohort_table", call = call)
  assertLogical(null, length = 1)

  set <- settings(cohort)

  if (isTidySelect(rlang::enquo(cohortId))) {
    cohortId <- selectTables(set$cohort_name, cohortId)
  }

  if (is.null(cohortId) & !null) {
    cli::cli_abort(c("x" = "`NULL` is not allowed for {.var cohortId}"))
  }

  if (is.null(cohortId)) {
    cohortId <- set$cohort_definition_id
  } else if (is.numeric(cohortId)) {
    cohortId <- as.integer(cohortId)
    areIn <- cohortId %in% set$cohort_definition_id
    notPresent <- cohortId[!areIn]
    cohortId <- cohortId[areIn]
    if (length(notPresent) > 0) {
      report(
        message = "cohort definition id: {notPresent} not defined in settings.",
        validation = validation,
        call = call
      )
    }
    cohortId <- set$cohort_definition_id[getId(set$cohort_definition_id, cohortId)]
  } else if (is.character(cohortId)) {
    areIn <- cohortId %in% set$cohort_name
    notPresent <- cohortId[!areIn]
    cohortId <- cohortId[areIn]
    if (length(notPresent) > 0) {
      report(
        message = "cohort name: {notPresent} not defined in settings.",
        validation = validation,
        call = call
      )
    }
    cohortId <- set$cohort_definition_id[getId(set$cohort_name, cohortId)]
  } else {
    cli::cli_abort("{.arg cohortId} can either be an integer, a character, a tidyselect expression or NULL.")
  }

  if (length(cohortId) == 0) {
    report(message = "cohortId is empty.", validation = validation, call = call)
  }

  return(cohortId)
}
isTidySelect <- function(arg) {
  # check if call
  isCall <- rlang::quo_is_call(arg)

  # selection functions that we want to support
  tidyFunctions <- c(
    "starts_with", "contains", "ends_with", "matches",
    "everything", "all_of", "any_of"
  )

  if (isCall) {
    fn <- as.character(rlang::quo_get_expr(arg))[1] |>
      removePackageName()
    return(fn %in% tidyFunctions)
  }

  return(FALSE)
}
removePackageName <- function(x) {
  x <- stringr::str_split_1(x, "::")
  x[length(x)]
}
getId <- function(x, ids) {
  purrr::map_int(ids, \(xx) which(x == xx))
}

#' Validate conceptSet argument. It can either be a list, a codelist, a
#' concept set expression or a codelist with details. The output will always be a
#' codelist.
#'
#' @param conceptSet It can be either a named list of concepts or a codelist,
#' codelist_with_details or concept_set_expression object.
#' @param cdm A cdm_reference object, needed if a concept_set_expression is
#' provided.
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
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
                                       validation = "error",
                                       call = parent.frame()) {
  if (is.null(conceptSet)) return(emptyCodelist())
  if (inherits(conceptSet, "codelist")) {
    conceptSet <- validateCodelist(conceptSet, call = call)
  } else if (inherits(conceptSet, "codelist_with_details")) {
    conceptSet <- validateCodelistWithDetails(conceptSet, call) |>
      purrr::map(\(x) dplyr::pull(x, "concept_id")) |>
      newCodelist()
  } else if (inherits(conceptSet, "concept_set_expression")) {
    concepts <- validateConceptSetExpression(conceptSet, call)
    concepts <- concepts |>
      lapply(dplyr::select, c("concept_id", "excluded", "descendants")) |>
      dplyr::bind_rows(.id = "concept_name")
    descendants <- concepts |>
      dplyr::filter(.data$descendants == TRUE) |>
      dplyr::select(-"descendants")
    if (nrow(descendants) > 0) {
      assertClass(cdm, "cdm_reference")
      nm <- uniqueTableName()
      cdm <- insertTable(cdm = cdm, name = nm, table = descendants)
      descendants <- cdm$concept_ancestor |>
        dplyr::rename("to_join" = "ancestor_concept_id") |>
        dplyr::inner_join(
          cdm[[nm]] |>
            dplyr::rename("to_join" = "concept_id"),
          by = "to_join"
        ) |>
        dplyr::select(
          "concept_id" = "descendant_concept_id", "excluded", "concept_name"
        ) |>
        dplyr::collect()
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
        by = c("concept_id", "concept_name")
      ) |>
      dplyr::group_by(.data$concept_name) |>
      dplyr::group_split() |>
      as.list()
    names(conceptSet) <- purrr::map_chr(conceptSet, \(x) unique(x$concept_name))
    conceptSet <- conceptSet |>
      purrr::map(\(x) unique(x$concept_id)) |>
      newCodelist()
  } else {
    conceptSet <- newCodelist(conceptSet)
  }
  return(conceptSet)
}
assertValidation <- function(validation, call = parent.frame()) {
  validation |>
    assertChoice(choices = c("error", "warning"), length = 1, call = call)
}

#' Validate a window argument. It must be a list of two elements (window start
#' and window end), both must be integerish and window start must be lower or
#' equal than window end.
#'
#' @param window time window
#' @param snakeCase return default window  name in snake case if TRUE
#' @param call A call argument to pass to cli functions.
#'
#' @return time window
#' @export
#' @examples
#' validateWindowArgument(list(c(0, 15), c(-Inf, Inf)))
#' validateWindowArgument(list(c(0, 15), c(-Inf, Inf)), snakeCase = FALSE)
#'
validateWindowArgument <- function(window,
                                   snakeCase = TRUE,
                                   call = parent.frame()) {
  assertLogical(snakeCase, length = 1, call = call)

  if (!is.list(window)) {
    window <- list(window)
  }

  # Find if any NA, throw warning that it will be changed to Inf, change it later
  if (any(unlist(lapply(window, is.na)))) {
    cli::cli_abort("NA found in window, please use Inf or -Inf instead", call = call)
  }

  assertList(window, call = call)
  elements <- window |>
    unlist() |>
    purrr::keep(\(x) !is.infinite(x)) |>
    unique()
  assertNumeric(elements, integerish = TRUE, call = call, msg = "Elements of window must be integerish.")

  # if any element of window list has length over 2, throw error
  if (any(lengths(window) > 2)) {
    "window can only contain two values: windowStart and windowEnd" |>
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

  assertWindowName(window, snakeCase, call = call)
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
        cli::cli_inform(c("window names casted to snake_case: ", changes))
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
assertWindowName <- function(window, snakeCase, call = parent.frame()) {
  names(window) <- getWindowNames(window, snakeCase = snakeCase)
  lower <- purrr::map_dbl(window, \(x) x[1])
  upper <- purrr::map_dbl(window, \(x) x[2])

  if (any(lower > upper)) {
    "First element in window must be smaller or equal to the second one" |>
      cli::cli_abort(call = call)
  }
  if (any(is.infinite(lower) & lower == upper & sign(upper) == 1)) {
    cli::cli_abort("Not both elements in the window can be +Inf", call = call)
  }
  if (any(is.infinite(lower) &
          lower == upper & sign(upper) == -1)) {
    cli::cli_abort("Not both elements in the window can be -Inf", call = call)
  }

  window
}

#' Validate the ageGroup argument. It must be a list of two integerish numbers
#' lower age and upper age, both of the must be greater or equal to 0 and lower
#' age must be lower or equal to the upper age. If not named automatic names
#' will be given in the output list.
#'
#' @param ageGroup age group in a list.
#' @param multipleAgeGroup allow mutliple age group.
#' @param overlap allow overlapping ageGroup.
#' @param null null age group allowed true or false.
#' @param ageGroupName Name of the default age group.
#' @param call parent frame.
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
                                     ageGroupName = "age_group",
                                     call = parent.frame()) {
  # initial checks
  assertLogical(null, length = 1)
  assertLogical(multipleAgeGroup, length = 1)
  assertLogical(overlap, length = 1)
  assertCharacter(ageGroupName, length = 1)

  if (is.null(ageGroup)) {
    if (null) {
      return(NULL)
    } else {
      cli::cli_abort("`ageGroup` argument can not be NULL.", call = call)
    }
  }

  # convert to list of lists if it is not
  if (is.numeric(ageGroup)) {
    ageGroup <- list(list(ageGroup))
  } else if (rlang::is_bare_list(ageGroup)) {
    if (length(ageGroup) == 0) {
      return(NULL)
    } else if (is.numeric(ageGroup[[1]])) {
      ageGroup <- list(ageGroup)
    }
  } else {
    cli::cli_abort("`ageGroup` must be a list of age groups.", call = call)
  }

  len <- length(ageGroup)

  # check multiple age group
  if (!multipleAgeGroup & len > 1) {
    cli::cli_abort("Multiple age group are not allowed", call = call)
  }

  # correct individual age groups
  ageGroup <- ageGroup |>
    purrr::map(\(x) correctAgeGroup(x, overlap = overlap, call = call))

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
                            call) {
  len <- length(ageGroup)

  # assert numeric
  isNumeric <- purrr::map_lgl(ageGroup, is.numeric) |>
    all()
  if (!isNumeric) {
    "Elements of `ageGroup` argument are not numeric." |>
      cli::cli_abort(call = call)
  }

  # correct length 1
  ageGroup <- purrr::map(ageGroup, \(x) {
    if (length(x) == 1) rep(x, 2) else x
  })

  # length 2
  if (any(lengths(ageGroup) != 2)) {
    "Elements of `ageGroup` must have length 2." |>
      cli::cli_abort(call = call)
  }

  allValues <- unlist(ageGroup)

  # no NA
  if (any(is.na(allValues))) {
    "Elements of `ageGroup` argument can not contain NA." |>
      cli::cli_abort(call = call)
  }

  # assert integerish
  if (!isIntegerish(allValues)) {
    "Elements of `ageGroup` argument must be integerish." |>
      cli::cli_abort(call = call)
  }

  # convert to numeric as Inf can not be integer
  ageGroup <- purrr::map(ageGroup, as.numeric)

  # positive
  if (any(unlist(ageGroup) < 0L)) {
    "Elements of `ageGroup` argument must be greater or equal to {.val 0}." |>
      cli::cli_abort(call = call)
  }

  # min <= max
  isMinBigger <- purrr::map_lgl(ageGroup, \(x) x[1] > x[2]) |>
    any()
  if (isMinBigger) {
    "First element of `ageGroup` argument must be smaller or equal than the second one." |>
      cli::cli_abort(call = call)
  }

  # overlap
  if (!overlap & len > 1) {
    for (i in 1:(len - 1)) {
      for (j in (i + 1):len) {
        if (thereIsOverlap(ageGroup[[i]], ageGroup[[j]])) {
          "`ageGroup` must not contain overlap between groups." |>
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
#' @param cdm A cdm_reference object
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
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
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
                                validation = "error",
                                call = parent.frame()) {
  # input validation
  assertValidation(validation)
  assertLogical(checkOverlapObservation, length = 1)
  assertLogical(checkStartBeforeEndObservation, length = 1)
  assertCharacter(requiredTables)

  # assert class
  assertClass(cdm, class = c("cdm_reference"), all = TRUE, call = call)

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
    "Required tables not present in the cdm object: {.pkg {notPresent}}." |>
      cli::cli_abort(call = call)
  }

  return(cdm)
}

#' Validate if a an object is a valid 'summarised_result' object.
#'
#' @param result summarised_result object to validate.
#' @param checkNoDuplicates Whether there are not allowed duplicates in the
#' result object.
#' @param checkNameLevel Whether the name-level paired columns are can be
#' correctly split.
#' @param checkSuppression Whether the suppression in the result object is well
#' defined.
#' @param validation Only error is supported at the moment.
#' @param call parent.frame
#'
#' @return summarise result object
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
                                   validation = "error",
                                   call = parent.frame()) {
  assertTrue(validation == "error")
  assertTable(result, class = "summarised_result", call = call)

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
#' @param validation Whether to throw warning or error.
#' @param call Passed to cli functions.
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
                              validation = "warning",
                              call = parent.frame()) {
  # input check
  cols <- colnames(table)
  assertCharacter(column)
  assertValidation(validation)

  # assert if they exist
  eliminate <- column[column %in% cols]
  if (length(eliminate) > 0) {
    if (validation == "error") {
      cli::cli_abort(c("x" = "columns {.var {eliminate}} already exist in the table. Remove or rename new columns."), call = call)
    } else if (validation == "warning") {
      cli::cli_warn(c("!" = "columns {.var {eliminate}} already exist in the table. They will be overwritten."))
      table <- table |>
        dplyr::select(!dplyr::all_of(eliminate))
    }
  }

  # output table or sc_column
  return(table)
}

#' Validate whether a variable points to a certain exiting column in a table.
#'
#' @param column Name of a column that you want to check that exist in `x`
#' table.
#' @param x Table to check if the column exist.
#' @param type Type of the column.
#' @param validation Whether to throw warning or error.
#' @param call Passed to cli functions.
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
                           call = parent.frame()) {
  assertCharacter(column, length = 1, call = call)
  assertTable(x)
  assertValidation(validation)
  assertChoice(type, c("character", "date", "logical", "numeric", "integer"))

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
  validateResultArgument(result)
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
#' @param call Passed to cli functions.
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
                              call = parent.frame()) {
  assertCharacter(nameStyle, length = 1, call = call)
  elementsInDots <- list(...)
  assertList(elementsInDots, class = "character", call = call)

  elementsInDots <- elementsInDots |>
    purrr::keep(\(x) length(x) > 1) |>
    names() |>
    toSnakeCase()
  elementsInNameStyle <- stringr::str_extract_all(nameStyle, "\\{([^}]+)\\}") |>
    dplyr::first() |>
    purrr::map_chr(\(x) substr(x, 2, nchar(x) - 1))
  missingElements <- elementsInDots[!elementsInDots %in% elementsInNameStyle]
  if (length(missingElements) > 0) {
    c("!" = "{missingElements} must be included in `nameStyle`.",
      "*" = "elements in `...`: {.var {elementsInDots}}.",
      "*" = "elements in `nameStyle`: {.var {elementsInNameStyle}}.") |>
      cli::cli_abort(call = call)
  }
  return(invisible(nameStyle))
}

#' To validate a strata list. It makes sure that elements are unique and point
#' to columns in table.
#'
#' @param strata A list of characters that point to columns in table.
#' @param table A table with columns.
#' @param call Passed to cli functions.
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
                                   call = parent.frame()) {
  if (is.character(strata)) {
    strata <- list(strata)
  }
  assertList(strata, class = "character", call = call)
  cols <- colnames(table)

  strataUnique <- unique(strata)
  n <- length(strata) - length(strataUnique)
  if (n > 0) {
    cli::cli_warn("{n} elements of strata eliminated because they were repeated.")
  }

  strataCols <- unique(purrr::flatten_chr(strataUnique))
  notPresent <- strataCols[!strataCols %in% cols]
  if (length(notPresent) > 0) {
    "Elements in strata not present as columns in table: {.var {notPresent}}." |>
      cli::cli_abort(call = call)
  }

  return(strataUnique)
}
