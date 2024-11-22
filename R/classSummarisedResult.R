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

#' 'summarised_results' object constructor
#'
#' @param x Table.
#' @param settings Settings for the summarised_result object.
#'
#' @return A `summarised_result` object
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(omopgenerics)
#'
#' x <- tibble(
#'   "result_id" = 1L,
#'   "cdm_name" = "cprd",
#'   "group_name" = "cohort_name",
#'   "group_level" = "acetaminophen",
#'   "strata_name" = "sex &&& age_group",
#'   "strata_level" = c("male &&& <40", "male &&& >=40"),
#'   "variable_name" = "number_subjects",
#'   "variable_level" = NA_character_,
#'   "estimate_name" = "count",
#'   "estimate_type" = "integer",
#'   "estimate_value" = c("5", "15"),
#'   "additional_name" = "overall",
#'   "additional_level" = "overall"
#' ) |>
#'   newSummarisedResult()
#'
#' x
#' settings(x)
#' summary(x)
#'
#' x <- tibble(
#'   "result_id" = 1L,
#'   "cdm_name" = "cprd",
#'   "group_name" = "cohort_name",
#'   "group_level" = "acetaminophen",
#'   "strata_name" = "sex &&& age_group",
#'   "strata_level" = c("male &&& <40", "male &&& >=40"),
#'   "variable_name" = "number_subjects",
#'   "variable_level" = NA_character_,
#'   "estimate_name" = "count",
#'   "estimate_type" = "integer",
#'   "estimate_value" = c("5", "15"),
#'   "additional_name" = "overall",
#'   "additional_level" = "overall"
#' ) |>
#'   newSummarisedResult(settings = tibble(
#'     result_id = 1L, result_type = "custom_summary", mock = TRUE, value = 5
#'   ))
#'
#' x
#' settings(x)
#' summary(x)
#'
newSummarisedResult <- function(x, settings = attr(x, "settings")) {
  # inital input check
  assertTable(x = x, class = "data.frame", columns = resultColumns("summarised_result"), allowExtraColumns = TRUE)
  assertTable(x = settings, class = "data.frame", null = TRUE, columns = "result_id", allowExtraColumns = TRUE)

  # constructor
  x <- constructSummarisedResult(x, settings)

  # validate
  x <- validateSummarisedResult(x)

  return(x)
}

constructSummarisedResult <- function(x, settings) {
  x <- dplyr::as_tibble(x) |>
    dplyr::mutate(result_id = as.integer(.data$result_id))

  settings <- createSettings(x, settings) |>
    dplyr::arrange(.data$result_id)

  x <- x |>
    dplyr::select(dplyr::all_of(resultColumns(table = "summarised_result"))) |>
    dplyr::filter(.data$variable_name != "settings") |>
    dplyr::arrange(.data$result_id)

  structure(.Data = x, settings = settings) |>
    addClass(c("summarised_result", "omop_result"))
}
validateSummarisedResult <- function(x,
                                     call = parent.frame()) {
  # settings
  validateResultSettings(attr(x, "settings"), call = call)

  # sr
  validateSummarisedResultTable(x, call = call)
}
createSettings <- function(x, settings) {
  set <- list()

  # provided settings
  set$provided <- dplyr::as_tibble(settings)

  # present in result
  set$present <- x |>
    dplyr::select("result_id") |>
    dplyr::distinct()

  # long settings
  notCharacter <- character()
  setLong <- x |>
    dplyr::filter(.data$variable_name == "settings") |>
    dplyr::select(
      "result_id", "estimate_name", "estimate_type", "estimate_value"
    )
  if (nrow(setLong) > 0) {
    if (any(unique(setLong$estimate_type) != "character")) {
      notCharacter <- setLong$estimate_name[setLong$estimate_type != "character"]
    }
    set$long <- setLong |>
      dplyr::select(!"estimate_type") |>
      dplyr::distinct() |>
      tidyr::pivot_wider(
        names_from = "estimate_name", values_from = "estimate_value"
      )
  }

  # extra columns
  cols <- colnames(x)
  cols <- cols[!cols %in% resultColumns("summarised_result")]
  if (length(cols) > 0) {
    cli::cli_inform("{.var {cols}} moved to settings.")
    set$extra_columns <- x |>
      dplyr::select("result_id", dplyr::all_of(cols)) |>
      dplyr::distinct()
  }

  # merge settings
  set <- set |>
    purrr::compact() |>
    purrr::map(\(x) dplyr::mutate(x, result_id = as.integer(.data$result_id))) |>
    purrr::reduce(dplyr::full_join, by = "result_id")

  # missing settings
  compulsory <- c("result_type", "package_name", "package_version")
  colsMissing <- compulsory[!compulsory %in% colnames(set)]
  if (length(colsMissing) > 0) {
    cli::cli_inform("{.var {colsMissing}} added to {.pkg settings}.")
    for (col in colsMissing) {
      set <- set |>
        dplyr::mutate(!!col := "")
    }
  }
  set <- set |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(compulsory), \(x) dplyr::coalesce(x, "")
    ))

  # group, strata and additional
  set <- set |>
    addLabels(x, "group") |>
    addLabels(x, "strata") |>
    addLabels(x, "additional")

  # all settings must be character
  types <- variableTypes(set)
  notCharacter <- c(
    notCharacter,
    types$variable_name[types$variable_type != "character" & types$variable_name != "result_id"]
  )
  if (length(notCharacter) > 0) {
    cli::cli_inform("{.var {notCharacter}} casted to character.")
    set <- set |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(notCharacter), \(x) as.character(x)
      ))
  }

  # min_cell_count
  if (!"min_cell_count" %in% colnames(set)) {
    set <- set |>
      dplyr::mutate(min_cell_count = "0")
  } else {
    set <- set |>
      dplyr::mutate(
        min_cell_count = dplyr::coalesce(.data$min_cell_count, "0"),
        min_cell_count = dplyr::if_else(
          .data$min_cell_count == "1", "0", .data$min_cell_count
        )
      )
  }

  # remove NA
  colsRemove <- set |>
    purrr::map(\(x) {
      if (all(is.na(x))) {
        x
      } else {
        NULL
      }
    }) |>
    purrr::compact() |>
    names()
  if (length(colsRemove) > 0) {
    cli::cli_inform("{.var {colsRemove}} eliminated from settings as all elements are NA.")
    set <- set |>
      dplyr::select(!dplyr::all_of(colsRemove))
  }

  # order variables
  initialCols <- c(
    "result_id", "result_type", "package_name", "package_version", "group",
    "strata", "additional", "min_cell_count"
  )
  otherCols <- sort(colnames(set)[!colnames(set) %in% initialCols])
  set <- set |>
    dplyr::select(dplyr::all_of(c(initialCols, otherCols)))

  return(set)
}
addLabels <- function(set, x, prefix) {
  if (!prefix %in% colnames(set)) {
    if (nrow(x) == 0) {
      set <- set |>
        dplyr::mutate(!!prefix := "")
    } else {
      set <- set |>
        dplyr::left_join(
          x |>
            dplyr::group_by(.data$result_id) |>
            dplyr::group_split() |>
            purrr::map(\(x) {
              resId <- x$result_id[1]
              lab <- x |>
                dplyr::select(dplyr::all_of(paste0(prefix, "_name"))) |>
                dplyr::distinct() |>
                dplyr::pull() |>
                stringr::str_split(pattern = " &&& ") |>
                unlist() |>
                unique()
              lab <- paste0(lab[lab != "overall"], collapse = " &&& ")
              dplyr::tibble(result_id = resId, !!prefix := lab)
            }) |>
            dplyr::bind_rows(),
          by = "result_id"
        )
    }
  }
  set |>
    dplyr::mutate(!!prefix := dplyr::coalesce(.data[[prefix]], ""))
}
validateResultSettings <- function(set, call) {
  if (is.null(set)) {
    "{.cls summarised_result} object does not have settings attribute." |>
      cli::cli_abort(call = call)
  }
  if (!"result_id" %in% colnames(set)) {
    "{.var result_id} must be part of settings attribute." |>
      cli::cli_abort(call = call)
  }
  types <- variableTypes(set)
  if (types$variable_type[types$variable_name == "result_id"] != "integer") {
    "{.var result_id} must be {.cls integer} in settings attribute." |>
      cli::cli_abort(call = call)
  }
  types <- types |>
    dplyr::filter(.data$variable_name != "result_id")
  notCharacter <- types$variable_name[types$variable_type != "character"]
  if (length(notCharacter) > 0) {
    "{.var {notCharacter}} must be {.cls character} in settings attribute." |>
      cli::cli_abort(call = call)
  }
  if (length(set$result_id) != length(unique(set$result_id))) {
    "{.var result_id} must be unique in settings attribute." |>
      cli::cli_abort(call = call)
  }
  if (nrow(set) != nrow(dplyr::distinct(dplyr::select(set, !"result_id")))) {
    "Each {.var result_id} must be unique and contain a unique set of {.pkg settings}." |>
      cli::cli_abort(call = call)
  }
  # tidy names
  tidyGroup <- extractColumns(set, "group")
  tidyStrata <- extractColumns(set, "strata")
  tidyAdditional <- extractColumns(set, "additional")
  reportOverlap(tidyGroup, tidyStrata, "group", "strata", call)
  reportOverlap(tidyGroup, tidyAdditional, "group", "additional", call)
  reportOverlap(tidyStrata, tidyAdditional, "strata", "additional", call)

  invisible()
}
getLabels <- function(x) {
  res <- stringr::str_split(string = x, pattern = " &&& ") |>
    purrr::flatten_chr()
  res[!res %in% c("", "overall")]
}
extractColumns <- function(x, col) {
  x[[col]] |>
    as.list() |>
    rlang::set_names(as.character(x$result_id)) |>
    purrr::map(\(x) unique(getLabels(x)))
}
reportOverlap <- function(tidy1, tidy2, group1, group2, call) {
  x <- purrr::map2(tidy1, tidy2, intersect) |>
    purrr::compact()
  if (length(x) == 0) {
    return(invisible())
  }
  message <- x |>
    purrr::imap_chr(\(x, nm) {
      paste0(
        "In result_id = ", nm, ": `", paste0(x, collapse = "`, `"),
        "` present in both {.pkg ", group1, "} and {.pkg ", group2, "}."
      )
    }) |>
    unname()
  cli::cli_abort(message = message, call = call)
}
validateSummarisedResultTable <- function(x,
                                          duplicates = TRUE,
                                          pairs = TRUE,
                                          duplicateEstimates = TRUE,
                                          suppressPossibility = TRUE,
                                          call) {
  # all columns
  columns <- resultColumns(table = "summarised_result")
  notPresent <- columns[!columns %in% colnames(x)]
  if (length(notPresent) > 0) {
    "{.var {notPresent}} not present in {.cls summarised_result} object." |>
      cli::cli_abort(call = call)
  }

  # correct type
  x <- checkColumnsFormat(x = x, "summarised_result")

  # Cannot contain NA columns
  checkNA(x = x, "summarised_result")

  # estimate type
  estimateType <- unique(x$estimate_type)
  notValidEstimateTypes <- estimateType[!estimateType %in% estimateTypeChoices()]
  if (length(notValidEstimateTypes) > 0) {
    "{.var {notValidEstimateTypes}} {?is/are} not valid estimate_type values." |>
      cli::cli_abort(call = call)
  }

  # all ids in result must be in settings
  idsResult <- unique(x$result_id)
  idsSettings <- unique(attr(x, "settings")$result_id)
  notPresent <- idsResult[!idsResult %in% idsSettings]
  if (length(notPresent) > 0) {
    cli::cli_abort("result_id: {.var {notPresent}} not present in {.pkg settings} but present in data.")
  }

  # duplicates
  if (duplicates) {
    nr <- nrow(x)
    x <- x |>
      dplyr::distinct()
    eliminated <- nr - nrow(x)
    if (eliminated > 0) {
      cli::cli_inform(c("!" = "{eliminated} duplicated row{?s} eliminated."))
    }
  }

  # columPairs
  if (pairs) {
    validateNameLevel(x = x, prefix = "group", validation = "warning")
    validateNameLevel(x = x, prefix = "strata", validation = "warning")
    validateNameLevel(x = x, prefix = "additional", validation = "warning")
  }

  # no duplicated estimates
  if (duplicateEstimates) {
    checkDuplicated(x, validation = "error")
  }

  # suppress availability
  if (suppressPossibility) {
    checkGroupCount(x)
  }

  return(x)
}
checkColumns <- function(x, resultName, call = parent.frame()) {
  cols <- resultColumns(table = resultName)
  notPresent <- cols[!cols %in% colnames(x)]
  if (length(notPresent) > 0) {
    cli::cli_abort(
      "{paste0(notPresent, collapse = ', ')} must be present in a {.cls {resultName}}
      object."
    )
  }
  x |> dplyr::relocate(dplyr::all_of(cols))
}
checkNA <- function(x, type, call = parent.frame()) {
  cols <- fieldsResults$result_field_name[
    fieldsResults$result == type & fieldsResults$na_allowed == FALSE
  ]
  for (col in cols) {
    if (any(is.na(unique(x[[col]])))) {
      cli::cli_abort("`{col}` must not contain NA.", call = call)
    }
  }
  invisible(NULL)
}
checkColumnsFormat <- function(x, resultName) {
  cols <- resultColumns(resultName)
  expectedFormat <- fieldsResults$datatype[fieldsResults$result == resultName]
  formats <- purrr::map_chr(x, typeof)
  id <- formats != expectedFormat
  cols <- cols[id]
  formats <- formats[id]
  expectedFormat <- expectedFormat[id]
  if (length(cols) > 0) {
    err <- character()
    for (k in seq_along(cols)) {
      res <- tryCatch(
        expr = {
          x <- x |>
            dplyr::mutate(!!cols[k] := giveType(.data[[cols[k]]], expectedFormat[k]))
          list(x = x, err = character())
        },
        error = function(e) {
          list(x = x, err = cols[k])
        }
      )
      x <- res$x
      err <- c(err, res$err)
    }
    if (length(err) > 0) {
      err <- paste0(err, ": format=", formats, " (expected=", expectedFormat, ")")
      names(err) <- rep("*", length(err))
      cli::cli_abort(c("The following colum does not have a correct format", err))
    } else {
      err <- paste0(cols, ": from ", formats, " to ", expectedFormat)
      names(err) <- rep("*", length(err))
      cli::cli_inform(c("!" = "The following column type were changed:", err))
    }
  }
  invisible(x)
}
checkGroupCount <- function(x, validation = "error", call = parent.frame()) {
  grouping <- c(
    "result_id", "cdm_name", "group_name", "group_level", "strata_name",
    "strata_level", "additional_name", "additional_level"
  )
  obsLabels <- x |>
    dplyr::pull("variable_name") |>
    unique()
  obsLabelsL <- obsLabels |>
    stringr::str_replace_all(pattern = "_", replacement = " ") |>
    tolower()
  res <- character()
  n <- 0
  for (gcount in groupCount) {
    if (n < 5) {
      ol <- obsLabels[obsLabelsL %in% gcount]
      xx <- x |>
        dplyr::filter(
          .data$variable_name %in% ol & grepl("count", .data$estimate_name)
        ) |>
        dplyr::select(dplyr::all_of(c(grouping, "variable_name"))) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::group_split() |>
        as.list()
      for (k in seq_along(xx)) {
        if (n < 5) {
          res <- c(res, "*" = glue::glue("{nrow(xx[[k]])} '{gcount}' in variable_name for: {getGrouping(xx[[k]])}."))
          n <- n + 1
        }
      }
    }
  }
  if (length(res) > 0) {
    res <- c(
      "Each grouping (unique combination of: {grouping}) can not contain repeated group identifiers ({groupCount}).",
      "First {n} combination{?s}:",
      res
    )
    cli::cli_abort(res)
  }
  return(invisible(NULL))
}
getGrouping <- function(x) {
  x <- x |>
    dplyr::select(-dplyr::any_of("variable_name")) |>
    dplyr::distinct() |>
    as.list()
  lapply(seq_along(x), function(kk) {
    paste0(names(x)[kk], ": ", x[[kk]])
  }) |>
    unlist() |>
    paste0(collapse = ", ")
}

#' Validate if two columns are valid Name-Level pair.
#'
#' @param x A tibble.
#' @param prefix Prefix for the name-level pair, e.g. 'strata' for
#' strata_name-strata_level pair.
#' @param sep Separation pattern.
#' @param validation Either 'error', 'warning' or 'message'.
#' @param call Will be used by cli to report errors.
#'
#' @export
#'
validateNameLevel <- function(x,
                              prefix,
                              sep = " &&& ",
                              validation = "error",
                              call = parent.frame()) {
  # inital checks
  assertCharacter(prefix, length = 1)
  nameColumn <- paste0(prefix, "_name")
  levelColumn <- paste0(prefix, "_level")
  assertTable(x, columns = c(nameColumn, levelColumn))
  assertCharacter(sep)
  assertValidation(validation)

  # distinct pairs
  distinctPairs <- x |>
    dplyr::select(
      "name" = dplyr::all_of(nameColumn), "level" = dplyr::all_of(levelColumn)
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(dplyr::across(
      c("name", "level"),
      list(elements = ~ stringr::str_split(.x, pattern = sep))
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::ends_with("elements"),
      list(length = ~ lengths(.x))
    ))

  # pairs that dont match
  notMatch <- distinctPairs |>
    dplyr::filter(
      .data$name_elements_length != .data$level_elements_length
    )

  # error / warning
  if (nrow(notMatch) > 0) {
    unmatch <- notMatch |>
      dplyr::select("name", "level") |>
      dplyr::mutate("name_and_level" = paste0(
        .env$nameColumn, ": ", .data$name, "; ", .env$levelColumn, ": ",
        .data$level
      )) |>
      dplyr::pull("name_and_level")
    num <- length(unmatch)
    nun <- min(num, 5)
    unmatch <- unmatch[1:nun]
    names(unmatch) <- rep("*", nun)
    mes <- "name: `{nameColumn}` and level: `{levelColumn}` does not match in
    number of arguments ({num} unmatch), first {nun} unmatch:"

    # report
    report(c(mes, unmatch), validation = validation, call = call)
  }

  # check case
  nameCase <- distinctPairs[["name_elements"]] |>
    unlist() |>
    unique()
  notSnake <- nameCase[!isCase(nameCase, "snake")]
  if (length(notSnake) > 0) {
    "{length(notSnake)} element{?s} in {nameColumn} {?is/are} not snake_case." |>
      report(validation = validation, call = call)
  }

  return(invisible(x))
}
isCase <- function(x, case) {
  if (length(x) == 0) {
    return(logical())
  }
  flag <- switch(case,
    "snake" = isSnakeCase(x),
    "sentence" = isSentenceCase(x),
    "NA" = rep(TRUE, length(x)),
    rep(NA, length(x))
  )
  return(flag)
}
isSentenceCase <- function(x) {
  if (length(x) > 0) {
    x == snakecase::to_sentence_case(x)
  } else {
    x
  }
}
isSnakeCase <- function(x) {
  if (length(x) > 0) {
    x == toSnakeCase(x)
  } else {
    x
  }
}
checkColumnContent <- function(x, col, content) {
  if (!all(x[[col]] %in% content)) {
    notType <- x[[col]][!x[[col]] %in% content] |> unique()
    len <- length(notType)
    notType <- notType[1:min(5, len)]
    cli::cli_abort(c(
      "{col} contains incorrect values, possible values:
      {paste0(content, collapse = ', ')}. Observed values:
      {paste0(notType, collapse = ', ')}{ifelse(len>5, '...', '.')}"
    ))
  }
  return(invisible(TRUE))
}
checkDuplicated <- function(x, validation, call = parent.frame()) {
  nraw <- nrow(x)
  ndist <- x |>
    dplyr::select(!"estimate_value") |>
    dplyr::distinct() |>
    nrow()
  dup <- nraw - ndist
  if (dup > 0) {
    report(
      message = c(
        "{dup} duplicated results with different estimate values found.",
        "i" = "Run the following to see which are",
        "data |>",
        " " = "dplyr::group_by(dplyr::across(!'estimate_value')) |>",
        " " = "dplyr::tally() |>",
        " " = "dplyr::filter(n > 1)"
      ),
      validation = validation,
      call = call
    )
  }
  return(invisible(TRUE))
}
giveType <- function(x, type) {
  switch(type,
    "integer" = as.integer(x),
    "double" = as.double(x),
    "character" = as.character(x),
    "logical" = as.logical(x),
    x
  )
}
validateTidyNames <- function(result, call = parent.frame()) {
  # setting columns
  colsSettings <- colnames(settings(result))
  colsSettings <- colsSettings[colsSettings != "result_id"]

  # group columns
  colsGroup <- uniqueCols(result$group_name)

  # strata columns
  colsStrata <- uniqueCols(result$strata_name)

  # additional columns
  colsAdditional <- uniqueCols(result$additional_name)

  # default columns
  colsSummarisedResult <- resultColumns("summarised_result")

  cols <- list(
    settings = colsSettings,
    group = colsGroup,
    strata = colsStrata,
    additional = colsAdditional,
    summarised_result = colsSummarisedResult
  )

  # compare each pair
  len <- length(cols)
  nms <- names(cols)
  for (k in 1:(len - 1)) {
    for (i in (k + 1):len) {
      both <- intersect(cols[[k]], cols[[i]])
      if (length(both) > 0) {
        "{.var {both}} {?is/are} present in both '{nms[k]}' and '{nms[i]}'. This will be an error in the next release." |>
          cli::cli_warn() # Turn error
      }
    }
  }

  return(invisible(result))
}
uniqueCols <- function(x) {
  x <- x |>
    unique() |>
    stringr::str_split(" &&& ") |>
    unlist() |>
    unique()
  x[x != "overall"]
}

#' Required columns that the result tables must have.
#'
#' @param table Table to see required columns.
#'
#' @return Required columns
#'
#' @export
#'
#' @examples
#' library(omopgenerics)
#'
#' resultColumns()
#'
resultColumns <- function(table = "summarised_result") {
  assertChoice(table, unique(fieldsResults$result))
  x <- fieldsResults$result_field_name[fieldsResults$result == table]
  return(x)
}

#' Choices that can be present in `estimate_type` column.
#'
#' @return A character vector with the options that can be present in
#' `estimate_type` column in the summarised_result objects.
#'
#' @export
#'
#' @examples
#' library(omopgenerics)
#'
#' estimateTypeChoices()
#'
estimateTypeChoices <- function() {
  c(
    "numeric", "integer", "date", "character", "proportion", "percentage",
    "logical"
  )
}

#' Empty `summarised_result` object.
#'
#' @param settings Tibble/data.frame with the settings of the empty
#' summarised_result. It has to contain at least `result_id` column.
#'
#' @return An empty `summarised_result` object.
#'
#' @export
#'
#' @examples
#' library(omopgenerics)
#'
#' emptySummarisedResult()
#'
emptySummarisedResult <- function(settings = NULL) {
  if (is.null(settings)) {
    settings <- dplyr::tibble(
      "result_id" = integer(),
      "result_type" = character(),
      "package_name" = character(),
      "package_version" = character()
    )
  }
  resultColumns("summarised_result") |>
    rlang::rep_named(list(character())) |>
    dplyr::as_tibble() |>
    dplyr::mutate("result_id" = as.integer()) |>
    newSummarisedResult(settings = settings)
}

report <- function(message,
                   validation, # error/warning/inform
                   call = parent.frame(), # where error is reported
                   .envir = parent.frame()) { # where glue statements are evaluated
  if (validation == "error") {
    cli::cli_abort(addSignal(message, "x"), .envir = .envir, call = call)
  } else if (validation == "warning") {
    cli::cli_warn(addSignal(message, "!"), .envir = .envir)
  } else if (validation == "inform") {
    cli::cli_inform(addSignal(message, "!"), .envir = .envir)
  }
  return(invisible(TRUE))
}
addSignal <- function(x, nm) {
  if (length(x) > 0) names(x)[1] <- nm
  return(x)
}
