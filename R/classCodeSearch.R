# Copyright 2026 DARWIN EU (C)
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

codeSerachColumns <- dplyr::tribble(
  ~table, ~column, ~type, ~convert,
  "search_strategy", "strategy_id", "integer", "as.integer",
  "search_strategy", "strategy_name", "character", "as.character",
  "search_strategy", "strategy_value", "character", "as.character",
  "codes", "concept_id", "integer", "as.integer",
  "codes", "found_from", "character", "as.character",
  "codes", "concept_name", "character", "as.character",
  "codes", "vocabulary_version", "character", "as.character",
  "codes", "domain_id", "character", "as.character",
  "codes", "vocabulary_id", "character", "as.character",
  "codes", "concept_class_id", "character", "as.character",
  "codes", "standard_concept", "character", "as.character",
  "codes", "concept_code", "character", "as.character",
  "codes", "valid_start_date", "date", "as.Date",
  "codes", "valid_end_date", "date", "as.Date",
  "codes", "invalid_reason", "character", "as.character"
)
codeSearchColumns <- codeSerachColumns

#' Create a new `code_search` object
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param codes A tibble with concept ids. It must contain "concept_id",
#' "found_from", "concept_name", "vocabulary_version", "domain_id",
#' "vocabulary_id", "concept_class_id", "standard_concept", "concept_code",
#' "valid_start_date", "valid_end_date" and "invalid_reason" as columns.
#' @param searchStrategy A tibble with the search strategy used to derive the
#' codes. It must contain "strategy_id", "strategy_name" and "strategy_value"
#' as columns.
#'
#' @returns A `code_search` object.
#' @export
#'
newCodeSearch <- function(codes, searchStrategy) {
  colsCodes <- codeSearchColumns |>
    dplyr::filter(.data$table == "codes") |>
    dplyr::pull("column")
  assertTable(codes, columns = colsCodes)

  colsStrategy <- codeSearchColumns |>
    dplyr::filter(.data$table == "search_strategy") |>
    dplyr::pull("column")
  assertTable(searchStrategy, columns = colsStrategy)

  codes <- dplyr::as_tibble(codes) |>
    dplyr::relocate(dplyr::all_of(colsCodes))
  searchStrategy <- dplyr::as_tibble(searchStrategy) |>
    dplyr::relocate(dplyr::all_of(colsStrategy))

  codes <- castCodeSearchColumns(codes, "codes")
  searchStrategy <- castCodeSearchColumns(searchStrategy, "search_strategy")

  structure(
    .Data = codes,
    search_strategy = searchStrategy,
    class = c("code_search", class(codes))
  )
}

validateCodeSearch <- function(codeSearch) {
  assertClass(codeSearch, "code_search")
}

#' Empty code search object
#'
#' @param searchStrategy The search strategy used to generate the candidate
#' codes.
#'
#' @returns An empty `code_search` object.
#' @export
#'
emptyCodeSearch <- function(searchStrategy = NULL) {
  if (is.null(searchStrategy)) {
    searchStrategy <- dplyr::tibble(
      strategy_id = integer(),
      strategy_name = character(),
      strategy_value = character()
    )
  }

  dplyr::tibble(
    concept_id = integer(),
    found_from = character(),
    concept_name = character(),
    vocabulary_version = character(),
    domain_id = character(),
    vocabulary_id = character(),
    concept_class_id = character(),
    standard_concept = character(),
    concept_code = character(),
    valid_start_date = as.Date(character()),
    valid_end_date = as.Date(character()),
    invalid_reason = character()
  ) |>
    newCodeSearch(searchStrategy = searchStrategy)
}

castCodeSearchColumns <- function(x, table) {
  cols <- codeSearchColumns |>
    dplyr::filter(.data$table == .env$table)
  q <- paste0(cols$convert, "(.data$", cols$column, ")") |>
    rlang::parse_exprs() |>
    rlang::set_names(nm = cols$column)
  x |>
    dplyr::mutate(!!!q)
}

#' Print a code search
#'
#' @param x A `code_search` object.
#' @param ... For compatibility with the generic.
#'
#' @return Invisibly returns the input.
#' @export
#'
print.code_search <- function(x, ...) {
  cat("\033[1;34mi\033[0m This is a candidate code search.\n")

  st <- searchStrategy(x) |>
    dplyr::filter(.data$strategy_id == 1L) |>
    tidyr::pivot_wider(
      names_from = "strategy_name",
      values_from = "strategy_value"
    )

  if (nrow(st) > 0) {
    pkg <- paste0(
      "\033[32m", st$package_name %||% "unknown package",
      "\033[0m (\033[1m", st$package_version %||% "unknown version",
      "\033[0m)"
    )
    cat(paste0("\033[3mCandidate codes\033[0m generated using ", pkg, ":\n"))
    fun <- st$function_name %||% ""
    parameters <- st |>
      dplyr::select(!dplyr::any_of(c(
        "package_name", "package_version", "strategy_id", "function_name"
      ))) |>
      purrr::imap_chr(\(x, nm) paste0(nm, " = ", x)) |>
      paste0(collapse = ",\n  ")
    cat(paste0(fun, "(\n  ", parameters, "\n)\n"))
  } else {
    cat("\033[3mNo search strategy found.\033[0m\n")
  }

  NextMethod()
}

#' Import a `code_search` object from an Excel spreadsheet
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param path Path to the Excel spreadsheet.
#'
#' @returns A `code_search` object.
#' @export
#'
importCodeSearch <- function(path) {
  assertCharacter(path, length = 1)
  rlang::check_installed("openxlsx")

  if (file.exists(path)) {
    file <- path
  } else if (file.exists(paste0(path, ".xlsx"))) {
    file <- paste0(path, ".xlsx")
  } else if (file.exists(paste0(path, ".xls"))) {
    file <- paste0(path, ".xls")
  } else {
    if (dir.exists(path)) {
      file <- list.files(path, pattern = ".xls$|.xlsx$", full.names = TRUE)
      if (length(file) > 1) {
        cli::cli_abort(
          c(x = "Multiple Excel spreadsheets found in {.path {path}} please specify just one.")
        )
      }
    } else {
      cli::cli_abort(c(x = "Provided file/path does not exist."))
    }
  }

  sheets <- openxlsx::getSheetNames(file = file)
  notPresent <- c("SearchStrategy", "CandidateCodes") |>
    purrr::keep(\(x) !x %in% sheets)
  if (length(notPresent) > 0) {
    cli::cli_abort(
      c(x = "{.var {notPresent}} sheet{?s} not present in excel file.")
    )
  }

  newCodeSearch(
    codes = openxlsx::read.xlsx(
      xlsxFile = file, sheet = "CandidateCodes", detectDates = TRUE
    ),
    searchStrategy = openxlsx::read.xlsx(
      xlsxFile = file, sheet = "SearchStrategy"
    )
  )
}

#' Export a `code_search` object into an Excel spreadsheet
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param codeSearch A `code_search` object.
#' @param file Name of the Excel spreadsheet to be created.
#' @param path Path where to create the Excel file.
#'
#' @returns The `code_search` object is exported to a file.
#' @export
#'
exportCodeSearch <- function(codeSearch, file, path = getwd()) {
  codeSearch <- validateCodeSearch(codeSearch)
  assertCharacter(file, length = 1)
  assertCharacter(path, length = 1)

  rlang::check_installed("openxlsx")

  if (!endsWith(file, ".xlsx") & !endsWith(file, ".xls")) {
    file <- paste0(file, ".xlsx")
  }

  wb <- openxlsx::loadWorkbook(
    file = system.file("Instructions.xlsx", package = "omopgenerics")
  )

  openxlsx::addWorksheet(wb = wb, sheetName = "SearchStrategy")
  openxlsx::writeData(
    wb = wb, sheet = "SearchStrategy", x = searchStrategy(codeSearch)
  )

  openxlsx::addWorksheet(wb = wb, sheetName = "CandidateCodes")
  openxlsx::writeData(wb = wb, sheet = "CandidateCodes", x = codeSearch)

  openxlsx::saveWorkbook(
    wb = wb, file = file.path(path, file), overwrite = TRUE
  )

  invisible(file.path(path, file))
}

#' Get the search strategy used to create a `code_search`
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param codeSearch A `code_search` object.
#'
#' @returns A search strategy tibble.
#' @export
#'
searchStrategy <- function(codeSearch) {
  validateCodeSearch(codeSearch = codeSearch)
  attr(codeSearch, "search_strategy")
}
