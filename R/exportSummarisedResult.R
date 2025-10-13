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

#' Export a summarised_result object to a csv file.
#'
#' @param ... A set of summarised_result objects.
#' @param minCellCount Minimum count for suppression purposes.
#' @param fileName Name of the file that will be created. Use \{cdm_name\} to
#' refer to the cdmName of the objects and \{date\} to add the export date.
#' @param path Path where to create the csv file. It is ignored if fileName it
#' is a full name with path included.
#' @param logFile Path to the log file to export.
#' @param logSqlPath Path to the folder that contains the sql logs to export.
#' @param logExplainPath Path to the folder that contains the sql logs explain
#' to export.
#'
#' @export
#'
exportSummarisedResult <- function(...,
                                   minCellCount = 5,
                                   fileName = "results_{cdm_name}_{date}.csv",
                                   path = getwd(),
                                   logFile = getOption("omopgenerics.logFile"),
                                   logSqlPath = getOption("omopgenerics.log_sql_path"),
                                   logExplainPath = getOption("omopgenerics.log_sql_explain_path")) {
  # initial checks
  results <- list(...) |>
    purrr::compact()
  assertCharacter(fileName, length = 1, minNumCharacter = 1)
  assertCharacter(path, length = 1, minNumCharacter = 1)
  if (dirname(fileName) != ".") {
    path <- dirname(fileName)
    fileName <- basename(fileName)
  }
  if (!dir.exists(path)) {
    cli::cli_abort("path: {path}, does not exist")
  }

  fileExt <- tools::file_ext(fileName)
  if (fileExt == "") {
    fileName <- paste0(fileName, ".csv")
  } else if (fileExt != "csv") {
    cli::cli_warn("Only .csv extrension is allowed (.{fileExt} -> .csv).")
    fileName <- paste0(tools::file_path_sans_ext(fileName), ".csv")
  }

  # get cdm name
  if (!is.null(logFile) | !is.null(logSqlPath) | !is.null(logExplainPath)) {
    cdmName <- results |>
      purrr::map(\(x) unique(x$cdm_name)) |>
      unlist() |>
      unique()
    if (length(cdmName) != 1) {
      cdmName <- "unknown"
    }
  }

  # export log
  if (!is.null(logFile)) {
    results$log_summary <- summariseLogFile(logFile = logFile, cdmName = cdmName)
  }

  # export sql log
  if (!is.null(logSqlPath)) {
    results$sql <- summariseLogSqlPath(logSqlPath = logSqlPath, cdmName = cdmName)
  }

  # export sql explain log
  if (!is.null(logExplainPath)) {
    results$explain <- summariseLogExplainPath(logExplainPath = logExplainPath, cdmName = cdmName)
  }

  # if result is list(), results will be a list containing an empty list
  if (length(results) == 0) {
    cli::cli_warn("Input is NULL, empty result exported.")
    results <- emptySummarisedResult()
  } else {
    results <- bind(results)
  }

  results <- suppress(results, minCellCount = minCellCount)
  # cdm name
  cdmName <- results$cdm_name |>
    unique() |>
    paste0(collapse = "_")
  fileName <- stringr::str_replace(
    string = fileName,
    pattern = "\\{cdm_name\\}",
    replacement = cdmName
  )
  # date
  date <- format(Sys.Date(), format = "%Y_%m_%d") |> as.character()
  fileName <- stringr::str_replace(
    string = fileName,
    pattern = "\\{date\\}",
    replacement = date
  )

  # to tibble + pivot settings
  x <- results |>
    dplyr::as_tibble() |>
    dplyr::union_all(results |> pivotSettings() |> dplyr::as_tibble())

  # change encoding to utf8
  x <- tryCatch(expr = {
    purrr::map(x, stringi::stri_enc_toutf8)
  }, error = function(e) {
    cli::cli_warn(c("!" = "Couldn't change encoding to UTF8, please report this issue.", as.character(e)))
    x
  })


  utils::write.csv(
    x,
    file = file.path(path, fileName), row.names = FALSE
  )
}

pivotSettings <- function(x) {
  x |>
    settings() |>
    dplyr::mutate(dplyr::across(!"result_id", \(x) as.character(x))) |>
    tidyr::pivot_longer(
      cols = !"result_id",
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    dplyr::filter(!is.na(.data$estimate_value)) |>
    dplyr::inner_join(
      x |>
        settings() |>
        variableTypes() |>
        dplyr::select(
          "estimate_name" = "variable_name", "estimate_type" = "variable_type"
        ) |>
        dplyr::mutate("estimate_type" = dplyr::if_else(
          .data$estimate_type == "categorical", "character", .data$estimate_type
        )),
      by = "estimate_name"
    ) |>
    dplyr::mutate(
      "estimate_value" = as.character(.data$estimate_value),
      "variable_name" = "settings",
      "variable_level" = NA_character_,
      "group_name" = "overall",
      "group_level" = "overall",
      "strata_name" = "overall",
      "strata_level" = "overall",
      "additional_name" = "overall",
      "additional_level" = "overall"
    ) |>
    dplyr::arrange(.data$result_id) |>
    dplyr::left_join(
      x |>
        dplyr::select("result_id", "cdm_name") |>
        dplyr::distinct() |>
        dplyr::group_by(.data$result_id) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::ungroup(),
      by = "result_id"
    )
}
variableTypes <- function(table) {
  assertTable(table, class = "data.frame")
  if (ncol(table) > 0) {
    x <- dplyr::tibble(
      "variable_name" = colnames(table),
      "variable_type" = lapply(colnames(table), function(x) {
        table |>
          dplyr::select(dplyr::all_of(x)) |>
          utils::head(1) |>
          dplyr::pull() |>
          dplyr::type_sum() |>
          assertClassification()
      }) |> unlist()
    )
  } else {
    x <- dplyr::tibble(
      "variable_name" = character(),
      "variable_type" = character()
    )
  }
  return(x)
}
assertClassification <- function(x) {
  switch(x,
    "chr" = "character",
    "fct" = "character",
    "ord" = "character",
    "glue" = "character",
    "date" = "date",
    "dttm" = "date",
    "lgl" = "logical",
    "drtn" = "numeric",
    "dbl" = "numeric",
    "int" = "integer",
    "int64" = "integer",
    NA_character_
  )
}
