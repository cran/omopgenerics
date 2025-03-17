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

#' Import a set of summarised results.
#'
#' @param path Path to directory with CSV files containing summarised results or
#' to a specific CSV file with a summarised result.
#' @param recursive If TRUE and path is a directory, search for files will
#' recurse into directories
#' @param ... Passed to `readr::read_csv`.
#'
#'
#' @return A summarised result
#' @export
#'
importSummarisedResult <- function(path,
                                   recursive = FALSE,
                                   ...) {
  rlang::check_installed("readr")
  assertCharacter(path)
  dots <- list(...)
  dots$col_types = c(.default = "c", result_id = "i")
  dots$show_col_types = FALSE
  encode <- "locale" %in% names(dots)

  result <- path |>
    # get all paths
    purrr::map(\(x) {
      # check file or path
      ext <- tools::file_ext(x)
      if (ext == "") {
        if (!dir.exists(x)) {
          cli::cli_abort(c("x" = "Given path does not exist"))
        }
        x <- list.files(
          x, recursive = recursive, pattern = "\\.csv$", full.names = TRUE
        )
      } else if (ext == "csv") {
        if (!file.exists(x)) {
          cli::cli_abort(c("x" = "Given file does not exist"))
        }
      }
      x
    }) |>
    unlist() |>
    rlang::set_names() |>
    # read all files
    purrr::map(\(x) {
      cli::cli_inform("Reading file: {.path {x}}.")
      args <- dots
      args$file <- x
      if (!encode) {
        args$locale <- tryCatch({
          enc <- readr::guess_encoding(x, n_max = -1) |>
            dplyr::arrange(dplyr::desc(.data$confidence)) |>
            utils::head(1) |>
            dplyr::pull("encoding")
          readr::locale(encoding = enc)
        },
        error = function(e) {readr::default_locale()}
        )
      }
      do.call(readr::read_csv, args)
    }) |>
    # convert to summarised_results
    purrr::imap(\(x, nm) {
      cli::cli_inform("Converting to summarised_result: {.path {nm}}.")
      tryCatch({
        omopgenerics::newSummarisedResult(x)
      },
      error = function(e) {
        cli::cli_inform(c(x = "Failed to convert because:", as.character(e)))
        NULL
      })
    }) |>
    purrr::compact()

  if (length(result) == 0) {
    cli::cli_warn("No results obtained, returning an empty summarised result.")
    result <- emptySummarisedResult()
  } else {
    result <- bind(result) |>
      dplyr::arrange(.data$cdm_name, .data$result_id)
  }

  result
}
