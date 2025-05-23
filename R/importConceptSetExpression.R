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

#' Import a concept set expression.
#'
#' @param path Path to where files will be created.
#' @param type Type of files to export. Currently 'json' and 'csv' are
#' supported.
#'
#' @return A concept set expression
#' @export
importConceptSetExpression <- function(path, type = "json") {
  assertChoice(type, choices = c("json", "csv"))
  files <- findFiles(path, type)

  # read content
  conceptSetExpression <- purrr::map(files, \(x) readConceptSetExpression(x, type)) |>
    purrr::compact() |>
    newConceptSetExpression()

  cli::cli_inform("{.strong {length(conceptSetExpression)}} conceptSetExpression{?s} imported.")

  return(conceptSetExpression)
}

findFiles <- function(path, type, call = parent.frame()) {
  assertCharacter(path, length = 1, call = call)
  if (!file.exists(path)) {
    cli::cli_warn("directory {.path {path}} does not exist, output will be empty")
    return(list())
  }
  if (file.info(path)$isdir) {
    path <- list.files(path = path, full.names = TRUE)
  }
  path <- path[tools::file_ext(path) == type]
  names(path) <- tools::file_path_sans_ext(basename(path))
  as.list(path)
}
readConceptSetExpression <- function(file, type) {
  tryCatch({
    if (type == "csv") {
      content <- readr::read_csv(file = file, show_col_types = FALSE) |>
        dplyr::select(
          "concept_id", dplyr::any_of(c("excluded", "descendants", "mapped"))
        )
    } else if (type == "json") {
      rlang::check_installed("jsonlite")
      content <- jsonlite::fromJSON(file)
      content <- dplyr::tibble(
        concept_id = content$items$concept$CONCEPT_ID,
        excluded = content$items$isExcluded,
        descendants = content$items$includeDescendants,
        mapped = content$items$includeMapped
      ) |>
        dplyr::select("concept_id", "excluded", "descendants", "mapped")
    }
    return(content)
  },
  error = function(e) {
    cli::cli_warn("skipping file: {.path {file}} due to: {e}.")
    return(NULL)
  })
}
