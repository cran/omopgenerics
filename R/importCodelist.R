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

#' Import a codelist.
#'
#' @inheritParams importFileDoc
#'
#' @return A codelist
#' @export
importCodelist <- function(path, type = NULL, recursive = FALSE) {
  assertChoice(type, choices = c("json", "csv"), length = 1, null = TRUE)
  files <- findFiles(path, type, recursive)

  # read content
  codelist <- purrr::map(files, \(x) readConceptSetExpression(x, type)) |>
    purrr::compact() |>
    purrr::imap(\(x, nm) {
      if (any(as.logical(x$descendants))) {
        cli::cli_warn(c("!" = "skipping: {.pkg {nm}} because descendants = TRUE is not supported in codelists."))
        return(NULL)
      }
      res <- nameConceptSet(x, nm, "codelist_name")
      res |>
        dplyr::mutate(
          codelist_name = as.character(.data$codelist_name),
          concept_id = as.integer(.data$concept_id)
        ) |>
        dplyr::select("codelist_name", "concept_id")
    }) |>
    purrr::compact() |>
    dplyr::bind_rows() |>
    newCodelist()

  cli::cli_inform("{.pkg {length(codelist)}} codelist{?s} imported.")

  return(codelist)
}
