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

#' Import a codelist with details.
#'
#' @inheritParams importFileDoc
#'
#' @return A codelist_with_details object.
#'
#' @export
#'
importCodelistWithDetails <- function(path, type = NULL, recursive = FALSE) {
  assertChoice(type, choices = c("json", "csv"), length = 1, null = TRUE)
  files <- findFiles(path, type, recursive)

  # read content
  codelistWithDetails <- purrr::map(files, \(x) readConceptSetExpression(x, type)) |>
    purrr::compact() |>
    purrr::imap(\(x, nm) {
      if (any(as.logical(x$descendants))) {
        cli::cli_warn(c("!" = "skipping: {.pkg {nm}} because descendants = TRUE is not supported in codelists."))
        return(NULL)
      }
      res <- nameConceptSet(x, nm, "codelist_with_details_name")
      res |>
        dplyr::select(!dplyr::any_of(c("excluded", "descendants", "mapped"))) |>
        dplyr::mutate(
          codelist_with_details_name = as.character(.data$codelist_with_details_name),
          concept_id = as.integer(.data$concept_id)
        ) |>
        dplyr::relocate("codelist_with_details_name", "concept_id")
    }) |>
    purrr::compact() |>
    dplyr::bind_rows()

  if (nrow(codelistWithDetails) == 0) {
    codelistWithDetails <- emptyCodelistWithDetails()
  } else {
    codelistWithDetails <- newCodelistWithDetails(codelistWithDetails)
  }

  cli::cli_inform("{.pkg {length(codelistWithDetails)}} codelist{?s} with details imported.")

  return(codelistWithDetails)
}
