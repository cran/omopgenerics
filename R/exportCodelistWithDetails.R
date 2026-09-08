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

#' Export a codelist with details object.
#'
#' @inheritParams codelistWithDetailsDoc
#' @inheritParams exportFileDoc
#'
#' @return Files with codelists with details
#' @export
#'
exportCodelistWithDetails <- function(x, path, type = "json") {
  assertChoice(type, choices = c("json", "csv"))
  assertCharacter(path, length = 1)
  if (!dir.exists(path)) {
    cli::cli_abort(c("x" = "Given path does not exist"))
  }
  x <- validateCodelistWithDetails(x)

  files <- writeCodelistWithDetails(x, path, type)

  return(invisible(files))
}

writeCodelistWithDetails <- function(x, path, type) {
  purrr::imap_chr(x, \(x, nm) {
    x <- dplyr::as_tibble(x)
    file <- file.path(path, paste0(nm, ".", type))
    if (type == "csv") {
      rlang::check_installed("readr")
      readr::write_csv(x, file = file)
    } else if (type == "json") {
      rlang::check_installed("jsonlite")
      items <- purrr::map(seq_len(nrow(x)), \(id) {
        concept <- x[id, , drop = FALSE]
        colnames(concept) <- toupper(colnames(concept))
        concept <- as.list(concept)
        list(
          concept = concept,
          isExcluded = FALSE,
          includeDescendants = FALSE,
          includeMapped = FALSE
        )
      })
      jsonlite::write_json(
        list(items = items), path = file, pretty = TRUE, auto_unbox = TRUE
      )
    }
    return(file)
  })
}
