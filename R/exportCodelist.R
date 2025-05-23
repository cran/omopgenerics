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

#' Export a codelist object.
#'
#' @param x A codelist
#' @param path Path to where files will be created.
#' @param type Type of files to export. Currently 'json' and 'csv' are
#' supported.
#'
#' @return Files with codelists
#' @export
#'
exportCodelist <- function(x, path, type = "json") {
  assertChoice(type, choices = c("json", "csv"))
  assertCharacter(path, length = 1)
  if (!dir.exists(path)) {
    cli::cli_abort(c("x" = "Given path does not exist"))
  }
  x <- validateCodelist(x)

  files <- writeCodelist(x, path, type)

  return(invisible(files))
}

writeCodelist <- function(x, path, type) {
  purrr::imap_chr(x, \(x, nm) {
    file <- file.path(path, paste0(nm, ".", type))
    if (type == "csv") {
      readr::write_csv(dplyr::tibble(concept_id = x), file = file)
    } else if (type == "json") {
      rlang::check_installed("jsonlite")
      items <- purrr::map(x, \(x) {
        list(
          concept = list(
            CONCEPT_ID = x,
            CONCEPT_NAME = "",
            STANDARD_CONCEPT = "",
            STANDARD_CONCEPT_CAPTION = "",
            INVALID_REASON = "",
            INVALID_REASON_CAPTION = "",
            CONCEPT_CODE = "",
            DOMAIN_ID = "",
            VOCABULARY_ID = "",
            CONCEPT_CLASS_ID = ""
          ),
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
