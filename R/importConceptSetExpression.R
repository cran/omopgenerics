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
#' @inheritParams importFileDoc
#'
#' @return A concept set expression
#' @export
importConceptSetExpression <- function(path, type = NULL, recursive = FALSE) {
  assertChoice(type, choices = c("json", "csv"), length = 1, null = TRUE)
  files <- findFiles(path, type, recursive)

  # read content
  conceptSetExpression <- purrr::map(files, \(x) readConceptSetExpression(x, type)) |>
    purrr::compact() |>
    purrr::imap(\(res, nm) {
      res <- nameConceptSet(res, nm, "concept_set_expression_name")
      cols <- colnames(res)
      q <- c("excluded", "descendants", "mapped") |>
        purrr::keep(\(x) !x %in% cols) |>
        rlang::set_names() |>
        purrr::map_chr(\(x) "FALSE") |>
        rlang::parse_exprs()
      res |>
        dplyr::mutate(
          !!!q,
          concept_set_expression_name = as.character(.data$concept_set_expression_name),
          excluded = as.logical(.data$excluded),
          descendants = as.logical(.data$descendants),
          mapped = as.logical(.data$mapped),
          concept_id = as.integer(.data$concept_id)
        ) |>
        dplyr::select(
          "concept_set_expression_name", "concept_id", "excluded",
          "descendants", "mapped"
        )
    }) |>
    dplyr::bind_rows() |>
    newConceptSetExpression()

  cli::cli_inform("{.strong {length(conceptSetExpression)}} concept set expression{?s} imported.")

  return(conceptSetExpression)
}

findFiles <- function(path, type, recursive, call = parent.frame()) {
  assertCharacter(path, call = call)
  assertLogical(recursive, length = 1, call = call)

  if (is.null(type)) {
    type <- c("json", "csv")
  }
  pattern <- paste0("\\.", type, "$", collapse = "|")

  # get all paths
  path <- path |>
    purrr::map(\(x) {
      if (!stringr::str_starts(string = x, pattern = "https")) {
        if (!file.exists(x)) {
          cli::cli_warn(c("x" = "directory {.path {x}} does not exist"))
          return(list())
        }
        if (file.info(x)$isdir) {
          x <- list.files(path = x, full.names = TRUE, pattern = pattern, recursive = recursive)
        }
      }
      return(x)
    }) |>
    unlist() |>
    as.character()
  pathClean <- sub("\\?.*$", "", path) # for urls
  names(path) <- tools::file_path_sans_ext(basename(pathClean))
  as.list(path)
}

nameConceptSet <- function(x, nm, name) {
  cols <- colnames(x)
  nameCols <- c(name, "codelist_name", "codelist_with_details_name", "concept_set_expression_name") |>
    unique()
  oldName <- nameCols[nameCols %in% cols][1]
  if (is.na(oldName)) {
    x <- x |>
      dplyr::mutate(!!name := .env$nm)
  } else if (oldName != name) {
    colnames(x)[colnames(x) == oldName] <- name
  }
  x <- x |>
    dplyr::select(!dplyr::any_of(setdiff(nameCols, name)))
  return(x)
}

readConceptSetExpression <- function(file, type) {
  tryCatch({
    if (is.null(type)) {
      type <- tolower(tools::file_ext(file))
    }
    if (type == "csv") {
      opt <- c("excluded", "descendants", "mapped")
      content <- readr::read_csv(file = file, show_col_types = FALSE)
      colnames(content) <- toSnakeCase(colnames(content))
      for (col in opt) {
        if (!col %in% colnames(content)) {
          content <- content |>
            dplyr::mutate(!!col := FALSE)
        }
      }
    } else if (type == "json") {
      rlang::check_installed("jsonlite")
      content <- jsonlite::fromJSON(file)
      items <- content$items
      content <- dplyr::as_tibble(items$concept)
      colnames(content) <- toSnakeCase(colnames(content))
      content <- content |>
        dplyr::mutate(
          excluded = items$isExcluded %||% FALSE,
          descendants = items$includeDescendants %||% FALSE,
          mapped = items$includeMapped %||% FALSE
        ) |>
        dplyr::mutate(dplyr::across(
          .cols = dplyr::all_of(c("excluded", "descendants", "mapped")),
          .fns = \(x) dplyr::coalesce(as.logical(x), FALSE)
        ))
    }
    if (!"concept_id" %in% colnames(content)) {
      cli::cli_abort(c(x = "Column concept_id not found."))
    }
    return(content)
  },
  error = function(e) {
    cli::cli_warn("skipping file: {.path {file}} due to: {e}.")
    return(NULL)
  })
}
