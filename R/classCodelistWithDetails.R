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


#' 'codelist' object constructor
#'
#' @param x A named list where each element contains a tibble with the column
#' concept_id
#'
#' @return A codelist object.
#'
#' @export
#'
newCodelistWithDetails <- function(x) {
  # constructor
  x <- constructCodelistWithDetails(x)

  # validate
  x <- validateCodelistWithDetails(x)

  return(x)
}

constructCodelistWithDetails <- function(x) {
  if (inherits(x, "tbl") & all(c("concept_id", "codelist_with_details_name") %in% colnames(x))) {
    x <- x |>
      dplyr::collect() |>
      dplyr::group_by(.data$codelist_with_details_name) |>
      dplyr::group_split() |>
      as.list()
    names(x) <- purrr::map_chr(x, \(x) unique(x$codelist_with_details_name))
    x <- x |>
      purrr::map(\(x) {
        x <- x |>
          dplyr::select(!"codelist_with_details_name")
        for (col in setdiff(colnames(x), "concept_id")) {
          if (all(is.na(x[[col]]))) {
            x <- x |>
              dplyr::select(!dplyr::all_of(col))
          }
        }
        x
      })
  }

  x |>
    addClass("codelist_with_details")
}

validateCodelistWithDetails <- function(codelistWithDetails, call = parent.frame()) {
  assertList(codelistWithDetails, named = TRUE, class = c("data.frame", "tbl_df"), call = call)

  for (nm in names(codelistWithDetails)) {
    if (isFALSE(any("concept_id" %in% colnames(codelistWithDetails[[nm]])))) {
      cli::cli_abort("`{nm}` column concept_id not found", call = call)
    }

    if (any(is.na(unique(codelistWithDetails[[nm]]$concept_id)))) {
      cli::cli_abort("`{nm}` must not contain NA in concept_id field.", call = call)
    }

    codelistWithDetails[[nm]] <- codelistWithDetails[[nm]] |>
      dplyr::mutate(concept_id = as.integer(.data$concept_id)) |>
      dplyr::arrange(.data$concept_id)
  }

  # alphabetical order
  if (length(codelistWithDetails) > 0) {
    codelistWithDetails <- codelistWithDetails[order(names(codelistWithDetails))]
  }

  return(codelistWithDetails)
}


#' Print a codelist with details
#'
#' @param x A codelist with details
#' @param ...  Included for compatibility with generic. Not used.
#'
#' @return  Invisibly returns the input
#' @export
#'
#' @examples
#' codes <- list("disease X" = dplyr::tibble(
#'   concept_id = c(1, 2, 3),
#'   other = c("a", "b", "c")
#' ))
#' codes <- newCodelistWithDetails(codes)
#' print(codes)
#'
print.codelist_with_details <- function(x, ...) {
  cli::cli_h1("{length(x)} codelist{?s} with details")
  cli::cat_line("")
  disp <- 6
  len <- min(length(x), disp)
  for (i in seq_len(len)) {
    cli::cat_line(paste0("- ", names(x)[i], " (", length(x[[i]]$concept_id), " codes)"))
  }
  if (length(x) > disp) {
    cli::cat_line(paste0("along with ", length(x) - disp, " more codelists with details"))
  }
  invisible(x)
}

#' Empty `codelist` object.
#'
#' @return An empty codelist object.
#'
#' @export
#'
#' @examples
#' emptyCodelistWithDetails()
#'
emptyCodelistWithDetails <- function() {
  newCodelistWithDetails(list())
}

#' @export
bind.codelist_with_details <- function(...) {
  c(...)
}

#' @export
c.codelist_with_details <- function(...) {
  combineCodelist(x = list(...), type = "codelist_with_details")
}

#' @export
`[.codelist_with_details` <- function(x, i) {
  cl <- class(x)
  obj <- NextMethod()
  class(obj) <- cl
  return(obj)
}

#' @export
as_tibble.codelist_with_details <- function(x, ...) {
  if (length(x) == 0) {
    return(dplyr::tibble(
      codelist_with_details_name = character(),
      concept_id = integer()
    ))
  }
  x |>
    unclass() |>
    dplyr::bind_rows(.id = "codelist_with_details_name")
}
