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


#' 'concept_set_expression' object constructor
#'
#' @param x a named list of tibbles, each of which containing concept set
#' definitions
#'
#' @return A concept_set_expression
#' @export
#'
newConceptSetExpression <- function(x) {
  # constructor
  x <- constructConceptSetExpression(x)

  # validate
  x <- validateConceptSetExpression(x)

  return(x)
}

constructConceptSetExpression <- function(x) {
  if (inherits(x, "tbl")) {
    ex <- colnames(x)
    if ("codelist_name" %in% ex & !"concept_set_expression_name" %in% ex) {
      x <- x |>
        dplyr::rename("concept_set_expression_name" = "codelist_name")
    }
    cols <- c("excluded", "descendants", "mapped")
    for (col in cols) {
      if (!col %in% ex) {
        x <- x |>
          dplyr::mutate(!!col := FALSE)
      }
    }
    cols <- c("concept_id", cols, "concept_set_expression_name")
    if (all(cols %in% ex)) {
      x <- x |>
        dplyr::collect() |>
        dplyr::group_by(.data$concept_set_expression_name) |>
        dplyr::group_split() |>
        as.list()
      names(x) <- purrr::map_chr(x, \(x) unique(x$concept_set_expression_name))
      x <- purrr::map(x, \(x) dplyr::select(x, !"concept_set_expression_name"))
    }
  } else if (is.list(x) & length(x) > 0) {
    if (is.numeric(x[[1]])) {
      x <- newCodelist(x)
    }
  }

  if (inherits(x = x, what = "codelist")) {
    x <- x |>
      purrr::map(\(x) {
        dplyr::tibble(
          concept_id = as.integer(x),
          excluded = FALSE,
          descendants = FALSE,
          mapped = FALSE
        )
      })
  }

  x <- x |>
    addClass(c("concept_set_expression", "conceptSetExpression"))

  return(x)
}

validateConceptSetExpression <- function(x, call = parent.frame()) {
  assertList(x, named = TRUE, class = c("tbl"), call = call)

  for (i in seq_along(x)) {
    assertTable(
      x = x[[i]], class = "data.frame", columns = c("concept_id"), call = call
    )
    cols <- c("excluded", "descendants", "mapped")
    cols <- cols[!cols %in% colnames(x[[i]])]
    for (col in cols) {
      x[[i]] <- x[[i]] |>
        dplyr::mutate(!!col := FALSE)
    }
    assertNumeric(x[[i]]$concept_id, integerish = TRUE, call = call)
    assertLogical(x[[i]]$excluded, call = call)
    assertLogical(x[[i]]$descendants, call = call)
    assertLogical(x[[i]]$mapped, call = call)
  }

  # to keep class
  x[] <- x |>
    purrr::map(\(x) {
      x |>
        dplyr::relocate(c("concept_id", "excluded", "descendants", "mapped")) |>
        dplyr::mutate(concept_id = as.integer(.data$concept_id)) |>
        dplyr::arrange(.data$concept_id)
    })

  # alphabetical order
  if (length(x) > 0) {
    x <- x[order(names(x))]
  }

  return(x)
}


#' Print a concept set expression
#'
#' @param x A concept set expression
#' @param ...  Included for compatibility with generic. Not used.
#'
#' @return  Invisibly returns the input
#' @export
#'
#' @examples
#' asthma_cs <- list(
#'   "asthma_narrow" = dplyr::tibble(
#'     "concept_id" = 1,
#'     "excluded" = FALSE,
#'     "descendants" = TRUE,
#'     "mapped" = FALSE
#'   ),
#'   "asthma_broad" = dplyr::tibble(
#'     "concept_id" = c(1, 2),
#'     "excluded" = FALSE,
#'     "descendants" = TRUE,
#'     "mapped" = FALSE
#'   )
#' )
#' asthma_cs <- newConceptSetExpression(asthma_cs)
#' print(asthma_cs)
print.concept_set_expression <- function(x, ...) {
  cli::cli_h1("{length(x)} concept set expression{?s}")
  cli::cat_line("")
  disp <- 6
  len <- min(length(x), disp)
  for (i in seq_len(len)) {
    cli::cat_line(paste0("- ", names(x)[i], " (", nrow(x[[i]]), " concept criteria)"))
  }
  if (length(x) > disp) {
    cli::cat_line(paste0("along with ", length(x) - disp, " more concept sets"))
  }
  invisible(x)
}

# TODO DELETE WHEN WE DEPRECATE THE OLD CLASS
#' @export
print.conceptSetExpression <- function(x, ...) {
  cli::cli_h1("{length(x)} concept set expression{?s}")
  cli::cat_line("")
  disp <- 6
  len <- min(length(x), disp)
  for (i in seq_len(len)) {
    cli::cat_line(paste0("- ", names(x)[i], " (", nrow(x[[i]]), " concept criteria)"))
  }
  if (length(x) > disp) {
    cli::cat_line(paste0("along with ", length(x) - disp, " more concept sets"))
  }
  invisible(x)
}

#' Empty `concept_set_expression` object.
#'
#' @return An empty concept_set_expression object.
#'
#' @export
#'
#' @examples
#' emptyConceptSetExpression()
#'
emptyConceptSetExpression <- function() {
  newConceptSetExpression(list())
}

#' @export
as_tibble.concept_set_expression <- function(x, ...) {
  x |>
    unclass() |>
    dplyr::bind_rows(.id = "concept_set_expression_name")
}

#' @export
bind.concept_set_expression <- function(...) {
  c(...)
}

#' @export
c.concept_set_expression <- function(...) {
  # all codelists together
  allCodelists <- unlist(list(...), recursive = FALSE)
  allCodelists <- allCodelists[!duplicated(allCodelists)]

  # check for repeated names
  nms <- names(allCodelists)
  if (length(nms) != length(unique(nms))) {
    # identify repeated
    duplicated <- names(which(table(nms) > 1))
    id <- nms %in% duplicated
    dup <- nms[id]

    # assign new names
    nameChange <- character()
    for (k in seq_along(dup)) {
      oldName <- dup[k]
      newName <- purrr::map_chr(oldName, \(x) findNewName(x, nms))
      nms <- c(nms, newName)
      nameChange <- c(nameChange, rlang::set_names(newName, oldName))
    }

    # report name change
    msg <- purrr::imap_chr(nameChange, \(x, nm) paste0(nm, " -> ", x))
    names(msg) <- rep("*", length(msg))
    c("!" = "Repeated names found between concept_set_expression, renamed as:", msg) |>
      cli::cli_warn()

    names(allCodelists)[id] <- unname(nameChange)
  }

  # add class
  newConceptSetExpression(allCodelists)
}

#' @export
`[.concept_set_expression` <- function(x, i) {
  cl <- class(x)
  obj <- NextMethod()
  class(obj) <- cl
  return(obj)
}
