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


#' 'codelist' object constructor
#'
#' @param x A named list where each element contains a vector of concept IDs.
#' @inheritParams conceptCdmDoc
#'
#' @return A codelist object.
#'
#' @export
#'
newCodelist <- function(x, cdm = NULL) {
  # constructor
  x <- constructCodelist(x)

  # validate
  x <- validateCodelist(x, cdm = cdm)

  return(x)
}

constructCodelist <- function(x) {
  if (inherits(x, "tbl")) {
    if (x |> dplyr::tally() |> dplyr::pull() == 0) {
      x <- list()
    } else {
      if ("concept_set_expression_name" %in% colnames(x) & !"codelist_name" %in% colnames(x)) {
        x <- x |>
          dplyr::rename("concept_set_expression_name" = "codelist_name")
      }
      if ("codelist_name" %in% colnames(x)) {
        x <- x |>
          dplyr::collect() |>
          dplyr::group_by(.data$codelist_name) |>
          dplyr::group_split() |>
          as.list()
        names(x) <- purrr::map_chr(x, \(x) unique(x$codelist_name))
        x <- purrr::map(x, \(x) as.integer(unique(x$concept_id)))
      }
    }
  }

  x |>
    addClass("codelist")
}

validateCodelist <- function(codelist,
                             nm = deparse1(substitute(codelist), backtick = TRUE),
                             cdm = NULL,
                             call = parent.frame()) {
  codelist |>
    assertList(
      named = TRUE, class = c("numeric", "integer", "integer64"),
      empty = TRUE, call = call
    )

  # check if they need to be cast
  if (purrr::map_lgl(codelist, \(x) inherits(x, "numeric") | inherits(x, "integer64")) |> any()) {
    codelist <- purrr::map(codelist, as.integer)
    cli::cli_warn(c("!" = "`codelist` cast to integers."))
  }

  # check if there is any NA
  containNA <- codelist |>
    purrr::keep(\(x) any(is.na(x))) |>
    names()
  if (length(containNA) > 0) {
    c(x = "{length(containNA)} codelist{?s} contain NA: {.var {containNA}}.") |>
      cli::cli_abort()
  }

  # unique and sort
  codelist <- purrr::map(codelist, \(x) sort(unique(x)))

  # check unique names
  if (length(names(codelist)) != length(unique(names(codelist)))) {
    cli::cli_abort("The names of the codelists most be unique.",call = call)
  }

  # alphabetical order
  if (length(codelist) > 0) {
    codelist <- codelist[order(names(codelist))]
  }

  checkCodelistInConcept(codelist, cdm = cdm, call = call)

  codelist <- codelist |>
    addClass("codelist")

  return(codelist)
}

checkCodelistInConcept <- function(codelist, cdm = NULL, call = parent.frame()) {
  if (missing(cdm) || is.null(cdm) || length(codelist) == 0) {
    return(invisible(codelist))
  }

  cdm <- validateCdmArgument(cdm, requiredTables = "concept", call = call)

  conceptIds <- codelist |>
    unlist(use.names = FALSE) |>
    unique() |>
    as.integer()

  if (length(conceptIds) == 0) {
    return(invisible(codelist))
  }

  presentIds <- cdm$concept |>
    dplyr::filter(.data$concept_id %in% .env$conceptIds) |>
    dplyr::distinct(.data$concept_id) |>
    dplyr::pull("concept_id") |>
    as.integer()

  missing <- codelist |>
    purrr::map(\(x) setdiff(x, presentIds)) |>
    purrr::keep(\(x) length(x) > 0)

  if (length(missing) > 0) {
    missingIds <- missing |>
      unlist(use.names = FALSE) |>
      unique() |>
      sort()
    if (length(missingIds) > 5) {
      cli::cli_warn(
        "{length(missingIds)} unique codelist concept IDs are not present in `cdm$concept`.",
        call = call
      )
    } else {
      bullets <- purrr::imap_chr(
        missing,
        \(x, nm) paste0(nm, ": ", paste0(x, collapse = ", "))
      )
      names(bullets) <- rep("*", length(bullets))
      cli::cli_warn(
        c(
          "The following codelist concept IDs are not present in `cdm$concept`:",
          bullets
        ),
        call = call
      )
    }
  }

  invisible(codelist)
}


#' Print a codelist
#'
#' @inheritParams codelistDoc
#' @inheritParams unusedDotsDoc
#'
#' @return  Invisibly returns the input
#' @export
#'
#' @examples
#' codes <- list("disease X" = c(1, 2, 3), "disease Y" = c(4, 5))
#' codes <- newCodelist(codes)
#' print(codes)
#'
print.codelist <- function(x, ...) {
  cli::cli_h1("{length(x)} codelist{?s}")
  cli::cat_line("")
  disp <- 6
  len <- min(length(x), disp)
  for (i in seq_len(len)) {
    cli::cat_line(paste0("- ", names(x)[i], " (", length(x[[i]]), " codes)"))
  }
  if (length(x) > disp) {
    cli::cat_line(paste0("along with ", length(x) - disp, " more codelists"))
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
#' emptyCodelist()
#'
emptyCodelist <- function() {
  newCodelist(list())
}

#' @export
bind.codelist <- function(...) {
  c(...)
}

#' @export
c.codelist <- function(...) {
  combineCodelist(x = list(...), type = "codelist")
}

combineCodelist <- function(x, type) {
  # function
  fun <- switch (type,
    "codelist" = newCodelist,
    "codelist_with_details" = newCodelistWithDetails,
    "concept_set_expression" = newConceptSetExpression
  )

  # check classes
  x <- x |>
    purrr::imap(\(element, nm) {
      if (!inherits(element, type)) {
        element <- tryCatch(fun(element), error = function(e) NULL)
        if (is.null(element)) {
          cli::cli_inform(c("!" = "Element `{nm}` eliminated as could not be converted to `<{type}>`."))
        } else {
          cli::cli_inform(c("i" = "Element `{nm}` converted to `<{type}>`."))
        }
      }
      element
    }) |>
    purrr::compact()

  # all codelists together
  x <- unlist(x, recursive = FALSE)

  # remove repeated codelists
  x <- removeRepeated(x = x)

  # rename repeated names
  x <- renameCodelists(x = x)

  # add class
  x <- fun(x)

  return(x)
}
removeRepeated <- function(x) {
  # check for repeated elements
  nms <- names(x)
  if (length(nms) != length(unique(nms))) {
    eliminate <- rep(FALSE, length(nms))
    for (k in seq_along(nms)) {
      if (!eliminate[k]) {
        # other codelist with same name
        id <- which(nms[k] == nms)
        id <- id[id > k]
        cont <- x[[k]]
        if (length(id) > 0) {
          for (i in id) {
            if (identical(cont, x[[i]])) {
              eliminate[i] <- TRUE
            }
          }
        }
      }
    }
    x <- x[!eliminate]
  }
  return(x)
}
renameCodelists <- function(x) {
  nms <- names(x)
  if (length(nms) != length(unique(nms))) {
    # find duplicated names
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
    c("!" = "Repeated names found between codelist, renamed as:", msg) |>
      cli::cli_warn()

    names(x)[id] <- unname(nameChange)
  }
  x
}
findNewName <- function(name, usedNames) {
  usedNames <- usedNames[startsWith(x = usedNames, prefix = paste0(name, "_"))]
  k <- 1
  newName <- paste0(name, "_", k)
  while(newName %in% usedNames) {
    k <- k + 1
    newName <- paste0(name, "_", k)
  }
  return(newName)
}

#' @export
`[.codelist` <- function(x, i) {
  cl <- class(x)
  obj <- NextMethod()
  class(obj) <- cl
  return(obj)
}

#' @export
as_tibble.codelist <- function(x, ...) {
  if (length(x) == 0) {
    return(dplyr::tibble(
      codelist_name = character(),
      concept_id = integer()
    ))
  }
  x |>
    purrr::map(\(x) dplyr::tibble(concept_id = as.integer(x))) |>
    dplyr::bind_rows(.id = "codelist_name")
}
