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
#'
#' @return A codelist object.
#'
#' @export
#'
newCodelist <- function(x) {
  # constructor
  x <- constructCodelist(x)

  # validate
  x <- validateCodelist(x)

  return(x)
}

constructCodelist <- function(x) {
  if (inherits(x, "tbl") & all(c("concept_id", "codelist_name") %in% colnames(x))) {
    x <- x |>
      dplyr::collect() |>
      dplyr::group_by(.data$codelist_name) |>
      dplyr::group_split() |>
      as.list()
    names(x) <- purrr::map_chr(x, \(x) unique(x$codelist_name))
    x <- purrr::map(x, \(x) as.integer(unique(x$concept_id)))
  }

  x |>
    addClass("codelist")
}

validateCodelist <- function(codelist, call = parent.frame()) {
  codelist |>
    assertList(
      named = TRUE, class = c("numeric", "integer", "integer64"), call = call
    )

  # check if they need to be casted
  if (purrr::map_lgl(codelist, \(x) inherits(x, "numeric") | inherits(x, "integer64")) |> any()) {
    codelist <- purrr::map(codelist, as.integer)
    cli::cli_warn(c("!" = "`codelist` casted to integers."))
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

  codelist <- codelist |>
    addClass("codelist")

  return(codelist)
}


#' Print a codelist
#'
#' @param x A codelist
#' @param ...  Included for compatibility with generic. Not used.
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
    c("!" = "Repeated names found between codelist, renamed as:", msg) |>
      cli::cli_warn()

    names(allCodelists)[id] <- unname(nameChange)
  }

  # add class
  newCodelist(allCodelists)
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
  x |>
    purrr::map(\(x) dplyr::tibble(concept_id = as.integer(x))) |>
    dplyr::bind_rows(.id = "codelist_name")
}
