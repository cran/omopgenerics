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
  containNA <- purrr::imap(codelist, \(x, nm) {
    if (any(is.na(x))) nm else character()
  }) |>
    purrr::flatten_chr()
  if (length(containNA) > 0) {
    cli::cli_abort("{.var {containNA}} must not contain NA.", call = call)
  }

  # check unique names
  if (length(names(codelist)) != length(unique(names(codelist)))) {
    cli::cli_abort("The names of the codelists most be unique.",call = call)
  }

  # alphabetical order
  if (length(codelist) > 0) {
    codelist <- codelist[order(names(codelist))] |>
      addClass("codelist")
  }

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
  if (length(x) <= 6) {
    for (i in seq_along(x)) {
      cli::cat_line(paste0("- ", names(x)[i], " (", length(x[[i]]), " codes)"))
    }
  } else {
    for (i in seq_along(x[1:6])) {
      cli::cat_line(paste0("- ", names(x[1:6])[i], " (", length(x[[i]]), " codes)"))
    }
    cli::cat_line(paste0("along with ", length(x) - 6, " more codelists"))
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
