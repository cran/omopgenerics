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

#' Drop a table from a cdm object.
#'
#' @inheritParams cdmOrTableDoc
#' @param name Name(s) of the table(s) to drop. Tidyselect statements are
#' supported.
#'
#' @export
#'
#' @return The table in the cdm reference.
#'
dropSourceTable <- function(cdm, name) {
  UseMethod("dropSourceTable")
}

#' @export
dropSourceTable.cdm_reference <- function(cdm, name) {
  name <- rlang::enquo(name)
  namesCdm <- names(cdm)
  namesSource <- listSourceTables(cdm = cdm)
  toDrop <- c(namesCdm, namesSource) |>
    unique() |>
    tidySelect(selection = name, cohortAttributes = "add")
  toDropCdm <- namesCdm[namesCdm %in% toDrop]
  toDropSource <- namesSource[namesSource %in% toDrop]
  dropSourceTable(cdmSource(cdm), name = toDropSource)
  if (length(toDropCdm) > 0) {
    for (nm in toDropCdm) {
      cdm[[nm]] <- NULL
    }
  }
  return(invisible(cdm))
}

#' @export
dropSourceTable.cdm_table <- function(cdm, name) {
  cdm <- cdmReference(cdm)
  dropSourceTable.cdm_reference(cdm = cdm, name = {{ name }})
}

tidySelect <- function(options, selection, cohortAttributes = "ignore") {
  if (rlang::quo_is_null(selection)) {
    return(options)
  }
  opts <- options |>
    rlang::set_names() |>
    as.list() |>
    dplyr::as_tibble()
  if (selectType(selection) == "character") {
    selection <- rlang::eval_tidy(selection)
    opt <- opts |>
      dplyr::select(dplyr::any_of(selection))
  } else {
    opt <- opts |>
      dplyr::select(!!selection)
  }
  opt <- opt |>
    colnames() |>
    unique()
  if (cohortAttributes == "add") {
    expr <- paste0("(", paste0(cohortAttibutesSuffixes(), collapse = "|"), ")$")
    optSelected <- opt
    optBase <- optSelected[!grepl(expr, optSelected)]
    optAttributes <- optBase |>
      purrr::map(\(x) paste0(x, c("", cohortAttibutesSuffixes()))) |>
      purrr::flatten_chr() |>
      purrr::keep(\(x) x %in% options)
    if (isNegatedSelection(selection)) {
      opt <- optAttributes
    } else {
      opt <- c(optSelected, optAttributes) |>
        unique()
    }
  } else if (cohortAttributes == "remove") {
    expr <- paste0("(", paste0(cohortAttibutesSuffixes(), collapse = "|"), ")$")
    opt <- opt[!grepl(expr, opt)]
  }
  return(opt)
}
isNegatedSelection <- function(selection) {
  if (!rlang::is_quosure(selection)) {
    return(FALSE)
  }
  expr <- rlang::quo_get_expr(selection)
  rlang::is_call(expr, "!")
}
cohortAttibutesSuffixes <- function() {
  c("_set", "_attrition", "_codelist")
}

#' @export
dropSourceTable.local_cdm <- function(cdm, name) {
  return(invisible(TRUE))
}
