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

#' Restrict the cdm object to a subset of tables.
#'
#' @param cdm A cdm_reference object.
#' @param ... Selection of tables to use, it supports tidyselect expressions.
#'
#' @return A cdm_reference with only the specified tables.
#'
#' @export
#'
#' @examples
#' cdm <- emptyCdmReference("my cdm")
#' cdm
#'
#' cdm |>
#'   cdmSelect("person")
#'
cdmSelect <- function(cdm, ...) {
  assertClass(cdm, "cdm_reference")

  allTables <- names(cdm)
  toKeep <- allTables |>
    as.list() |>
    rlang::set_names(allTables) |>
    dplyr::as_tibble() |>
    dplyr::select(...) |>
    colnames()
  toDrop <- allTables[!allTables %in% toKeep]
  if (length(toDrop) > 0) {
    for (nm in toDrop) {
      cdm[[nm]] <- NULL
    }
  }
  return(cdm)
}
