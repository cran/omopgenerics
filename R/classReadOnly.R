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

#' A new read-only source for the cdm
#'
#' This source can be used for `cdm_reference` objects that should allow reading
#' existing CDM tables, but should not allow creating, computing, inserting,
#' reading from, or dropping source tables.
#'
#' @return A read-only source in the format of a cdm source.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' newReadOnlySource()
#' }
#'
newReadOnlySource <- function() {
  structure(.Data = list(), class = "read_only_source") |>
    newCdmSource(sourceType = "read_only")
}

abortReadOnlySource <- function(fun) {
  cli::cli_abort(c(x = "{.fn {fun}} is not available for a read-only CDM source."))
}

#' @export
insertTable.read_only_source <- function(cdm, ...) {
  abortReadOnlySource("insertTable")
}

#' @export
dropSourceTable.read_only_source <- function(cdm, ...) {
  abortReadOnlySource("dropSourceTable")
}

#' @export
readSourceTable.read_only_source <- function(cdm, ...) {
  abortReadOnlySource("readSourceTable")
}

#' @export
#' @importFrom dplyr compute
compute.read_only_source <- function(x, ...) {
  abortReadOnlySource("compute")
}

#' @export
insertFromSource.read_only_source <- function(cdm, ...) {
  abortReadOnlySource("insertFromSource")
}

#' @export
cdmTableFromSource.read_only_source <- function(src, ...) {
  abortReadOnlySource("cdmTableFromSource")
}

#' @export
insertCdmTo.read_only_source <- function(cdm, to) {
  abortReadOnlySource("insertCdmTo")
}

#' @export
listSourceTables.read_only_source <- function(cdm) {
  character()
}

#' @export
cdmDisconnect.read_only_source <- function(cdm, ...) {
  invisible(TRUE)
}

#' @export
summary.read_only_source <- function(object, ...) {
  list(package = "omopgenerics")
}
