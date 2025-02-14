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

#' Insert a cdm_reference object to a different source.
#'
#' @param cdm A cdm_reference, if not local it will be collected into memory.
#' @param to A cdm_source or another cdm_reference, with a valid cdm_source.
#'
#' @return The first cdm_reference object inserted to the source.
#' @export
#'
insertCdmTo <- function(cdm,
                        to) {
  UseMethod("insertCdmTo", object = to)
}

#' @export
insertCdmTo.cdm_reference <- function(cdm, to) {
  insertCdmTo(cdm = cdm, to = cdmSource(to))
}

#' @export
insertCdmTo.local_cdm <- function(cdm, to) {
  dplyr::collect(cdm)
}
