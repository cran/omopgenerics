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

#' Disconnect from a cdm object.
#'
#' @param cdm A cdm reference or the source of a cdm reference.
#' @param ... Used for consistency.
#'
#' @export
#'
#' @return TRUE if process wass successful.
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' person <- tibble(
#'   person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
#'   race_concept_id = 0, ethnicity_concept_id = 0
#' )
#' observation_period <- tibble(
#'   observation_period_id = 1, person_id = 1,
#'   observation_period_start_date = as.Date("2000-01-01"),
#'   observation_period_end_date = as.Date("2023-12-31"),
#'   period_type_concept_id = 0
#' )
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "mock"
#' )
#'
#' cdmDisconnect(cdm)
#'
cdmDisconnect <- function(cdm, ...) {
  UseMethod("cdmDisconnect")
}

#' @export
cdmDisconnect.cdm_reference <- function(cdm, ...) {
  cdmDisconnect(cdmSource(cdm), ...)
}

#' @export
cdmDisconnect.local_cdm <- function(cdm, ...) {
  invisible(TRUE)
}
