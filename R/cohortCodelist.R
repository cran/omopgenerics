
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

#' Get codelist from a cohort_table object.
#'
#' @param cohort A cohort_table object.
#' @param cohortId A particular cohort definition id that is present in the
#' cohort table. If NULL the codelists of all cohorts will be retrieved.
#' @param codelistType The reason for the codelist. Can be "index event", "inclusion
#' criteria", or "exit criteria".
#' @param type deprecated.
#' @param cohortTable deprecated.
#'
#' @return A table with the codelists used.
#'
#' @export
#'
#' @examples
#'  \donttest{
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
#' cohort <- tibble(
#'   cohort_definition_id = c(1, 1, 1, 2),
#'   subject_id = 1,
#'   cohort_start_date = as.Date(c(
#'     "2020-01-01", "2021-01-01", "2022-01-01", "2022-01-01"
#'   )),
#'   cohort_end_date = as.Date(c(
#'     "2020-01-01", "2021-01-01", "2022-01-01", "2022-01-01"
#'   ))
#' )
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "my_example_cdm",
#'   cohortTables = list("cohort1" = cohort)
#' )
#' cdm$cohort1 <- newCohortTable(table = cdm$cohort1,
#'                                 cohortCodelistRef = dplyr::tibble(
#'                                 cohort_definition_id = c(1,1,1,2,2),
#'                                 codelist_name =c("disease X", "disease X", "disease X",
#'                                                  "disease Y", "disease Y"),
#'                                 concept_id = c(1,2,3,4,5),
#'                                 codelist_type = "index event"
#'                               ))
#' cohortCodelist(cdm$cohort1, cohortId = 1, codelistType = "index event")
#' }
cohortCodelist <- function(cohort,
                           cohortId = NULL,
                           codelistType = c("index event",
                                            "inclusion criteria",
                                            "exit criteria"),
                           type = lifecycle::deprecated(),
                           cohortTable = lifecycle::deprecated()) {
  # input check
  if (lifecycle::is_present(cohortTable)) {
    lifecycle::deprecate_soft(when = "1.3.1",
                              what = "cohortCodelist(cohortTable= )",
                              with = "cohortCodelist(cohort= )")
    if (missing(cohort)) {
      cohort <- cohortTable
    }
  }
  cohort <- validateCohortArgument(cohort = cohort)
  cohortId <- validateCohortIdArgument(cohortId = cohortId, cohort = cohort)
  if (lifecycle::is_present(type)) {
    lifecycle::deprecate_warn(when = "1.2.0",
                              what = "cohortCodelist(type= )",
                              with = "cohortCodelist(codelistType= )")
    if (missing(codelistType)) {
      codelistType <- type
    }
  }
  assertChoice(codelistType, c("index event", "inclusion criteria", "exit criteria"))

  x <- attr(cohort, "cohort_codelist") |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)  |>
    dplyr::left_join(
      attr(cohort, "cohort_set") |>
        dplyr::select("cohort_definition_id", "cohort_name"),
      by = "cohort_definition_id"
    ) |>
    dplyr::collect()

  if (nrow(x) == 0) {
    cli::cli_warn("No codelists found for the specified cohorts")
    return(newCodelist(list()))
  }

  x <- x |>
    dplyr::filter(.data$codelist_type %in% .env$codelistType)

  if (nrow(x) == 0) {
    cli::cli_warn("No codelists found for the specified codelistType={codelistType}")
    return(newCodelist(list()))
  }

  x <- x |>
    dplyr::group_by(.data$codelist_name) |>
    dplyr::group_split() |>
    unclass()
  names(x) <- purrr::map_chr(x, \(x) unique(x$codelist_name))

  # check consistent naming
  x <- x |>
    purrr::imap(\(x, nm) {
      candidates <- x |>
        dplyr::group_by(.data$cohort_name, .data$codelist_type) |>
        dplyr::group_split() |>
        unclass()
      names(candidates) <- purrr::map_chr(candidates, \(x) {
        paste0("cohort name: '", unique(x$cohort_name), "', codelist type: '", unique(x$codelist_type), "'")
      })
      candidates <- purrr::map(candidates, \(x) as.integer(sort(unique(x$concept_id))))

      # check different values
      if (length(candidates) > 1) {
        err <- character()
        nms <- names(candidates)
        for (i in 1:(length(nms) - 1)) {
          for (j in (i+1):length(nms)) {
            if (!identical(candidates[[i]], candidates[[j]])) {
              err <- c(err, paste0(
                "- Codelist {.pkg ", nm, "} is not the same for {.emph ", nms[i], "} and {.emph ", nms[j], "}."
              ))
            }
          }
        }
        if (length(err) > 0) {
          return(err)
        }
      }

      candidates[[1]]
    })

  # check if multiple names
  err <- unlist(x[purrr::map_lgl(x, is.character)])
  if (length(err) > 0) {
    mes <- c(
      x = "Some codelists have the same name but different content, see: ",
      err,
      "i" = "Please use `cohortId` and/or `codelistType` to sleect the concepts of interest."
    )
    cli::cli_abort(message = mes)
  }

  # add class
  x <- newCodelist(x)

  return(x)
}
