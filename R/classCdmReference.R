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

#' `cdm_reference` objects constructor
#'
#' @param tables List of tables that are part of the OMOP Common Data Model
#' reference.
#' @param cdmName Name of the cdm object.
#' @param cdmVersion Version of the cdm. Supported versions 5.3 and 5.4.
#' @param .softValidation Whether to perform a soft validation of consistency.
#' If set to FALSE, non overlapping observation periods are ensured.
#'
#'
#' @return A `cdm_reference` object.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdmTables <- list(
#'   "person" = tibble(
#'     person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
#'     race_concept_id = 0, ethnicity_concept_id = 0
#'   ) |>
#'     newCdmTable(newLocalSource(), "person"),
#'   "observation_period" = tibble(
#'     observation_period_id = 1, person_id = 1,
#'     observation_period_start_date = as.Date("2000-01-01"),
#'     observation_period_end_date = as.Date("2023-12-31"),
#'     period_type_concept_id = 0
#'   ) |>
#'     newCdmTable(newLocalSource(), "observation_period")
#' )
#' cdm <- newCdmReference(tables = cdmTables, cdmName = "mock")
#'
#' cdm
#' }
newCdmReference <- function(tables,
                            cdmName,
                            cdmVersion = NULL,
                            .softValidation = FALSE) {
  # inputs
  assertList(tables, named = TRUE, class = "cdm_table")
  assertCharacter(cdmName, length = 1)
  assertLogical(.softValidation, length = 1)

  # get cdm version
  if (is.null(cdmVersion)) {
    cdmVersion <- getVersion(tables)
  }

  # get cdm source
  if (length(tables) < 2) {
    cli::cli_abort("At least person and observation_period should be provided in tables")
  }

  # constructor
  cdm <- constructCdmReference(
    tables = tables, cdmName = cdmName, cdmVersion = cdmVersion,
    cdmSource = tableSource(tables[[1]])
  )

  # validate
  cdm <- validateCdmReference(cdm, soft = .softValidation)

  return(cdm)
}

getVersion <- function(cdm) {
  version <- tryCatch(
    {
      version <- cdm[["cdm_source"]] |>
        dplyr::pull("cdm_version") |>
        as.character()
      if (tolower(substr(version, 1, 1)) == "v") {
        version <- substr(version, 2, nchar(version))
      }
      substr(version, 1, 3)
    },
    error = function(e) {
      "5.3"
    }
  )
  return(version)
}
constructCdmReference <- function(tables, cdmName, cdmVersion, cdmSource) {
  structure(
    .Data = tables,
    cdm_name = cdmName,
    cdm_source = cdmSource,
    cdm_version = cdmVersion,
    class = "cdm_reference"
  )
}
validateCdmReference <- function(cdm, soft) {
  # assert version
  version <- cdmVersion(cdm)
  assertChoice(version, names(fieldsTables), length = 1)

  # assert source
  assertClass(cdmSource(cdm), "cdm_source")

  # assert lowercase names
  xNames <- names(cdm)
  x <- xNames[xNames != tolower(xNames)]
  if (length(x) > 0) {
    cli::cli_abort(
      "table names should be lower case; {combine(x)} {verb(x)} not."
    )
  }

  # assert compulsory tables
  compulsoryTables <- c("person", "observation_period")
  x <- compulsoryTables[!compulsoryTables %in% xNames]
  if (length(x) > 0) {
    cli::cli_abort("{combine(x)} {verb(x)} not included in the cdm object")
  }

  # validate omop tables
  omopTables <- omopTables(version = version)
  omopTables <- omopTables[omopTables %in% xNames]
  for (nm in omopTables) {
    if (nm %in% c("person", "observation_period")) {
      cdm[[nm]] <- newOmopTable(cdm[[nm]], version = version, cast = !soft)
    } else {
      cdm[[nm]] <- tryCatch(
        expr = {
          newOmopTable(cdm[[nm]], version = version, cast = !soft)
        },
        error = function(e) {
          cli::cli_warn(c(
            "{nm} table not included in cdm because:", as.character(e)
          ))
          return(NULL)
        }
      )
    }
  }

  # validate achilles tables
  achillesTables <- achillesTables(version = version)
  achillesTables <- achillesTables[achillesTables %in% xNames]
  for (nm in achillesTables) {
    cdm[[nm]] <- tryCatch(
      expr = {
        newAchillesTable(cdm[[nm]], version = version, cast = !soft)
      },
      error = function(e) {
        cli::cli_warn(c(
          "{nm} table not included in cdm because:", as.character(e)
        ))
        return(NULL)
      }
    )
  }

  # not overlapping periods
  if (!soft) {
    checkOverlapObservation(cdm$observation_period)
    checkStartBeforeEndObservation(cdm$observation_period)
    checkPlausibleObservationDates(cdm$observation_period)
  }

  return(invisible(cdm))
}
combine <- function(x) {
  if (length(x) < 2) {
    return(x)
  }
  paste0(paste0(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}
verb <- function(x) {
  ifelse(length(x) == 1, "is", "are")
}
plural <- function(x) {
  ifelse(length(x) == 1, "", "s")
}
checkColumnsCdm <- function(table, nm, required, call = parent.frame()) {
  columns <- colnames(table)

  # check required
  x <- required[!required %in% columns]
  if (length(x) > 0) {
    "{combine(x)} {verb(x)} not present in table {nm}" |>
      cli::cli_abort(call = call)
  }

  return(invisible(TRUE))
}
checkOverlapObservation <- function(x, call = parent.frame()) {
  x <- x |>
    dplyr::group_by(.data$person_id) |>
    dplyr::arrange(.data$observation_period_start_date) |>
    dplyr::mutate("next_observation_period_start_date" = dplyr::lead(
      .data$observation_period_start_date
    )) |>
    dplyr::mutate(overlap = dplyr::if_else(!is.na(.data$next_observation_period_start_date) &
                                           .data$observation_period_end_date >=
                                           .data$next_observation_period_start_date,
                                           1L, 0L)) |>
    dplyr::select(c("person_id", "overlap")) |>
    dplyr::filter(.data$overlap == 1L) |>
    dplyr::collect()
  if (nrow(x) > 0) {
    x5 <- x |>
      dplyr::ungroup() |>
      dplyr::select("person_id") |>
      utils::head(5) |>
      dplyr::pull()
    if(length(x) < 5){
    cli::cli_abort(
      message = c(
        "There is overlap between observation_periods, {nrow(x)} overlap{?s}
        detected for person ID {x5}"
      ),
      call = call
    )
    } else {
     cli::cli_abort(
        message = c(
          "There is overlap between observation_periods, {nrow(x)} overlap{?s}
        detected. First five affected person ID {x5}"
        ),
        call = call
      )
    }
  }
}
checkStartBeforeEndObservation <- function(x, call = parent.frame()) {
  x <- x |>
    dplyr::mutate(start_end = dplyr::if_else(.data$observation_period_start_date >
                                               .data$observation_period_end_date,
                                           1L, 0L)) |>
    dplyr::select("person_id", "start_end") |>
    dplyr::filter(.data$start_end == 1L) |>
    dplyr::collect()
  if (nrow(x) > 0) {
    x5 <- x |>
      dplyr::ungroup() |>
      dplyr::select("person_id") |>
      utils::head(5) |>
      dplyr::pull()

    if(length(x) < 5){
      cli::cli_abort(
        message = c(
          "There is overlap between observation_periods, {nrow(x)} overlap{?s}
        detected for person ID {x5}"
        ),
        call = call
      )
    } else {
      cli::cli_abort(
        message = c(
          "There are observation periods with start date after end date, {nrow(x)}
        detected. First five affected person ID {x5}"
        ),
        call = call
      )
    }
  }
}
checkPlausibleObservationDates <- function(x, call = parent.frame()) {
  if (isTRUE(nrow(x |> utils::head(1) |> dplyr::collect()) > 0)) {
    x <- x |>
      dplyr::summarise(
        min_obs_start = min(.data$observation_period_start_date,
          na.rm = TRUE
        ),
        max_obs_end = max(.data$observation_period_end_date,
          na.rm = TRUE
        )
      ) |>
      dplyr::collect()

    if (as.Date(x$min_obs_start) < as.Date("1800-01-01")) {
      cli::cli_warn(
        message = c("There are observation period start dates before 1800-01-01",
          "i" = "The earliest min observation period start date found is {x$min_obs_start}"
        ),
        call = call
      )
    }

    if (as.Date(x$max_obs_end) > Sys.Date()) {
      cli::cli_warn(
        message = c("There are observation period end dates after the current date: {Sys.Date()}",
          "i" = "The latest max observation period end date found is {x$max_obs_end}"
        ),
        call = call
      )
    }
  }
}
checkPerson <- function(cdm, call = parent.frame()) {
  # check for table with persion_id or subject_id
  tableName <- names(cdm)

  tables <- tableName |>
    purrr::map_lgl(\(x) {
      id <- c("person_id", "subject_id")
      id <- id[id %in% colnames(cdm[[x]])]
      if (length(id) == 1) {
        cdm[[x]] |>
          dplyr::select("person_id" = dplyr::all_of(id)) |>
          dplyr::anti_join(cdm$person, by = "person_id") |>
          utils::head(1) |>
          dplyr::tally() |>
          dplyr::pull() > 0
      } else {
        FALSE
      }
    })

  tables <- tableName[tables]


  if (!is.null(tables)) {
    cli::cli_warn(
      message = c(
        "There are person_id in the {tables} table but not in person table"
      ),
      call = call
    )
  }

  return(invisible(cdm))
}

#' Get the name of a cdm_reference associated object
#'
#' @param x A cdm_reference or cdm_table object.
#'
#' @return Name of the cdm_reference.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' cdmName(cdm)
#'
#' cdmName(cdm$person)
#' }
cdmName <- function(x) {
  UseMethod("cdmName")
}

#' @export
cdmName.cdm_reference <- function(x) {
  attr(x, "cdm_name")
}

#' @export
cdmName.cdm_table <- function(x) {
  x |>
    cdmReference() |>
    cdmName()
}

#' @export
cdmName.default <- function(x) {
  NULL
}

#' Get the version of an object.
#'
#' @param x Object to know the cdm version of an object.
#'
#' @return A character vector indicating the cdm version.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' cdmVersion(cdm)
#' cdmVersion(cdm$person)
#' }
cdmVersion <- function(x) {
  UseMethod("cdmVersion")
}

#' @export
cdmVersion.cdm_reference <- function(x) {
  attr(x, "cdm_version")
}

#' @export
cdmVersion.cdm_table <- function(x) {
  x |>
    cdmReference() |>
    cdmVersion()
}

#' @export
cdmVersion.default <- function(x) {
  NULL
}

#' Get the cdmSource of an object.
#'
#' @param x Object to obtain the cdmSource.
#' @param cdm Deprecated, use x please.
#'
#' @return A cdm_source object.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' cdmSource(cdm)
#' cdmSource(cdm$person)
#' }
cdmSource <- function(x, cdm = lifecycle::deprecated()) {
  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_soft(when = "0.3.0", what = "cdmSource(cdm = )", with = "cdmSource(x = )")
    return(cdmSource(x = cdm))
  }
  UseMethod("cdmSource")
}

#' @export
cdmSource.cdm_reference <- function(x, ...) {
  attr(x, "cdm_source")
}

#' @export
cdmSource.cdm_table <- function(x, ...) {
  x |>
    cdmReference() |>
    cdmSource()
}

#' @export
cdmSource.default <- function(x, ...) {
  NULL
}

#' Get the source type of a cdm_reference object.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param  cdm A cdm_reference object.
#'
#' @return A character vector with the type of source of the cdm_reference
#' object.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' cdmSourceType(cdm)
#' }
cdmSourceType <- function(cdm) {
  lifecycle::deprecate_soft(
    when = "0.3.0", what = "cdmSourceType()", with = "sourceType()"
  )
  sourceType(cdm)
}

#' Subset a cdm reference object.
#'
#' @param x A cdm reference.
#' @param name The name of the table to extract from the cdm object.
#'
#' @return A single cdm table reference
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' cdm$person
#' }
`$.cdm_reference` <- function(x, name) {
  x[[name]]
}

#' Subset a cdm reference object.
#'
#' @param x A cdm reference
#' @param name The name or index of the table to extract from the cdm object.
#'
#' @return A single cdm table reference
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' cdm[["person"]]
#' }
`[[.cdm_reference` <- function(x, name) {
  if (length(name) > 1) {
    cli::cli_abort("You can only read one table of a cdm_reference.")
  }
  if (is.numeric(name)) {
    if (name > length(x)) {
      return(NULL)
    } else {
      name <- names(x)[name]
    }
  }
  if (all(!name %in% names(x))) {
    cli::cli_abort("{name} does not exist in the cdm_reference object.")
  }
  xraw <- unclass(x)
  tbl <- xraw[[name]]
  attr(tbl, "cdm_reference") <- x
  return(tbl)
}

#' Assign an table to a cdm reference.
#'
#' @param cdm A cdm reference.
#' @param name Name where to assign the new table.
#' @param value Table with the same source than the cdm object.
#'
#' @return The cdm reference.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = dplyr::tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = dplyr::tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' cdm$person
#' }
`$<-.cdm_reference` <- function(cdm, name, value) {
  cdm[[name]] <- value
  return(cdm)
}

#' Assign a table to a cdm reference.
#'
#' @param cdm A cdm reference.
#' @param name Name where to assign the new table.
#' @param value Table with the same source than the cdm object.
#'
#' @return The cdm reference.
#'
#' @export
#'
`[[<-.cdm_reference` <- function(cdm, name, value) {
  # check consistent naming of value
  if (!is.null(value)) {
    if (!"cdm_table" %in% class(value)) {
      call <- parent.frame()
      value <- tryCatch(
        expr = cdmTableFromSource(cdmSource(cdm), value),
        error = function(e) {
          value
        }
      )
      if (!"cdm_table" %in% class(value)) {
        value <- tryCatch(
          expr = insertFromSource(cdm, value),
          error = function(e) {
            cli::cli_abort(
              message = c(
                "An object of class {class(value)} cannot be assigned to
              a cdm_reference. You can only assign cdm_tables to a
              cdm_reference object or objects that can be converted to a
              cdm_table. Please use insertTable to insert tibbles to a
              cdm_reference.",
                "!" = "Error when trying to convert to a cdm_table:",
                as.character(e$message)
              ),
              call = call
            )
          }
        )
      }
    }
    if (!identical(tableSource(value), cdmSource(cdm))) {
      cli::cli_abort("Table and cdm does not share a common source.")
    }
    remoteName <- tableName(value)
    if (!is.na(remoteName) && name != remoteName) {
      cli::cli_abort(
        "You can't assign a table named {remoteName} to {name}. Please use
        compute to change table name."
      )
    }
    if (remoteName %in% omopTables()) {
      value <- value |> newOmopTable()
    }
    if (remoteName %in% achillesTables()) {
      value <- value |> newAchillesTable()
    }
    if ("cohort_table" %in% class(value)) {
      # value <- value |> castCohort()
    }
  }

  if (length(name) > 1) {
    cli::cli_abort("You can only edit one table of a cdm_reference.")
  }

  # check name lowercase
  if (all(name != tolower(name))) {
    cli::cli_abort("name should be lowercase but is {name}.")
  }

  # remove cdm_reference
  attr(value, "cdm_reference") <- NULL

  # assign
  cl <- class(cdm)
  cdm <- unclass(cdm)
  cdm[[name]] <- value
  class(cdm) <- cl

  return(cdm)
}

#' Print a CDM reference object
#'
#' @param x A cdm_reference object
#' @param ... Included for compatibility with generic. Not used.
#'
#' @return Invisibly returns the input
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = dplyr::tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = dplyr::tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' print(cdm)
#' }
print.cdm_reference <- function(x, ...) {
  type <- sourceType(x)
  name <- cdmName(x)
  classes <- cdmClasses(cdm = x) |>
    purrr::map(\(x) {
      if (length(x) == 0) {
        "-"
      } else {
        paste0(x, collapse = ", ")
      }
    })

  # print object
  cli::cli_h1("# OMOP CDM reference ({type}) of {name}")
  cli::cli_li(paste("{.strong omop tables:}", classes$omop_table))
  cli::cli_li(paste("{.strong cohort tables:}", classes$cohort_table))
  cli::cli_li(paste("{.strong achilles tables:}", classes$achilles_table))
  cli::cli_li(paste("{.strong other tables:}", classes$cdm_table))

  # output
  invisible(x)
}

#' Retrieves the cdm reference into a local cdm.
#'
#' @param x A cdm_reference object.
#' @param ... For compatibility only, not used.
#'
#' @return A local cdm_reference.
#'
#' @export
#'
#' @importFrom dplyr collect
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = dplyr::tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = dplyr::tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' collect(cdm)
#' }
collect.cdm_reference <- function(x, ...) {
  name <- cdmName(x)
  x <- unclass(x)
  tables <- list()
  cohortTables <- list()
  for (nm in names(x)) {
    if (inherits(x[[nm]], "cohort_table")) {
      cohortTables[[nm]] <- x[[nm]] |> dplyr::collect()
    } else {
      tables[[nm]] <- x[[nm]] |> dplyr::collect()
    }
  }
  cdm <- cdmFromTables(
    tables = tables, cdmName = name, cohortTables = cohortTables
  )
  return(cdm)
}

#' Standard tables that a cdm reference can contain in the OMOP Common Data
#' Model.
#'
#' @param version Version of the OMOP Common Data Model.
#'
#' @return Standard tables
#'
#' @export
#'
#' @examples
#' library(omopgenerics)
#'
#' omopTables()
#'
omopTables <- function(version = "5.3") {
  assertVersion(version = version)
  tableChoice(version = version, type = "cdm_table")
}

#' Required columns that the standard tables in the OMOP Common Data Model must
#' have.
#'
#' @param table Table to see required columns.
#' @param field Name of the specific field.
#' @param version Version of the OMOP Common Data Model.
#' @param onlyRequired deprecated
#'
#' @return Character vector with the column names
#'
#' @export
#'
#' @examples
#' library(omopgenerics)
#'
#' omopColumns("person")
#'
omopColumns <- function(table, field = NULL, version = "5.3", onlyRequired = lifecycle::deprecated()) {
  # lifecycle
  if (lifecycle::is_present(onlyRequired)) {
    lifecycle::deprecate_soft("0.4.0", "omopColumns(onlyRequired = )")
  }

  # initial checks
  assertVersion(version = version)
  assertTableName(table = table, version = version, type = "cdm_table")
  assertChoice(field, choices = colnames(fieldTablesColumns), null = TRUE)

  # get columns
  if (is.null(field)) {
    res <- getColumns(table = table, version = version, type = "cdm_table", required = FALSE)
  } else {
    res <- fieldTablesColumns[[field]][fieldTablesColumns$table_name == table]
  }
  return(res)
}

#' Cohort tables that a cdm reference can contain in the OMOP Common Data
#' Model.
#'
#' @param version Version of the OMOP Common Data Model.
#'
#' @return cohort tables
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' cohortTables()
#' }
cohortTables <- function(version = "5.3") {
  assertVersion(version = version)
  tableChoice(version = version, type = "cohort")
}

#' Required columns for a generated cohort set.
#'
#' @param table Either `cohort`, `cohort_set` or `cohort_attrition`
#' @param version Version of the OMOP Common Data Model.
#'
#' @return Character vector with the column names
#'
#' @return Required columns
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' cohortColumns("cohort")
#' }
cohortColumns <- function(table, version = "5.3") {
  assertVersion(version = version)
  assertTableName(table = table, version = version, type = "cohort")
  getColumns(table = table, version = version, type = "cohort", required = TRUE)
}

#' Names of the tables that contain the results of achilles analyses
#'
#' @param version Version of the OMOP Common Data Model.
#'
#' @return Names of the tables that are contain the results from the achilles
#' analyses
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' achillesTables()
#' }
achillesTables <- function(version = "5.3") {
  assertVersion(version = version)
  tableChoice(version = version, type = "achilles")
}

#' Required columns for each of the achilles result tables
#'
#' @param table Table for which to see the required columns. One of
#' "achilles_analysis", "achilles_results", or "achilles_results_dist".
#' @param version Version of the OMOP Common Data Model.
#' @param onlyRequired deprecated.
#'
#' @return Character vector with the column names
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' achillesColumns("achilles_analysis")
#' achillesColumns("achilles_results")
#' achillesColumns("achilles_results_dist")
#' }
achillesColumns <- function(table, version = "5.3", onlyRequired = lifecycle::deprecated()) {
  if (lifecycle::is_present(onlyRequired)) {
    lifecycle::deprecate_soft("0.4.0", "omopColumns(onlyRequired = )")
  }
  assertVersion(version = version)
  assertTableName(table = table, version = version, type = "achilles")
  getColumns(table = table, version = version, type = "achilles", required = FALSE)
}

assertVersion <- function(version, call = parent.frame()) {
  assertChoice(x = version, choices = names(fieldsTables), call = call)
}
assertTableName <- function(table, version, type, call = parent.frame()) {
  assertChoice(x = table, choices = tableChoice(version, type), call = call)
}
tableChoice <- function(version, type) {
  x <- omopTableFields(version)
  unique(x$cdm_table_name[x$type == type])
}
getColumns <- function(table, version, type, required) {
  x <- omopTableFields(version)
  if (isTRUE(required)) {
    x$cdm_field_name[
      x$cdm_table_name == table & x$is_required == TRUE & x$type == type
    ]
  } else {
    x$cdm_field_name[x$cdm_table_name == table & x$type == type]
  }
}

#' @export
str.cdm_reference <- function(object, ...) {
  res <- cli::cli_text(
    "A cdm reference of {cdmName(object)} with {length(object)} table{?s}: {names(object)}."
  ) |>
    cli::cli_fmt()
  cat(res)
}

#' Create an empty cdm_reference
#'
#' @param cdmName Name of the cdm_reference
#' @param cdmVersion Version of the cdm_reference
#'
#' @export
#'
#' @return An empty cdm_reference
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' emptyCdmReference(cdmName = "my_example_cdm")
#' }
emptyCdmReference <- function(cdmName, cdmVersion = NULL) {
  cdmFromTables(
    tables = list(
      "person" = emptyOmopTableInternal("person"),
      "observation_period" = emptyOmopTableInternal("observation_period")
    ),
    cdmName = cdmName,
    cdmVersion = cdmVersion
  )
}

emptyOmopTableInternal <- function(name, version = "5.3") {
  omopTableFields(version) |>
    dplyr::filter(.data$cdm_table_name == name & .data$type == "cdm_table") |>
    emptyTable()
}

#' Separate the cdm tables in classes
#'
#' @param cdm A cdm_reference object.
#'
#' @return A list of table names, the name of the list indicates the class.
#' @export
#'
cdmClasses <- function(cdm) {
  # initial check
  cdm <- validateCdmArgument(cdm = cdm)

  # options
  opts <- c("omop_table", "cohort_table", "achilles_table", "cdm_table")

  # get classes
  classes <- names(cdm) |>
    rlang::set_names() |>
    purrr::map_chr(\(nm) {
      dplyr::coalesce(dplyr::first(opts[opts %in% class(cdm[[nm]])]), "")
    }) |>
    purrr::keep(\(x) x != "")

  # result
  opts |>
    rlang::set_names() |>
    purrr::map(\(x) sort(as.character(names(classes[classes == x]))))
}
