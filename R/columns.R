#' Identify variables in group_name column
#'
#' @param result A tibble.
#'
#' @return Unique values of the group name column.
#' @description Identifies and returns the unique values in group_name column.
#'
#' @export
#'
#' @examples {
#'   library(dplyr)
#'   library(omopgenerics)
#'
#'   x <- tibble(
#'     "result_id" = as.integer(c(1, 2)),
#'     "cdm_name" = c("cprd", "eunomia"),
#'     "group_name" = "cohort",
#'     "group_level" = "my_cohort",
#'     "strata_name" = "sex",
#'     "strata_level" = "male",
#'     "variable_name" = "Age group",
#'     "variable_level" = "10 to 50",
#'     "estimate_name" = "count",
#'     "estimate_type" = "numeric",
#'     "estimate_value" = "5",
#'     "additional_name" = "overall",
#'     "additional_level" = "overall"
#'   ) |>
#'     newSummarisedResult(settings = tibble(
#'       "result_id" = c(1, 2), "custom" = c("A", "B")
#'     ))
#'
#'   x
#'
#'   x |> groupColumns()
#' }
#'
groupColumns <- function(result) {
  nameLevelColumns(result, "group")
}

#' Identify variables in strata_name column
#'
#' @param result A tibble.
#'
#' @return Unique values of the strata name column.
#' @description Identifies and returns the unique values in strata_name column.
#'
#' @export
#'
#' @examples {
#'   library(dplyr)
#'   library(omopgenerics)
#'
#'   x <- tibble(
#'     "result_id" = as.integer(c(1, 2)),
#'     "cdm_name" = c("cprd", "eunomia"),
#'     "group_name" = "cohort_name",
#'     "group_level" = "my_cohort",
#'     "strata_name" = "sex",
#'     "strata_level" = "male",
#'     "variable_name" = "Age group",
#'     "variable_level" = "10 to 50",
#'     "estimate_name" = "count",
#'     "estimate_type" = "numeric",
#'     "estimate_value" = "5",
#'     "additional_name" = "overall",
#'     "additional_level" = "overall"
#'   ) |>
#'     newSummarisedResult(settings = tibble(
#'       "result_id" = c(1, 2), "custom" = c("A", "B")
#'     ))
#'
#'   x
#'
#'   x |> strataColumns()
#' }
#'
strataColumns <- function(result) {
  nameLevelColumns(result, "strata")
}

#' Identify variables in additional_name column
#'
#' @param result A tibble.
#'
#' @return Unique values of the additional name column.
#' @description Identifies and returns the unique values in additional_name
#' column.
#'
#' @export
#'
#' @examples {
#'   library(dplyr)
#'   library(omopgenerics)
#'
#'   x <- tibble(
#'     "result_id" = as.integer(c(1, 2)),
#'     "cdm_name" = c("cprd", "eunomia"),
#'     "group_name" = "cohort_name",
#'     "group_level" = "my_cohort",
#'     "strata_name" = "sex",
#'     "strata_level" = "male",
#'     "variable_name" = "Age group",
#'     "variable_level" = "10 to 50",
#'     "estimate_name" = "count",
#'     "estimate_type" = "numeric",
#'     "estimate_value" = "5",
#'     "additional_name" = "overall",
#'     "additional_level" = "overall"
#'   ) |>
#'     newSummarisedResult(settings = tibble(
#'       "result_id" = c(1, 2), "custom" = c("A", "B")
#'     ))
#'
#'   x
#'
#'   x |> additionalColumns()
#' }
#'
additionalColumns <- function(result) {
  nameLevelColumns(result, "additional")
}

nameLevelColumns <- function(result, prefix) {
  if (prefix %in% colnames(attr(result, "settings"))) {
    x <- result |>
      settings()
    if ("result_id" %in% colnames(result)) {
      # we need a release of CohortSurvival for this feature
      # x <- x |>
      #   dplyr::filter(.data$result_id %in% unique(.env$result$result_id))
    } else {
      cli::cli_inform("{.var result_id} is not present in {.pkg result}.")
    }
    x <- x |>
      dplyr::select(dplyr::all_of(prefix)) |>
      dplyr::pull()
  } else {
    cli::cli_inform("{.var {prefix}} is not present in {.pkg settings}.")
    x <- result |>
      dplyr::pull(dplyr::all_of(paste0(prefix, "_name")))
  }
  x |>
    unique() |>
    getLabels() |>
    purrr::flatten_chr() |>
    unique()
}

#' Identify settings columns of a `<summarised_result>`
#'
#' @param result A `<summarised_result>`.
#' @param metadata Whether to include metadata columns in settings or not.
#'
#' @return Vector with names of the settings columns
#' @description Identifies and returns the columns of the settings table
#' obtained by using `settings()` in a `<summarised_result>` object.
#'
#' @export
#'
#' @examples {
#'   library(dplyr)
#'   library(omopgenerics)
#'
#'   x <- tibble(
#'     "result_id" = as.integer(c(1, 2)),
#'     "cdm_name" = c("cprd", "eunomia"),
#'     "group_name" = "cohort_name",
#'     "group_level" = "my_cohort",
#'     "strata_name" = "sex",
#'     "strata_level" = "male",
#'     "variable_name" = "Age group",
#'     "variable_level" = "10 to 50",
#'     "estimate_name" = "count",
#'     "estimate_type" = "numeric",
#'     "estimate_value" = "5",
#'     "additional_name" = "overall",
#'     "additional_level" = "overall"
#'   ) |>
#'     newSummarisedResult(settings = tibble(
#'       "result_id" = c(1, 2), "custom" = c("A", "B")
#'     ))
#'
#'   x
#'
#'   x |> settingsColumns()
#' }
settingsColumns <- function(result,
                            metadata = FALSE) {
  # retrieve settings columns
  cols <- result |>
    settings() |>
    purrr::map(\(x) x[!is.na(x)]) |>
    purrr::compact() |>
    names()
  if (metadata) {
    exclude <- "result_id"
  } else {
    exclude <- c(
      "result_id", "result_type", "package_name", "package_version", "group",
      "strata", "additional", "min_cell_count"
    )
  }
  cols[!cols %in% exclude]
}

#' Identify tidy columns of a `<summarised_result>`
#'
#' @param result A `<summarised_result>`.
#'
#' @return Table columns after applying `tidy()` function to a
#' `<summarised_result>`.
#'
#' @description Identifies and returns the columns that the tidy version of the
#' `<summarised_result>` will have.
#'
#' @export
#'
#' @examples {
#'   library(dplyr)
#'   library(omopgenerics)
#'
#'   x <- tibble(
#'     "result_id" = as.integer(c(1, 2)),
#'     "cdm_name" = c("cprd", "eunomia"),
#'     "group_name" = "cohort_name",
#'     "group_level" = "my_cohort",
#'     "strata_name" = "sex",
#'     "strata_level" = "male",
#'     "variable_name" = "Age group",
#'     "variable_level" = "10 to 50",
#'     "estimate_name" = "count",
#'     "estimate_type" = "numeric",
#'     "estimate_value" = "5",
#'     "additional_name" = "overall",
#'     "additional_level" = "overall"
#'   ) |>
#'     newSummarisedResult(settings = tibble(
#'       "result_id" = c(1, 2), "custom" = c("A", "B")
#'     ))
#'
#'   x
#'
#'   x |> tidyColumns()
#' }
tidyColumns <- function(result) {
  c(
    "cdm_name", groupColumns(result), strataColumns(result), "variable_name",
    "variable_level", unique(result$estimate_name), additionalColumns(result),
    settingsColumns(result)
  )
}
