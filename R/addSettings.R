#' Add settings columns to a `<summarised_result>` object
#'
#' @param result A `<summarised_result>` object.
#' @param settingsColumn Settings to be added as columns, by default
#' `settingsColumns(result)` will be added. If NULL or empty character vector,
#' no settings will be added.
#'
#' @export
#'
#' @return A `<summarised_result>` object with the added setting columns.
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
#'   x |> addSettings()
#' }
#'
addSettings <- function(result,
                        settingsColumn = settingsColumns(result)) {
  # checks
  if (is.null(attr(result, "settings"))) {
    cli::cli_abort("result doesn't have a `settings` attribute")
  }
  settingsColumn <- checkSettingsColumns(settingsColumn, result)
  set <- settings(result)

  if (length(settingsColumn) == 0) {
    return(result)
  }

  # add settings
  toJoin <- settingsColumn[settingsColumn %in% colnames(result)]
  result <- result |>
    dplyr::left_join(
      set |>
        dplyr::select(dplyr::any_of(c("result_id", settingsColumn))),
      by = c("result_id", toJoin)
    )

  return(result)
}

checkSettingsColumns <- function(settingsColumns, result, call = parent.frame()) {
  set <- settings(result)
  assertCharacter(x = settingsColumns, null = TRUE, call = call)
  if (!is.null(settingsColumns)) {
    assertTable(set, columns = settingsColumns)
    settingsColumns <- settingsColumns[settingsColumns != "result_id"]
    notPresent <- settingsColumns[!settingsColumns %in% colnames(set)]
    if (length(notPresent) > 0) {
      cli::cli_abort("The following `settings` are not present in settings attribute: {.var {notPresent}}.", call = call)
    }
  } else {
    settingsColumns <- character()
  }
  return(invisible(settingsColumns))
}
