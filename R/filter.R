#' Filter a `<summarised_result>` using the settings
#'
#' @param result A `<summarised_result>` object.
#' @param ... Expressions that return a logical value (columns in settings are
#' used to evaluate the expression), and are defined in terms of the variables
#' in .data. If multiple expressions are included, they are combined with the &
#' operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#' @export
#'
#' @return A `<summarised_result>` object with only the result_id rows that fulfill
#' the required specified settings.
#'
#' @examples
#' library(dplyr)
#' library(omopgenerics)
#'
#' x <- tibble(
#'   "result_id" = as.integer(c(1, 2)),
#'   "cdm_name" = c("cprd", "eunomia"),
#'   "group_name" = "cohort_name",
#'   "group_level" = "my_cohort",
#'   "strata_name" = "sex",
#'   "strata_level" = "male",
#'   "variable_name" = "Age group",
#'   "variable_level" = "10 to 50",
#'   "estimate_name" = "count",
#'   "estimate_type" = "numeric",
#'   "estimate_value" = "5",
#'   "additional_name" = "overall",
#'   "additional_level" = "overall"
#' ) |>
#'   newSummarisedResult(settings = tibble(
#'     "result_id" = c(1, 2), "custom" = c("A", "B")
#'   ))
#'
#' x
#'
#' x |> filterSettings(custom == "A")
#'
filterSettings <- function(result, ...) {
  set <- settings(result)

  # filter settings (try if error)
  result <- tryCatch(
    {
      set <- set |>
        dplyr::filter(...)

      if (nrow(set) == 0) {
        emptySummarisedResult()
      } else {
        colsRemove <- set |>
          purrr::keep(\(x) all(is.na(x))) |>
          names()
        set <- set |>
          dplyr::select(!dplyr::all_of(colsRemove))
        attr(result, "settings") <- set

        # filter id from settings
        result |>
          dplyr::filter(.data$result_id %in% .env$set$result_id)
      }
    },
    error = function(e) {
      cli::cli_warn(c(
        "!" = "Variable filtering does not exist, returning empty result: ",
        e$message
      ))
      emptySummarisedResult() # return empty result here
    }
  )

  return(result)
}

#' Filter the strata_name-strata_level pair in a summarised_result
#'
#' @param result A `<summarised_result>` object.
#' @param ... Expressions that return a logical value (`strataColumns()` are
#' used to evaluate the expression), and are defined in terms of the variables
#' in .data. If multiple expressions are included, they are combined with the &
#' operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#' @export
#'
#' @return A `<summarised_result>` object with only the rows that fulfill the
#' required specified strata.
#'
#' @examples
#' library(dplyr)
#' library(omopgenerics)
#'
#' x <- tibble(
#'   "result_id" = 1L,
#'   "cdm_name" = "eunomia",
#'   "group_name" = "cohort_name",
#'   "group_level" = "my_cohort",
#'   "strata_name" = c("sex", "sex &&& age_group", "sex &&& year"),
#'   "strata_level" = c("Female", "Male &&& <40", "Female &&& 2010"),
#'   "variable_name" = "number subjects",
#'   "variable_level" = NA_character_,
#'   "estimate_name" = "count",
#'   "estimate_type" = "integer",
#'   "estimate_value" = c("100", "44", "14"),
#'   "additional_name" = "overall",
#'   "additional_level" = "overall"
#' ) |>
#'   newSummarisedResult()
#'
#' x |>
#'   filterStrata(sex == "Female")
#'
filterStrata <- function(result, ...) {
  filterNameLevel(result, "strata", ...)
}

#' Filter the group_name-group_level pair in a summarised_result
#'
#' @param result A `<summarised_result>` object.
#' @param ... Expressions that return a logical value (`groupColumns()` are
#' used to evaluate the expression), and are defined in terms of the variables
#' in .data. If multiple expressions are included, they are combined with the &
#' operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#' @export
#'
#' @return A `<summarised_result>` object with only the rows that fulfill the
#' required specified group.
#'
#' @examples
#' library(dplyr)
#' library(omopgenerics)
#'
#' x <- tibble(
#'   "result_id" = 1L,
#'   "cdm_name" = "eunomia",
#'   "group_name" = c("cohort_name", "age_group &&& cohort_name", "age_group"),
#'   "group_level" = c("my_cohort", ">40 &&& second_cohort", "<40"),
#'   "strata_name" = "sex",
#'   "strata_level" = "Female",
#'   "variable_name" = "number subjects",
#'   "variable_level" = NA_character_,
#'   "estimate_name" = "count",
#'   "estimate_type" = "integer",
#'   "estimate_value" = c("100", "44", "14"),
#'   "additional_name" = "overall",
#'   "additional_level" = "overall"
#' ) |>
#'   newSummarisedResult()
#'
#' x |>
#'   filterGroup(cohort_name == "second_cohort")
#'
filterGroup <- function(result, ...) {
  filterNameLevel(result, "group", ...)
}

#' Filter the additional_name-additional_level pair in a summarised_result
#'
#' @param result A `<summarised_result>` object.
#' @param ... Expressions that return a logical value (`additionalColumns()` are
#' used to evaluate the expression), and are defined in terms of the variables
#' in .data. If multiple expressions are included, they are combined with the &
#' operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#' @export
#'
#' @return A `<summarised_result>` object with only the rows that fulfill the
#' required specified additional.
#'
#' @examples
#' library(dplyr)
#' library(omopgenerics)
#'
#' x <- tibble(
#'   "result_id" = 1L,
#'   "cdm_name" = "eunomia",
#'   "group_name" = "cohort_name",
#'   "group_level" = c("cohort1", "cohort2", "cohort3"),
#'   "strata_name" = "sex",
#'   "strata_level" = "Female",
#'   "variable_name" = "number subjects",
#'   "variable_level" = NA_character_,
#'   "estimate_name" = "count",
#'   "estimate_type" = "integer",
#'   "estimate_value" = c("100", "44", "14"),
#'   "additional_name" = c("year", "time_step", "year &&& time_step"),
#'   "additional_level" = c("2010", "4", "2015 &&& 5")
#' ) |>
#'   newSummarisedResult()
#'
#' x |>
#'   filterAdditional(year == "2010")
#'
filterAdditional <- function(result, ...) {
  filterNameLevel(result, "additional", ...)
}

filterNameLevel <- function(result, prefix, ..., call = parent.frame()) {
  # initial checks
  cols <- paste0(prefix, c("_name", "_level"))
  assertTable(result, columns = cols, call = call)

  if ("result_id" %in% colnames(result)) {
    cols <- c("result_id", cols)
  }

  # splitNameLevelInternal
  labs <- result |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::distinct() |>
    splitNameLevelInternal(prefix = prefix, keep = TRUE, fill = "overall")

  # filter
  tryCatch(
    expr = {
      result |>
        dplyr::inner_join(
          labs |>
            dplyr::filter(...) |>
            dplyr::select(dplyr::all_of(cols)),
          by = cols
        )
    },
    error = function(e) {
      cli::cli_warn(c(
        "!" = "Variable filtering does not exist, returning empty result: ",
        e$message
      ))
      emptySummarisedResult(settings = settings(result))
    }
  )
}
