#' Set estimates as columns
#'
#' @param result A `<summarised_result>`.
#' @param pivotEstimatesBy Names from which pivot wider the estimate values. If
#' NULL the table will not be pivotted.
#' @param nameStyle Name style (glue package specifications) to customise names
#' when pivotting estimates. If NULL standard tidyr::pivot_wider formatting will
#' be used.
#'
#' @return A tibble.
#'
#' @description
#' Pivot the estimates as new columns in result table.
#'
#' @export
#'
#' @examples {
#'   library(dplyr)
#'   library(omopgenerics)
#'
#'   x <- tibble(
#'     "result_id" = 1L,
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
#'     newSummarisedResult()
#'
#'   x |>
#'     pivotEstimates()
#' }
#'
pivotEstimates <- function(result,
                           pivotEstimatesBy = "estimate_name",
                           nameStyle = NULL) {
  # initial checks
  pivotEstimatesBy <- checkPivotEstimatesBy(pivotEstimatesBy = pivotEstimatesBy)
  assertCharacter(nameStyle, null = TRUE, length = 1)
  assertTable(result, columns = pivotEstimatesBy)

  # pivot estimates
  result_out <- result
  if (length(pivotEstimatesBy) > 0) {
    if (is.null(nameStyle)) {
      nameStyle <- paste0("{", paste0(pivotEstimatesBy, collapse = "}_{"), "}")
    }
    if (grepl("__", nameStyle)) {
      cli::cli_warn(c("!" = "Double underscores found in 'nameStyle'. Converting to a single underscore."))
    }
    typeNameConvert <- result |>
      dplyr::distinct(dplyr::across(dplyr::all_of(c("estimate_type", pivotEstimatesBy)))) |>
      dplyr::mutate(
        estimate_type = dplyr::case_when(
          grepl("percentage|proportion", .data$estimate_type) ~ "numeric",
          "date" == .data$estimate_type ~ "Date",
          .default = .data$estimate_type
        ),
        new_name = glue::glue(nameStyle, .na = "") |>
          stringr::str_replace_all("_+", "_") |> # remove multiple _
          stringr::str_replace_all("^_|_$", "") # remove leading/trailing _
      )

    # to cast columns
    q <- purrr::map_chr(seq_len(nrow(typeNameConvert)), \(k) {
      type <- typeNameConvert$estimate_type[k]
      col <- typeNameConvert$new_name[k]
      paste0("suppressWarnings(as.", type, "(.data[['", col, "']]))")
    }) |>
      rlang::parse_exprs() |>
      rlang::set_names(typeNameConvert$new_name)

    result_out <- result |>
      dplyr::select(-"estimate_type") |>
      tidyr::pivot_wider(
        names_from = dplyr::all_of(pivotEstimatesBy),
        values_from = "estimate_value",
        names_glue = nameStyle
      ) |>
      dplyr::rename_with(~ stringr::str_remove_all(., "_NA|NA_")) |>
      dplyr::mutate(!!!q)
  }
  return(result_out)
}

checkPivotEstimatesBy <- function(pivotEstimatesBy, call = parent.frame()) {
  assertCharacter(x = pivotEstimatesBy, null = TRUE, call = call)
  notValid <- any(c(
    !pivotEstimatesBy %in% resultColumns(),
    c("estimate_type", "estimate_value") %in% pivotEstimatesBy
  ))
  if (isTRUE(notValid)) {
    cli::cli_abort(
      c("x" = "`pivotEstimatesBy` must refer to <summarised_result> columns.
        It cannot include `estimate_value` and `estimate_type`."),
      call = call
    )
  }
  return(invisible(pivotEstimatesBy))
}
