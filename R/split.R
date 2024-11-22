#' Split group_name and group_level columns
#'
#' @param result A dataframe with at least the columns group_name and
#' group_level.
#' @param keep Whether to keep the original group_name and group_level columns.
#' @param fill Optionally, a character that specifies what value should be
#' filled in with when missing.
#'
#' @return A dataframe.
#' @description
#' Pivots the input dataframe so the values of the column group_name are
#' transformed into columns that contain values from the group_level column.
#'
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
#'   x |> splitGroup()
#' }
splitGroup <- function(result,
                       keep = FALSE,
                       fill = "overall") {
  splitNameLevelInternal(
    result = result,
    prefix = "group",
    keep = keep,
    fill = fill
  )
}

#' Split strata_name and strata_level columns
#'
#' @param result A dataframe with at least the columns strata_name and
#' strata_level.
#' @param keep Whether to keep the original group_name and group_level columns.
#' @param fill Optionally, a character that specifies what value should be
#' filled in with when missing.
#'
#' @return A dataframe.
#' @description
#' Pivots the input dataframe so the values of the column strata_name are
#' transformed into columns that contain values from the strata_level column.
#'
#' @export
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
#'   x |> splitStrata()
#' }
#'
splitStrata <- function(result,
                        keep = FALSE,
                        fill = "overall") {
  splitNameLevelInternal(
    result = result,
    prefix = "strata",
    keep = keep,
    fill = fill
  )
}

#' Split additional_name and additional_level columns
#'
#' @param result  A dataframe with at least the columns additional_name and
#' additional_level.
#' @param keep Whether to keep the original group_name and group_level columns.
#' @param fill Optionally, a character that specifies what value should be
#' filled in with when missing.
#'
#' @return A dataframe.
#' @description
#' Pivots the input dataframe so the values of the column additional_name are
#' transformed into columns that contain values from the additional_level column.
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
#'   x |> splitAdditional()
#' }
splitAdditional <- function(result,
                            keep = FALSE,
                            fill = "overall") {
  splitNameLevelInternal(
    result = result,
    prefix = "additional",
    keep = keep,
    fill = fill
  )
}

#' Split all pairs name-level into columns.
#'
#' @param result A data.frame.
#' @param keep Whether to keep the original name-level columns.
#' @param fill A character that specifies what value should be filled in when
#' missing.
#' @param exclude Name of a column pair to exclude.
#'
#' @return A dataframe with group, strata and additional as columns.
#'
#' @description
#' Pivots the input dataframe so any pair name-level columns are transformed
#' into columns (name) that contain values from the corresponding level.
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
#'   x |> splitAll()
#' }
#'
splitAll <- function(result,
                     keep = FALSE,
                     fill = "overall",
                     exclude = "variable") {
  assertTable(result, class = "data.frame")
  assertLogical(keep, length = 1)
  assertCharacter(fill, length = 1)
  assertCharacter(exclude, null = TRUE)

  cols <- colnames(result)
  cols <- intersect(
    cols[stringr::str_ends(cols, "_name")] |>
      stringr::str_replace("_name$", ""),
    cols[stringr::str_ends(cols, "_level")] |>
      stringr::str_replace("_level$", "")
  )
  cols <- cols[!cols %in% exclude]

  for (col in cols) {
    result <- tryCatch(
      expr = {
        result |>
          splitNameLevelInternal(
            result = result,
            prefix = col,
            keep = keep,
            fill = fill
          )
      },
      error = function(e) {
        cli::cli_warn(c(
          "!" = "Couldn't split pair: {.var {col}_name}-{.var {col}_level}: {e}"
        ))
        return(result)
      }
    )
  }

  return(result)
}

splitNameLevelInternal <- function(result,
                                   prefix,
                                   keep,
                                   fill,
                                   call = parent.frame()) {
  assertCharacter(prefix, length = 1, call = call)
  assertLogical(keep, length = 1, call = call)
  assertTable(
    result,
    columns = paste0(prefix, c("_name", "_level")), call = call
  )
  assertCharacter(fill, length = 1, na = TRUE, call = call)

  newCols <- nameLevelColumns(result, prefix)

  name <- paste0(prefix, "_name")
  level <- paste0(prefix, "_level")
  splitResult <- result |>
    dplyr::select(dplyr::all_of(c(name, level))) |>
    dplyr::distinct()

  nameValues <- splitResult[[name]] |>
    purrr::map(getLabels)
  levelValues <- splitResult[[level]] |>
    purrr::map(getLabels)
  if (!all(lengths(nameValues) == lengths(levelValues))) {
    cli::cli_abort("Column names and levels number does not match")
  }

  present <- newCols[newCols %in% colnames(result)]
  if (length(present) > 0) {
    "The following columns will be overwritten: {.var {present}}." |>
      cli::cli_warn()
    result <- result |>
      dplyr::select(!dplyr::all_of(present))
  }

  # create new columns
  newColumns <- newCols |>
    rlang::set_names() |>
    purrr::map(\(x) {
      purrr::map2_chr(nameValues, levelValues, \(nv, lv) {
        id <- which(nv == x)
        if (length(id) != 1) {
          NA_character_
        } else {
          lv[id]
        }
      })
    })
  splitResult <- splitResult |>
    dplyr::mutate(!!!newColumns)

  # add new columns back
  result <- result |>
    dplyr::left_join(splitResult, by = c(name, level)) |>
    dplyr::relocate(dplyr::all_of(newCols), .after = dplyr::all_of(level))

  if (!keep) {
    result <- result |>
      dplyr::select(-dplyr::all_of(c(name, level)))
  }

  # use fill
  if (!is.na(fill)) {
    result <- result |>
      dplyr::mutate(dplyr::across(
        dplyr::any_of(newCols), \(x) dplyr::coalesce(x, fill)
      ))
  }

  return(result)
}
