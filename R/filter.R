#' Filter a `<summarised_result>` using the settings
#'
#' @inheritParams summarisedResultDoc
#' @param ... Expressions that return a logical value (columns in settings are
#' used to evaluate the expression), and are defined in terms of the variables
#' in .data. If multiple expressions are included, they are combined with the &
#' operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#' @export
#'
#' @return A `<summarised_result>` object with only the result_id rows that
#' satisfy the specified settings.
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

#' Filter a `<summarised_result>` automatically
#'
#' @inheritParams summarisedResultDoc
#' @param ... Expressions that return a logical value. The columns used in the
#' expressions can be stored in the settings, strata, group, or additional
#' columns. `filterResult()` looks for columns using this hierarchy: settings,
#' strata, group, then additional. If multiple expressions are included, they
#' are combined with the & operator. Only rows for which all conditions evaluate
#' to TRUE are kept.
#'
#' @export
#'
#' @return A `<summarised_result>` object with only the rows that satisfy the
#' specified filters.
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
#'   "strata_name" = "sex",
#'   "strata_level" = "Female",
#'   "variable_name" = "number subjects",
#'   "variable_level" = NA_character_,
#'   "estimate_name" = "count",
#'   "estimate_type" = "integer",
#'   "estimate_value" = "100",
#'   "additional_name" = "overall",
#'   "additional_level" = "overall"
#' ) |>
#'   newSummarisedResult(settings = tibble(
#'     "result_id" = 1L, "analysis" = "overall"
#'   ))
#'
#' x |>
#'   filterResult(cohort_name == "my_cohort", sex == "Female")
#'
filterResult <- function(result, ...) {
  assertClass(result, "summarised_result")

  dots <- rlang::enquos(...)
  if (length(dots) == 0) {
    return(result)
  }

  dots <- unlist(
    lapply(dots, filterResultSplitAnd),
    recursive = FALSE
  )
  for (dot in dots) {
    result <- filterResultExpression(result, dot)
  }

  return(result)
}

filterResultSplitAnd <- function(quo) {
  expr <- rlang::get_expr(quo)
  env <- rlang::get_env(quo)

  if (rlang::is_call(expr, "&") && length(rlang::call_args(expr)) == 2) {
    args <- rlang::call_args(expr)
    return(c(
      filterResultSplitAnd(rlang::new_quosure(args[[1]], env)),
      filterResultSplitAnd(rlang::new_quosure(args[[2]], env))
    ))
  }

  list(quo)
}

filterResultExpression <- function(result, quo) {
  where <- filterResultWhere(result, quo)
  filterResultInform(where)

  switch(
    where,
    "settings" = filterResultCall(filterSettings, result, quo),
    "strata" = filterResultCall(filterStrata, result, quo),
    "group" = filterResultCall(filterGroup, result, quo),
    "additional" = filterResultCall(filterAdditional, result, quo),
    "result" = filterResultResult(result, quo),
    "none" = filterResultWarnEmpty(
      paste0(
        "Column(s) not found: ",
        paste0(filterResultVariables(quo), collapse = ", ")
      ),
      settings(result)
    )
  )
}

filterResultInform <- function(where) {
  message <- switch(
    where,
    "settings" = "Filtering using settings.",
    "strata" = "Filtering using strata.",
    "group" = "Filtering using group.",
    "additional" = "Filtering using additional.",
    "result" = "Filtering using result columns.",
    NULL
  )
  if (!is.null(message)) {
    cli::cli_inform(message)
  }
}

filterResultCall <- function(fun, result, quo) {
  env <- rlang::env(
    rlang::get_env(quo),
    .filterFun = fun,
    .filterResult = result
  )
  call <- rlang::call2(
    rlang::sym(".filterFun"),
    rlang::sym(".filterResult"),
    rlang::get_expr(quo)
  )

  rlang::eval_bare(call, env = env)
}

filterResultWhere <- function(result, quo) {
  variables <- filterResultVariables(quo)
  if (length(variables) == 0) {
    return("result")
  }

  set <- settings(result)
  hierarchy <- list(
    "settings" = colnames(set),
    "strata" = filterResultNameLevelColumns(set, "strata"),
    "group" = filterResultNameLevelColumns(set, "group"),
    "additional" = filterResultNameLevelColumns(set, "additional"),
    "result" = colnames(result)
  )

  for (where in names(hierarchy)) {
    if (any(variables %in% hierarchy[[where]])) {
      return(where)
    }
  }

  return("none")
}

filterResultNameLevelColumns <- function(set, column) {
  if (!column %in% colnames(set)) {
    return(character())
  }

  set[[column]] |>
    unique() |>
    getLabels() |>
    purrr::flatten_chr() |>
    unique()
}

filterResultVariables <- function(quo) {
  rlang::get_expr(quo) |>
    all.vars() |>
    setdiff(c(".data", ".env")) |>
    unique()
}

filterResultResult <- function(result, quo) {
  tryCatch(
    expr = {
      result |>
        dplyr::filter(!!quo)
    },
    error = function(e) {
      filterResultWarnEmpty(e, settings(result))
    }
  )
}

filterResultWarnEmpty <- function(e, set) {
  if (inherits(e, "condition")) {
    e <- e$message
  }
  cli::cli_warn(c(
    "!" = "Variable filtering does not exist, returning empty result: ",
    "x" = e
  ))
  emptySummarisedResult(settings = set)
}

#' Filter the strata_name-strata_level pair in a summarised_result
#'
#' @inheritParams summarisedResultDoc
#' @param ... Expressions that return a logical value (`strataColumns()` are
#' used to evaluate the expression), and are defined in terms of the variables
#' in .data. If multiple expressions are included, they are combined with the &
#' operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#' @export
#'
#' @return A `<summarised_result>` object with only the rows that satisfy the
#' specified strata.
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
#' @inheritParams summarisedResultDoc
#' @param ... Expressions that return a logical value (`groupColumns()` are
#' used to evaluate the expression), and are defined in terms of the variables
#' in .data. If multiple expressions are included, they are combined with the &
#' operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#' @export
#'
#' @return A `<summarised_result>` object with only the rows that satisfy the
#' specified group.
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
#' @inheritParams summarisedResultDoc
#' @param ... Expressions that return a logical value (`additionalColumns()` are
#' used to evaluate the expression), and are defined in terms of the variables
#' in .data. If multiple expressions are included, they are combined with the &
#' operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#' @export
#'
#' @return A `<summarised_result>` object with only the rows that satisfy the
#' specified additional columns.
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
