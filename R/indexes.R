
#' Expected indexes in a cdm object
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param cdm A cdm_reference object.
#' @param name Name(s) of the cdm tables.
#'
#' @return A tibble with 3 columns: `table_class` class of the table,
#' `table_name` name of the table, and `expected_index` index definition.
#' @export
#'
expectedIndexes <- function(cdm, name) {
  UseMethod("expectedIndexes")
}

#' @export
expectedIndexes.cdm_reference <- function(cdm, name = NULL) {
  # validate inputs
  cdm <- validateCdmArgument(cdm = cdm)
  name <- validateName(name = name, cdm = cdm)

  # chnage class
  nc <- class(cdmSource(x = cdm))
  class(cdm) <- nc[nc != "cdm_source"]

  # get expected indexes
  expectedIndexes(cdm = cdm, name = name) |>
    prepareResult(cdm = cdm)
}

#' @export
expectedIndexes.default <- function(cdm, name) {
  dplyr::tibble(
    table_class = character(),
    table_name = character(),
    expected_index = character()
  )
}

#' Existing indexes in a cdm object
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param cdm A cdm_reference object.
#' @param name Name(s) of the cdm tables.
#'
#' @return A tibble with 3 columns: `table_class` class of the table,
#' `table_name` name of the table, and `existing_index` index definition.
#' @export
#'
existingIndexes <- function(cdm, name) {
  UseMethod("existingIndexes")
}

#' @export
existingIndexes.cdm_reference <- function(cdm, name = NULL) {
  # validate inputs
  cdm <- validateCdmArgument(cdm = cdm)
  name <- validateName(name = name, cdm = cdm)

  # change classes
  nc <- class(cdmSource(x = cdm))
  class(cdm) <- nc[nc != "cdm_source"]

  # get existing indexes
  existingIndexes(cdm = cdm, name = name) |>
    prepareResult(cdm = cdm)
}

#' @export
existingIndexes.default <- function(cdm, name) {
  dplyr::tibble(
    table_class = character(),
    table_name = character(),
    existing_index = character()
  )
}

#' Status of the indexes
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param cdm A cdm_reference object.
#' @param name Name(s) of the cdm tables.
#'
#' @return A tibble with 3 columns: `table_class` class of the table,
#' `table_name` name of the table, `index` index definition, and `index_status`
#' status of the index, either: 'missing', 'extra', 'present'.
#' @export
#'
statusIndexes <- function(cdm, name = NULL) {
  # validate inputs
  cdm <- validateCdmArgument(cdm = cdm)
  nms <- validateName(name = name, cdm = cdm)

  # get existing indexes
  existing <- existingIndexes(cdm = cdm, name = name) |>
    dplyr::rename(index = "existing_index") |>
    dplyr::mutate(existing = TRUE)

  # get expected indexes
  expected <- expectedIndexes(cdm = cdm, name = name) |>
    dplyr::rename(index = "expected_index") |>
    dplyr::mutate(expected = TRUE)

  # status
  status <- existing |>
    dplyr::full_join(expected, by = c("table_class", "table_name", "index")) |>
    dplyr::mutate(
      dplyr::across(c("existing", "expected"), \(x) dplyr::coalesce(x, FALSE)),
      index_status = dplyr::case_when(
        .data$existing & .data$expected ~ "present",
        .data$existing ~ "extra",
        .data$expected ~ "missing",
      )
    ) |>
    dplyr::select(!c("existing", "expected")) |>
    prepareResult(cdm = cdm)

  # report status
  reportIndex(x = status, type = "present")
  reportIndex(x = status, type = "extra")
  reportIndex(x = status, type = "missing")

  return(invisible(status))
}

#' Create the missing indexes
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param cdm A cdm_reference object.
#' @param name Name(s) of the cdm tables.
#'
#' @return Whether the process was completed successfully.
#' @export
#'
createIndexes <- function(cdm, name = NULL) {
  # validate inputs
  cdm <- validateCdmArgument(cdm = cdm)
  nms <- validateName(name = name, cdm = cdm)

  # to create
  status <- statusIndexes(cdm = cdm, name = name) |>
    suppressMessages() |>
    dplyr::filter(.data$index_status == "missing")

  # report
  if (nrow(status) == 0) {
    cli::cli_inform(c("v" = "No indexes are missing."))
    result <- TRUE
  } else {
    result <- TRUE
    for (k in seq_len(nrow(status))) {
      nm <- status$table_name[k]
      idx <- status$index[k]
      result <- result & createTableIndex(table = cdm[[nm]], index = idx)
    }
  }

  invisible(result)
}

#' Create a table index
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param table A cdm_table object.
#' @param index Index to be created.
#'
#' @return Whether the index could be created
#' @export
#'
createTableIndex <- function(table, index) {
  UseMethod("createTableIndex")
}

#' @export
createTableIndex.cdm_table <- function(table, index) {
  # initial checks
  table <- validateCdmTable(table = table)
  assertCharacter(index)

  # add classes
  table <- addClass(x = table, value = class(cdmSource(x = table)))

  # create indexes
  res <- logical(length = length(index))
  for (k in seq_along(index)) {
    idx <- index[k]
    cli::cli_progress_message(paste0("Creating index `", idx, "`"))
    t0 <- as.numeric(Sys.time())
    res[k] <- createTableIndex(table = table, index = idx)
    time <- round(as.numeric(Sys.time()) - t0)
    if (res[k]) {
      cli::cli_inform(c(v = "Index `{idx}` created in {time}s."))
    } else {
      cli::cli_inform(c(x = "Index `{idx}` could not be created."))
    }
  }

  return(res)
}

#' @export
createTableIndex.default <- function(table, index) {
  return(FALSE)
}

validateName <- function(name, cdm, call = parent.frame()) {
  if (is.null(name)) {
    name <- names(cdm)
  } else {
    assertCharacter(name)
    name <- unique(name) |>
      purrr::keep(\(x) x %in% names(cdm))
  }
  return(name)
}
prepareResult <- function(x, cdm) {
  # idx column
  idx <- c("expected_index", "existing_index", "index")
  idx <- idx[idx %in% colnames(x)]

  # order result
  x |>
    dplyr::select("table_class", "table_name", dplyr::matches("index")) |>
    dplyr::mutate(id = dplyr::case_when(
      .data$table_class == "omop_table" ~ 1,
      .data$table_class == "achilles_table" ~ 2,
      .data$table_class == "cohort_table" ~ 3,
      .data$table_class == "cdm_table" ~ 4
    )) |>
    dplyr::arrange(.data$id, .data$table_name, .data[[idx]]) |>
    dplyr::select(!"id")
}
reportIndex <- function(x, type) {
  # filter status
  x <- x |>
    dplyr::filter(.data$index_status == .env$type)

  # report
  if (nrow(x) > 0) {
    nm <- switch(type,
                 "missing" = "x",
                 "extra" = "!",
                 "present" = "v")
    "{nrow(x)} '{type}' index{dplyr::if_else(nrow(x) == 1, '', 'es')}." |>
      rlang::set_names(nm = nm) |>
      cli::cli_inform()
  }

  invisible()
}
