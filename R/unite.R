#' Unite one or more columns in name-level format
#'
#' @param x A dataframe.
#' @param cols Columns to aggregate.
#' @param name Column name of the `name` column.
#' @param level Column name of the `level` column.
#' @param keep Whether to keep the original columns.
#' @param ignore Level values to ignore.
#'
#' @return A tibble with the new columns.
#' @description
#' Unites targeted table columns into a pair of name-level columns.
#'
#' @noRd
#'

uniteNameLevelInternal <- function(x,
                                   cols = character(0),
                                   name = "group_name",
                                   level = "group_level",
                                   keep = FALSE,
                                   ignore = c(NA, "overall")) {
  # initial checks
  omopgenerics::assertCharacter(cols)
  omopgenerics::assertCharacter(name, length = 1)
  omopgenerics::assertCharacter(level, length = 1)
  omopgenerics::assertLogical(keep, length = 1)
  omopgenerics::assertCharacter(ignore, na = TRUE)
  omopgenerics::assertTable(x, columns = cols)

  if (name == level) {
    cli::cli_abort("Provide different names for the name and level columns.")
  }

  if("groups" %in% names(attributes(x))) {
    cli::cli_warn("The table will be ungrouped.")
    x <- x |> dplyr::ungroup()
  }

  keyWord <- " &&& "

  if (length(cols) > 0) {
    id <- min(which(colnames(x) %in% cols))

    present <- c(name, level)[c(name, level) %in% colnames(x)]
    if (length(present) > 0) {
      cli::cli_warn(
        "The following columns will be overwritten:
      {paste0(present, collapse = ', ')}."
      )
    }

    containKey <- cols[grepl(keyWord, cols)]
    if (length(containKey) > 0) {
      cli::cli_abort("Column names must not contain '{keyWord}' : `{paste0(containKey, collapse = '`, `')}`")
    }
    containKey <- cols[
      lapply(cols, function(col){any(grepl(keyWord, x[[col]]))}) |> unlist()
    ]
    if (length(containKey) > 0) {
      cli::cli_abort("Column values must not contain '{keyWord}'. Present in: `{paste0(containKey, collapse = '`, `')}`.")
    }

    x <- x |>
      newNameLevel(
        cols = cols, name = name, level = level, ignore = ignore,
        keyWord = keyWord
      )

    if (keep) {
      colskeep <- cols
    } else {
      colskeep <- character()
      x <- x |> dplyr::select(!dplyr::all_of(cols))
    }

    # move cols
    if (id == 1) {
      x <- x |>
        dplyr::relocate(dplyr::all_of(c(colskeep, name, level)))
    } else {
      id <- colnames(x)[id - 1]
      x <- x |>
        dplyr::relocate(
          dplyr::all_of(c(colskeep, name, level)), .after = dplyr::all_of(id)
        )
    }
  } else {
    x <- x |>
      dplyr::mutate(!!name := "overall", !!level := "overall")
  }

  if ("settings" %in% names(attributes(x)) & endsWith(name, "_name") & endsWith(level, "_level")) {
    prefName <- substr(name, 1, nchar(name) - 5)
    prefLevel <- substr(level, 1, nchar(level) - 6)
    if (prefName == prefLevel) {
      attr(x, "settings") <- attr(x, "settings") |>
        dplyr::mutate(!!prefName := paste0(cols, collapse = keyWord))
    }
  }

  return(x)
}

#' Unite one or more columns in group_name-group_level format
#'
#' @param x Tibble or dataframe.
#' @param cols Columns to aggregate.
#' @param keep Whether to keep the original columns.
#' @param ignore Level values to ignore.
#'
#' @return A tibble with the new columns.
#' @description
#' Unites targeted table columns into group_name-group_level columns.
#'
#' @examples
#' x <- dplyr::tibble(
#'   variable = "number subjects",
#'   value = c(10, 15, 40, 78),
#'   sex = c("Male", "Female", "Male", "Female"),
#'   age_group = c("<40", ">40", ">40", "<40")
#' )
#'
#' x |>
#'   uniteGroup(c("sex", "age_group"))
#'
#' @export
#'
uniteGroup <- function(x,
                       cols = character(0),
                       keep = FALSE,
                       ignore = c(NA, "overall")) {
  uniteNameLevelInternal(
    x = x, cols = cols, name = "group_name", level = "group_level", keep = keep,
    ignore = ignore
  )
}

#' Unite one or more columns in strata_name-strata_level format
#'
#' @param x Tibble or dataframe.
#' @param cols Columns to aggregate.
#' @param keep Whether to keep the original columns.
#' @param ignore Level values to ignore.
#'
#' @return A tibble with the new columns.
#' @description
#' Unites targeted table columns into strata_name-strata_level columns.
#'
#' @examples
#' x <- dplyr::tibble(
#'   variable = "number subjects",
#'   value = c(10, 15, 40, 78),
#'   sex = c("Male", "Female", "Male", "Female"),
#'   age_group = c("<40", ">40", ">40", "<40")
#' )
#'
#' x |>
#'   uniteStrata(c("sex", "age_group"))
#'
#' @export
#'
uniteStrata <- function(x,
                        cols = character(0),
                        keep = FALSE,
                        ignore = c(NA, "overall")) {
  uniteNameLevelInternal(
    x = x, cols = cols, name = "strata_name", level = "strata_level",
    keep = keep, ignore = ignore
  )
}

#' Unite one or more columns in additional_name-additional_level format
#'
#' @param x Tibble or dataframe.
#' @param cols Columns to aggregate.
#' @param keep Whether to keep the original columns.
#' @param ignore Level values to ignore.
#'
#' @return A tibble with the new columns.
#' @description
#' Unites targeted table columns into additional_name-additional_level columns.
#'
#' @examples
#' x <- dplyr::tibble(
#'   variable = "number subjects",
#'   value = c(10, 15, 40, 78),
#'   sex = c("Male", "Female", "Male", "Female"),
#'   age_group = c("<40", ">40", ">40", "<40")
#' )
#'
#' x |>
#'   uniteAdditional(c("sex", "age_group"))
#'
#' @export
#'
uniteAdditional <- function(x,
                            cols = character(0),
                            keep = FALSE,
                            ignore = c(NA, "overall")) {
  uniteNameLevelInternal(
    x = x, cols = cols, name = "additional_name", level = "additional_level",
    keep = keep, ignore = ignore
  )
}

## Helpers ---
newNameLevel <- function(x, cols, name, level, ignore, keyWord) {
  y <- x |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::distinct()
  nms <- character(nrow(y))
  lvl <- character(nrow(y))
  for (k in seq_len(nrow(y))) {
    lev <- y[k, ] |> as.matrix() |> as.vector()
    ind <- which(!lev %in% ignore)
    if (length(ind) > 0) {
      nms[k] <- paste0(cols[ind], collapse = keyWord)
      lvl[k] <- paste0(lev[ind], collapse = keyWord)
    } else {
      nms[k] <- "overall"
      lvl[k] <- "overall"
    }
  }
  x <- x |>
    dplyr::inner_join(
      y |>
        dplyr::mutate(!!name := .env$nms, !!level := .env$lvl),
      na_matches = "na",
      by = cols
    )
  return(x)
}
