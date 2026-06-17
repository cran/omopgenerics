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

#' Assert that an object is a character and satisfies certain conditions.
#'
#' @inheritParams assertDoc
#' @param minNumCharacter Minimum number of characters that all elements must
#' have.
#'
#' @export
#'
assertCharacter <- function(x,
                            length = NULL,
                            na = FALSE,
                            null = FALSE,
                            empty = TRUE,
                            unique = FALSE,
                            named = FALSE,
                            minNumCharacter = 0,
                            nm = deparse1(substitute(x), backtick = TRUE),
                            call = parent.frame(),
                            msg = NULL) {
  # perform checks
  report <- createCheckObject(x) |>
    checkNull(null) |>
    checkFunction(is.character, "is not character") |>
    checkEmpty(empty) |>
    checkLength(length) |>
    checkNa(na) |>
    checkNamed(named) |>
    checkUnique(unique) |>
    checkMinCharacter(minNumCharacter)

  # return if no error
  if (is.null(report$error)) {
    return(invisible(report$value))
  }

  # report error message
  errorMessage(
    nm = nm,
    report = report,
    msg = msg,
    call = call,
    object = "a character",
    length = length,
    na = na,
    null = null,
    empty = empty,
    unique = unique,
    named = named,
    minNumCharacter = minNumCharacter
  )
}

#' Assert that an object is one of a set of options.
#'
#' @inheritParams assertDoc
#' @param choices Options that x is allowed to be.
#'
#' @export
#'
assertChoice <- function(x,
                         choices,
                         length = NULL,
                         na = FALSE,
                         null = FALSE,
                         empty = TRUE,
                         unique = FALSE,
                         named = FALSE,
                         nm = deparse1(substitute(x), backtick = TRUE),
                         call = parent.frame(),
                         msg = NULL) {
  # perform checks
  report <- createCheckObject(x) |>
    checkNull(null) |>
    checkEmpty(empty) |>
    checkLength(length) |>
    checkNa(na) |>
    checkNamed(named) |>
    checkUnique(unique) |>
    checkChoices(choices)

  # return if no error
  if (is.null(report$error)) {
    return(invisible(report$value))
  }

  # report error message
  errorMessage(
    nm = nm,
    report = report,
    msg = msg,
    call = call,
    object = "a choice between: {choices}" |>
      cli::cli_text() |>
      cli::cli_fmt() |>
      paste0(collapse = " "),
    length = length,
    na = na,
    null = null,
    empty = empty,
    unique = unique,
    named = named
  )
}

#' Assert that an object has a certain class.
#'
#' @inheritParams assertDoc
#' @param class Expected class or classes.
#' @param all Whether it should have all the classes or only at least one of
#' them.
#' @param extra Whether the object can have extra classes.
#'
#' @export
#'
assertClass <- function(x,
                        class,
                        length = NULL,
                        null = FALSE,
                        empty = TRUE,
                        all = FALSE,
                        extra = TRUE,
                        nm = deparse1(substitute(x), backtick = TRUE),
                        call = parent.frame(),
                        msg = NULL) {
  # perform checks
  report <- createCheckObject(x) |>
    checkNull(null) |>
    checkLength(length) |>
    checkClass(class = class, all = all, extra = extra) |>
    checkEmpty(empty)

  # return if no error
  if (is.null(report$error)) {
    return(invisible(report$value))
  }

  # report error message
  if (all) {
    obj <- "an object with class: {.cls {class}}"
  } else {
    obj <- "an object with at least one of these classes: {.cls {class}}"
  }
  obj <- obj |>
    paste0("; it can {ifelse(extra, '', 'not ')}contain extra classes") |>
    cli::cli_text() |>
    cli::cli_fmt() |>
    paste0(collapse = " ") |>
    as.character()
  errorMessage(
    nm = nm,
    report = report,
    msg = msg,
    call = call,
    object = obj,
    length = length,
    null = null,
    empty = empty
  )
}

#' Assert that an object is a list.
#'
#' @inheritParams assertDoc
#' @param class Class that the elements must have.
#'
#' @export
#'
assertList <- function(x,
                       length = NULL,
                       na = FALSE,
                       null = FALSE,
                       empty = TRUE,
                       unique = FALSE,
                       named = FALSE,
                       class = NULL,
                       nm = deparse1(substitute(x), backtick = TRUE),
                       call = parent.frame(),
                       msg = NULL) {
  # perform checks
  report <- createCheckObject(x) |>
    checkNull(null) |>
    checkFunction(is.list, "is not a list") |>
    checkEmpty(empty) |>
    checkLength(length) |>
    checkNa(na) |>
    checkUnique(unique) |>
    checkNamed(named) |>
    checkListClass(class)

  # return if no error
  if (is.null(report$error)) {
    return(invisible(report$value))
  }

  # report error message
  if (!is.null(class)) {
    obj <- "a list with objects of class {class}" |>
      cli::cli_text() |>
      cli::cli_fmt() |>
      paste0(collapse = " ")
  } else {
    obj <- "a list"
  }
  errorMessage(
    nm = nm,
    report = report,
    msg = msg,
    call = call,
    object = obj,
    length = length,
    unique = unique,
    na = na,
    null = null,
    empty = empty,
    named = named
  )
}

#' Assert that an object is a logical.
#'
#' @inheritParams assertDoc
#'
#' @export
#'
assertLogical <- function(x,
                          length = NULL,
                          na = FALSE,
                          null = FALSE,
                          empty = TRUE,
                          unique = FALSE,
                          named = FALSE,
                          nm = deparse1(substitute(x), backtick = TRUE),
                          call = parent.frame(),
                          msg = NULL) {
  # perform checks
  report <- createCheckObject(x) |>
    checkNull(null) |>
    checkFunction(is.logical, "is not logical") |>
    checkEmpty(empty) |>
    checkLength(length) |>
    checkNa(na) |>
    checkUnique(unique) |>
    checkNamed(named)

  # return if no error
  if (is.null(report$error)) {
    return(invisible(report$value))
  }

  # report error message
  errorMessage(
    nm = nm,
    report = report,
    msg = msg,
    call = call,
    object = "a logical",
    length = length,
    na = na,
    null = null,
    empty = empty,
    unique = unique,
    named = named
  )
}

#' Assert that an object is a numeric.
#'
#' @inheritParams assertDoc
#' @param integerish Whether it has to be an integer
#' @param min Minimum value that the object can be.
#' @param max Maximum value that the object can be.
#'
#' @export
#'
assertNumeric <- function(x,
                          integerish = FALSE,
                          min = -Inf,
                          max = Inf,
                          length = NULL,
                          na = FALSE,
                          null = FALSE,
                          empty = TRUE,
                          unique = FALSE,
                          named = FALSE,
                          nm = deparse1(substitute(x), backtick = TRUE),
                          call = parent.frame(),
                          msg = NULL) {
  # perform checks
  report <- createCheckObject(x) |>
    checkNull(null) |>
    appendNoNa() |>
    checkFunction(is.numeric, "is not numeric") |>
    checkEmpty(empty) |>
    checkIntegerish(integerish) |>
    checkMin(min) |>
    checkMax(max) |>
    checkLength(length) |>
    checkNa(na) |>
    checkUnique(unique) |>
    checkNamed(named)

  # return if no error
  if (is.null(report$error)) {
    return(invisible(report$value))
  }

  # report error message
  errorMessage(
    nm = nm,
    report = report,
    msg = msg,
    call = call,
    object = ifelse(integerish, "an integerish numeric", "a numeric"),
    min = min,
    max = max,
    length = length,
    na = na,
    null = null,
    empty = empty,
    unique = unique,
    named = named
  )
}

#' Assert that an object is a table.
#'
#' @inheritParams assertDoc
#' @param class A class that the table must have: "tbl", "data.frame", "tbl_sql",
#' ...
#' @param numberColumns Number of columns that it has to contain.
#' @param numberRows Number of rows that it has to contain.
#' @param columns Name of the columns required.
#' @param allowExtraColumns Whether extra columns are allowed.
#' @param unique Whether it has to contain unique rows.
#'
#' @export
#'
assertTable <- function(x,
                        class = NULL, # "data.frame" can not be added currently due to PatientProfiles
                        numberColumns = NULL,
                        numberRows = NULL,
                        columns = character(),
                        allowExtraColumns = TRUE,
                        null = FALSE,
                        empty = TRUE,
                        unique = FALSE,
                        nm = deparse1(substitute(x), backtick = TRUE),
                        call = parent.frame(),
                        msg = NULL) {
  # perform checks
  report <- createCheckObject(x) |>
    checkNull(null) |>
    checkClass(class) |>
    checkEmpty(empty, table = TRUE, skip = FALSE) |>
    checkNumberColumns(numberColumns) |>
    checkNumberRows(numberRows) |>
    checkColumns2(columns, allowExtraColumns) |>
    checkDistinct(unique)

  # return if no error
  if (is.null(report$error)) {
    return(invisible(report$value))
  }

  # report error
  object <- if (length(class) == 0) {
    "a table"
  } else {
    "a table of class: {.cls {class}}" |>
      cli::cli_text() |>
      cli::cli_fmt() |>
      paste0(collapse = " ")
  }
  errorMessage(
    nm = nm,
    report = report,
    msg = msg,
    call = call,
    object = object,
    numberColumns = numberColumns,
    numberRows = numberRows,
    columns = columns,
    allowExtraColumns = allowExtraColumns,
    null = null,
    empty = empty,
    distinct = unique
  )

  return(invisible(x))
}

#' Assert that an expression is TRUE.
#'
#' @param x Expression to check.
#' @inheritParams assertDoc
#'
#' @export
#'
assertTrue <- function(x,
                       null = FALSE,
                       empty = TRUE,
                       nm = deparse1(substitute(x), backtick = TRUE),
                       call = parent.frame(),
                       msg = NULL) {
  # perform checks
  report <- createCheckObject(x) |>
    checkNull(null) |>
    checkEmpty(empty) |>
    checkTrue(empty)

  # return if no error
  if (is.null(report$error)) {
    return(invisible(report$value))
  }

  # report error
  errorMessage(
    nm = nm,
    report = report,
    msg = msg,
    call = call,
    object = "TRUE",
    null = null,
    empty = empty
  )
}

#' Assert Date
#'
#' @param x Expression to check.
#' @inheritParams assertDoc
#'
#' @return x
#' @export
#'
assertDate <- function(x,
                       length = NULL,
                       na = FALSE,
                       null = FALSE,
                       empty = TRUE,
                       unique = FALSE,
                       named = FALSE,
                       nm = deparse1(substitute(x), backtick = TRUE),
                       call = parent.frame(),
                       msg = NULL) {
  # perform checks
  report <- createCheckObject(x) |>
    checkNull(null) |>
    checkClass("Date") |>
    checkEmpty(empty) |>
    checkLength(length) |>
    checkNa(na) |>
    checkUnique(unique) |>
    checkNamed(named)

  # return if no error
  if (is.null(report$error)) {
    return(invisible(report$value))
  }

  # report error
  errorMessage(
    nm = nm,
    report = report,
    msg = msg,
    call = call,
    object = "a date vector",
    length = length,
    na = na,
    null = null,
    empty = empty,
    unique = unique,
    named = named
  )
}

createCheckObject <- function(x) {
  list(value = x, continue = TRUE, error = NULL)
}
appendNoNa <- function(x) {
  x$value_no_na <- x$value[!is.na(x$value)]
  x$present_no_na <- length(x$value_no_na) > 0
  return(x)
}
checkNull <- function(x, null) {
  if (is.null(x$value)) {
    x$continue <- FALSE
    if (!null) {
      x$error <- "can not be NULL"
    }
  }
  return(x)
}
checkEmpty <- function(x, empty, table = FALSE, skip = TRUE) {
  if (!x$continue) {
    return(x)
  }
  if (objectLength(x$value, table = table) == 0) {
    x$continue <- FALSE
    if (!empty) {
      x$error <- "can not be empty"
    } else if (!skip) {
      x$continue <- TRUE
    }
  }
  return(x)
}
objectLength <- function(x, table = FALSE) {
  if (table) {
    rows <- tryCatch(nrow(x), error = function(e) NULL)
    if (!is.null(rows) && length(rows) == 1 && !is.na(rows)) {
      return(rows)
    }
  }
  length(x)
}
checkLength <- function(x, length) {
  if (!x$continue) {
    return(x)
  }
  len <- base::length(x$value)
  if (!is.null(length) && len != length) {
    x$continue <- FALSE
    x$error <- paste0("has length ", len, " but must have length {length}")
  }
  return(x)
}
checkNa <- function(x, na) {
  if (!x$continue) {
    return(x)
  }
  if (!na && length(x$value) > 0) {
    pos <- which(is.na(x$value))
    if (length(pos) > 0) {
      x$continue <- FALSE
      x$error <- "contains NA in position {pos}" |>
        cli::cli_text() |>
        cli::cli_fmt()
    }
  }
  return(x)
}
checkNamed <- function(x, named) {
  if (!x$continue) {
    return(x)
  }
  if (named && length(names(x$value)[names(x$value) != ""]) != length(x$value)) {
    x$continue <- FALSE
    x$error <- "must be named"
  }
  return(x)
}
checkUnique <- function(x, unique) {
  if (!x$continue) {
    return(x)
  }
  if (unique && length(unique(x$value)) != length(x$value)) {
    x$continue <- FALSE
    x$error <- "must be unique"
  }
  return(x)
}
checkClass <- function(x, class, all = FALSE, extra = TRUE) {
  if (!x$continue) {
    return(x)
  }
  if (length(class) == 0) {
    return(x)
  }
  cl <- base::class(x$value)
  common <- intersect(class, cl)
  if (all) {
    if (!identical(class, common)) {
      x$continue <- FALSE
      x$error <- "must have class {.cls {class}} but has class {.cls {cl}}" |>
        cli::cli_text() |>
        cli::cli_fmt()
    }
  } else {
    if (length(common) == 0) {
      x$continue <- FALSE
      x$error <- "must have class {.var {collapseStr(class, 'or')}} but has class {.cls {cl}}" |>
        cli::cli_text() |>
        cli::cli_fmt()
    }
  }
  if (x$continue & !extra) {
    extra <- setdiff(cl, class)
    if (length(extra) > 0) {
      x$continue <- FALSE
      x$error <- "must not have classes outside {.cls {class}} but has class {.cls {cl}}" |>
        cli::cli_text() |>
        cli::cli_fmt()
    }
  }
  return(x)
}
checkInherits <- function(x, what) {
  if (!x$continue) {
    return(x)
  }
  if (!inherits(x$value, what)) {
    x$continue <- FALSE
    x$error <- "must inherit from {.var {what}}" |>
      cli::cli_text() |>
      cli::cli_fmt()
  }
  return(x)
}
checkFunction <- function(x, fun, message) {
  if (!x$continue) {
    return(x)
  }
  if (!do.call(fun, list(x$value))) {
    x$continue <- FALSE
    x$error <- message
  }
  return(x)
}
checkNumeric <- function(x) {
  if (!x$continue) {
    return(x)
  }
  if (!is.numeric(x$value)) {
    x$continue <- FALSE
    x$error <- "is not numeric"
  }
  return(x)
}
checkTrue <- function(x, empty) {
  if (!x$continue) {
    return(x)
  }
  if (empty && length(x$value) == 0) {
    return(x)
  }
  if (!isTRUE(x$value)) {
    x$continue <- FALSE
    x$error <- "is not TRUE"
  }
  return(x)
}
checkIntegerish <- function(x, integerish) {
  if (!x$continue) {
    return(x)
  }
  if (integerish & x$present_no_na & !all(is.infinite(x$value_no_na))) {
    if (inherits(x$value, "integer")) {
      return(x)
    }
    xInt <- x$value_no_na[!is.infinite(x$value_no_na)]
    err <- max(abs(xInt - round(xInt)))
    if (err > 0.0001) {
      x$continue <- FALSE
      x$error <- "is not integerish"
    }
  }
  return(x)
}
checkMin <- function(x, min) {
  if (!x$continue) {
    return(x)
  }
  if (!is.infinite(min) & x$present_no_na) {
    if (base::min(x$value_no_na) < min) {
      x$continue <- FALSE
      x$error <- paste0("is not bigger or equal to ", min)
    }
  }
  return(x)
}
checkMax <- function(x, max) {
  if (!x$continue) {
    return(x)
  }
  if (!is.infinite(max) & x$present_no_na) {
    if (base::max(x$value_no_na) > max) {
      x$continue <- FALSE
      x$error <- paste0("is not smaller or equal to ", max)
    }
  }
  return(x)
}
checkNumberColumns <- function(x, numberColumns) {
  if (!x$continue) {
    return(x)
  }
  if (!is.null(numberColumns)) {
    if (ncol(x$value) != numberColumns) {
      x$continue <- FALSE
      x$error <- paste(
        "must have", numberColumns, "columns, but has", ncol(x$value)
      )
    }
  }
  return(x)
}
checkNumberRows <- function(x, numberRows) {
  if (!x$continue) {
    return(x)
  }
  if (!is.null(numberRows)) {
    if (nrow(x$value) != numberRows) {
      x$continue <- FALSE
      x$error <- paste(
        "must have", numberRows, "rows, but has", nrow(x$value)
      )
    }
  }
  return(x)
}
checkColumns2 <- function(x, columns, allowExtraColumns) {
  if (!x$continue) {
    return(x)
  }
  if (!is.null(columns)) {
    cols <- colnames(x$value)
    notPresent <- setdiff(columns, cols)
    if (length(notPresent) > 0) {
      x$continue <- FALSE
      x$error <- "must have {notPresent} as columns" |>
        cli::cli_text() |>
        cli::cli_fmt() |>
        paste0(collapse = " ") |>
        as.character()
    }
    if (!allowExtraColumns & x$continue) {
      extraCols <- setdiff(cols, columns)
      if (length(extraCols) > 0) {
        x$continue <- FALSE
        x$error <- "must not have the following {extraCols} extra column{?s}" |>
          cli::cli_text() |>
          cli::cli_fmt() |>
          paste0(collapse = " ") |>
          as.character()
      }
    }
  }
  return(x)
}
checkDistinct <- function(x, unique) {
  if (!x$continue) {
    return(x)
  }
  if (unique) {
    if (nrow(x$value) != x$value |>
      dplyr::distinct() |>
      nrow()) {
      x$continue <- FALSE
      x$error <- "must contain unique rows"
    }
  }
  return(x)
}
checkListClass <- function(x, class) {
  if (!x$continue) {
    return(x)
  }
  if (!is.null(class)) {
    x <- appendNoNa(x)
    pos <- purrr::map_lgl(x$value_no_na, \(x) !any(class %in% base::class(x))) |>
      which()
    if (length(pos) > 0) {
      x$continue <- FALSE
      x$error <- "elements in position {pos} do not have class {.cls {class}}" |>
        cli::cli_text() |>
        cli::cli_fmt() |>
        paste0(collapse = " ")
    }
  }
  return(x)
}
checkMinCharacter <- function(x, minCharacter) {
  if (!x$continue) {
    return(x)
  }
  if (minCharacter > 0) {
    x <- appendNoNa(x)
    if (x$present_no_na) {
      if (base::min(nchar(x$value_no_na)) < minCharacter) {
        x$continue <- FALSE
        x$error <- paste("must have at least", minCharacter, "characters")
      }
    }
  }
  return(x)
}
checkChoices <- function(x, choices) {
  if (!x$continue) {
    return(x)
  }
  x <- appendNoNa(x)
  if (x$present_no_na) {
    if (!all(x$value_no_na %in% choices)) {
      x$continue <- FALSE
      x$error <- "a choice between: {choices}" |>
        cli::cli_text() |>
        cli::cli_fmt() |>
        paste0(collapse = " ")
    }
  }
  return(x)
}
collapseStr <- function(x, sep) {
  if (length(x) == 1) {
    return(x)
  }
  len <- length(x)
  paste0(paste0(x[-len], collapse = ", "), " ", sep, " ", x[len])
}
errorMessage <- function(nm,
                         report,
                         msg,
                         call,
                         object,
                         length = NULL,
                         na = NULL,
                         named = NULL,
                         unique = NULL,
                         null = NULL,
                         empty = NULL,
                         min = -Inf,
                         max = Inf,
                         minNumCharacter = 0,
                         numberColumns = NULL,
                         numberRows = NULL,
                         columns = NULL,
                         allowExtraColumns = NULL,
                         distinct = NULL) {
  if (is.null(msg)) {
    msg <- paste0(
      c(
        "`{nm}` must be {object}",
        if (!is.null(length)) "with length = {length}",
        if (isFALSE(na)) "it can not contain NA",
        if (isTRUE(named)) "it has to be named",
        if (isTRUE(unique)) "it has to contain unique elements",
        if (isFALSE(null)) "it can not be NULL",
        if (isFALSE(empty)) "it can not be empty",
        if (!is.infinite(min)) "greater or equal to {min}",
        if (!is.infinite(max)) "smaller or equal to {max}",
        if (minNumCharacter > 0) "with at least {minNumCharacter} character per element",
        if (is.numeric(numberColumns)) "with exactly {numberColumns} columns",
        if (is.numeric(numberRows)) "with exactly {numberRows} rows",
        if (is.character(columns) && length(columns) > 0) "must contain {columns} as columns",
        if (isFALSE(allowExtraColumns)) "no extra columns are allowed",
        if (isTRUE(distinct)) "rows must be unique"
      ),
      collapse = "; "
    ) |>
      cli::cli_text() |>
      cli::cli_fmt() |>
      paste0(collapse = " ") |>
      paste0(".") |>
      as.character()
  }
  specificError <- paste0("{.strong `{nm}` ", report$error, "}") |>
    cli::cli_text() |>
    cli::cli_fmt() |>
    paste0(collapse = " ") |>
    paste0(".") |>
    as.character()
  message <- c("x" = specificError, "!" = msg)
  cli::cli_abort(message = message, call = call)
}
