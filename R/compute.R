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

#' Store results in a table.
#'
#' @param x Table in the cdm.
#' @param name Name to store the table with.
#' @param temporary Whether to store table temporarily (TRUE) or permanently
#' (FALSE).
#' @param overwrite Whether to overwrite previously existing table with name
#' same.
#' @param logPrefix Prefix to use when saving a log file.
#' @param ... For compatibility (not used).
#'
#' @return Reference to a table in the cdm
#'
#' @export
#' @importFrom dplyr compute
compute.cdm_table <- function(x,
                              name = NULL,
                              temporary = NULL,
                              overwrite = TRUE,
                              logPrefix = NULL,
                              ...) {
  if (is.character(name) & is.null(temporary)) temporary <- FALSE
  if (is.null(name)) name <- uniqueTableName()
  if (is.null(temporary)) temporary <- TRUE
  src <- tableSource(x)
  cl <- class(src)[class(src) != "cdm_source"]
  cx <- class(x)

  # check if source is db
  logSqlPath <- getOption(x = "omopgenerics.log_sql_path")
  logSqlExplainPath <- getOption("omopgenerics.log_sql_explain_path")

  if (inherits(cdmSource(x), "db_cdm")) {
    if (!is.null(logSqlPath) | !is.null(logSqlExplainPath) ) {
      start <- Sys.time()
    }

    # log sql if option set
    if (!is.null(logSqlPath)) {
      # must have specified a directory that exists
      if (dir.exists(logSqlPath)) {
        md <- metadata(
          name = name,
          temporary = temporary,
          overwrite = overwrite,
          logPrefix = logPrefix,
          src = src
        )
        qr <- formatQuery(x = x)
        file_query <- file.path(logSqlPath, queryFile("query"))
        writeLines(text = c(md, qr), con = file_query)
        cli::cli_inform("SQL query saved to {.path {file_query}}.")
      } else {
        cli::cli_inform("SQL query not saved as '{logSqlPath}' not an existing directory")
      }
    }

    # log explain if option set
    if (!is.null(logSqlExplainPath)) {
      # must have specified a directory that exists
      if (dir.exists(logSqlExplainPath)) {
        md <- metadata(
          name = name,
          temporary = temporary,
          overwrite = overwrite,
          logPrefix = logPrefix,
          src = src
        )
        ex <- formatExplain(x = x)
        file_explain <- file.path(logSqlExplainPath, queryFile("explain"))
        writeLines(text = c(md, ex), con = file_explain)
        cli::cli_inform("SQL explain saved to {.path {file_explain}}.")
      } else {
        cli::cli_inform("SQL explain not saved as '{logSqlExplainPath}' not an existing directory")
      }
    }

    # increase query id
    increaseQueryId()
  }

  res <- x |>
    keepClass() |>
    addClass(cl) |>
    dplyr::compute(name = name, temporary = temporary, overwrite = overwrite)
  if (temporary) name <- NA_character_
  res <- res |>
    removeClass(cl) |>
    newCdmTable(src = src, name = name) |>
    restoreClass(cx) |>
    restoreAttributes(keepAttributes(x, cx))

  # update log with time taken
  if (inherits(cdmSource(x), "db_cdm")) {
    if (!is.null(logSqlPath) | !is.null(logSqlExplainPath)) {
      end <- Sys.time()
      time_diff <- sprintf("%.3f seconds", difftime(time1 = end, time2 = start, units = "secs"))
    }

    if (!is.null(logSqlPath)) {
      if (dir.exists(logSqlPath)) {
        lines <- readLines(file_query)
        lines <- gsub("^time_taken: pending$",
                      paste0("time_taken: ", time_diff),
                      lines)
        writeLines(lines, file_query)
      }
    }

    if (!is.null(logSqlExplainPath)) {
      if (dir.exists(logSqlExplainPath)) {
        lines <- readLines(file_explain)
        lines <- gsub("^time_taken: pending$",
                      paste0("time_taken: ", time_diff),
                      lines)
        writeLines(lines, file_explain)
      }
    }
  }

  return(res)
}
queryId <- function() {
  getOption(x = "og_query_id", default = 1L)
}
increaseQueryId <- function() {
  options(og_query_id = queryId() + 1L)
}
queryFile <- function(type) {
  paste0(
    "logged_", type, "_", sprintf("%05i", queryId()), "_on_",
    format(Sys.time(), format = "%Y_%m_%d_at_%H_%M_%S"), ".txt"
  )
}
metadata <- function(name, temporary, overwrite, logPrefix, src) {
  msg <- c("type: compute")
  schema <- attr(src, "write_schema")
  for (nm in c("catalog", "schema", "prefix")) {
    if (nm %in% names(schema)) {
      msg <- c(msg, paste0(nm, ": ", unname(schema[nm])))
    }
  }
  if (is.na(name)) {
    msg <- c(msg, "name: NA")
  } else {
    msg <- c(msg, paste0("name: ", name))
  }
  msg <- c(msg, paste0("temporary: ", temporary), paste0("overwrite: ", overwrite))
  if (!is.null(logPrefix)) {
    msg <- c(msg, paste0("log_prefix: ", logPrefix))
  }
  msg <- c(msg, paste0("time_taken: pending"))
  msg
}
formatQuery <- function(x) {
  qr <- utils::capture.output(dplyr::show_query(x))
  qr[1] <- paste0("sql: ", qr[1])
  if (length(qr) > 1) {
    qr[-1] <- paste0("  ", qr[-1])
  }
  qr
}
formatExplain <- function(x) {
  ex <- utils::capture.output(dplyr::explain(x))
  id <- min(which(grepl(pattern = "<PLAN>", x = ex)))
  if (length(id) != 1) {
    cli::cli_inform(c("!" = "Incorrectly formatted <explain>."))
    return("explain: -")
  }
  ex <- ex[id:length(ex)]
  ex[1] <- paste0("explain: ", ex[1])
  if (length(ex) > 1) {
    ex[-1] <- paste0("  ", ex[-1])
  }
  ex
}

#' @export
compute.local_cdm <- function(x, ...) {
  return(x)
}

#' Create a unique table name
#'
#' @param prefix Prefix for the table names.
#'
#' @return A string that can be used as a dbplyr temp table name
#' @export
#'
#' @examples
#' library(omopgenerics)
#' uniqueTableName()
uniqueTableName <- function(prefix = "") {
  assertCharacter(x = prefix, length = 1)
  i <- getOption("og_table_name", 0) + 1
  options(og_table_name = i)
  value <- paste0(sprintf("og_%03i", i), "_", round(as.numeric(Sys.time())))
  paste0(prefix, value)
}

#' Create a temporary prefix for tables, that contains a unique prefix that
#' starts with tmp.
#'
#' @return A temporary prefix.
#' @export
#'
#' @examples
#' library(omopgenerics)
#' tmpPrefix()
tmpPrefix <- function() {
  i <- getOption("tmp_prefix_number", 0) + 1
  options(tmp_prefix_number = i)
  sprintf("tmp_%03i_", i)
}
