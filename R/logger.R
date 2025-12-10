
#' Create a log file
#'
#' @param logFile File path to write logging messages. You can use '\{date\}'
#' and '\{time\}' to add the date and time in the log file name.
#'
#' @return Invisible TRUE if logger was created correctly.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' logFile <- tempfile(pattern = "log_{date}_{time}", fileext = ".txt")
#' createLogFile(logFile = logFile)
#'
#' logMessage("Starting analysis")
#' 1 + 1
#' logMessage("Analysis finished")
#'
#' res <- summariseLogFile()
#'
#' glimpse(res)
#'
#' tidy(res)
#'
createLogFile <- function(logFile = here::here("log_{date}_{time}")) {
  # input check
  assertCharacter(logFile, length = 1)

  # check overwrite
  if (!is.null(getOption("omopgenerics.logFile"))) {
    cli::cli_inform(c("!" = "Overwriting current log file"))
  }

  date <- format(Sys.Date(), "%Y_%m_%d")
  time <- format(Sys.time(), "%H_%M_%S")
  logFile <- as.character(glue::glue(logFile, date = date, time = time))

  # add txt extension
  if (!endsWith(logFile, ".txt")) {
    logFile <- paste0(logFile, ".txt")
  }

  # create logger file
  createLogger(logFile)

  # store logger file in options
  options("omopgenerics.logFile" = logFile)

  invisible(TRUE)
}

#' Log a message to a logFile
#'
#' The message is written to the logFile and displayed in the console, if
#' `logFile` does not exist the message is only displayed in the console.
#'
#' @param message Message to log.
#' @param logFile File path to write logging messages. Create a logFile with
#' `createLogFile()`.
#'
#' @return Invisible TRUE if the logging message is written to a log file.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' logFile <- tempfile(pattern = "log_{date}_{time}", fileext = ".txt")
#' createLogFile(logFile = logFile)
#'
#' logMessage("Starting analysis")
#' 1 + 1
#' logMessage("Analysis finished")
#'
#' res <- summariseLogFile()
#'
#' glimpse(res)
#'
#' tidy(res)
#'
logMessage <- function(message = "Start logging file",
                       logFile = getOption("omopgenerics.logFile")) {
  # input check
  assertCharacter(message, length = 1)
  assertCharacter(logFile, length = 1, null = TRUE)

  # write message
  writeMessage(message, logFile)
}

#' Summarise and extract the information of a log file into a
#' `summarised_result` object.
#'
#' @param logFile File path to the log file to summarise. Create a logFile with
#' `createLogFile()`.
#' @param cdmName Name of the cdm for the `summarise_result` object.
#'
#' @return A `summarise_result` with the information of the log file.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' logFile <- tempfile(pattern = "log_{date}_{time}", fileext = ".txt")
#' createLogFile(logFile = logFile)
#'
#' logMessage("Starting analysis")
#' 1 + 1
#' logMessage("Analysis finished")
#'
#' res <- summariseLogFile()
#'
#' glimpse(res)
#'
#' tidy(res)
#'
summariseLogFile <- function(logFile = getOption("omopgenerics.logFile"),
                             cdmName = "unknown") {
  # input check
  assertCharacter(logFile, length = 1)
  assertCharacter(cdmName, length = 1)
  if (!file.exists(logFile)) {
    cli::cli_abort(c(x = "logFile ({logFile}) does not exist."))
  }

  # record expoting
  writeMessage("Exporting log file", logFile)

  # read
  x <- readLines(logFile)

  # extract messages
  x <- x |>
    purrr::map(\(x) {
      dplyr::tibble(
        date_time = stringr::str_extract(x, "(?<=\\[).*?(?=\\])"),
        variable_name = stringr::str_extract(x, "(?<=\\] - ).*")
      )
    }) |>
    dplyr::bind_rows(.id = "log_id") |>
    dplyr::mutate(elapsed_time = as.numeric(as.POSIXct(
      .data$date_time, "%Y-%m-%d %H:%M:%S", tz = Sys.timezone()
    ))) |>
    dplyr::arrange(as.numeric(.data$log_id)) |>
    dplyr::mutate(elapsed_time = as.integer(
      dplyr::lead(.data$elapsed_time) - .data$elapsed_time
    ))

  # transform
  x |>
    dplyr::mutate(
      cdm_name = .env$cdmName,
      variable_level = NA_character_,
      package_name = "omopgenerics",
      package_version = as.character(utils::packageVersion("omopgenerics")),
      result_type = "summarise_log_file"
    ) |>
    transformToSummarisedResult(
      strata = "log_id",
      estimates = c("date_time", "elapsed_time"),
      settings = c("package_name", "package_version", "result_type")
    )
}

createLogger <- function(logFile) {
  # delete if logFile exists
  if (file.exists(logFile)) {
    c("!" = "Deleting prior existing log file: {.path {logFile}}.") |>
      cli::cli_inform()
    file.remove(logFile)
  }

  # create logFile
  cli::cli_inform(c(i = "Creating log file: {.path {logFile}}."))
  file.create(logFile)
  writeMessage("Log file created", logFile)
}
writeMessage <- function(message, logFile) {
  time <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")

  # display message
  cli::cli_inform("{.pkg {time}} - {message}")

  # logFile exists report it
  if (is.null(logFile)) {
    invisible(FALSE)
  } else if (file.exists(logFile)) {
    message <- paste0(time, " - ", message)
    con <- file(logFile, open = "a")
    writeLines(text = message, con = con)
    close(con)
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}

startLogQuery <- function(x, type = NULL, name = NULL, temporary = NULL, overwrite = NULL, logPrefix = NULL) {
  # check if source is db
  logSqlPath <- getOption(x = "omopgenerics.log_sql_path")
  sqlExplain <- getOption("omopgenerics.log_sql_explain", default = "FALSE") |>
    as.logical()
  logQuery <- inherits(cdmSource(x), "db_cdm") & !is.null(logSqlPath)

  if (logQuery) {
    if (!dir.exists(logSqlPath)) {
      cli::cli_inform("SQL query not saved as '{logSqlPath}' not an existing directory")
      logQuery <- NULL
    } else {
      start <- Sys.time()
      md <- metadata(
        type = type,
        name = name,
        temporary = temporary,
        overwrite = overwrite,
        logPrefix = logPrefix,
        src = omopgenerics::cdmSource(x)
      )
      qr <- formatQuery(x = x)
      txt <- c(md, qr)

      # if log explain
      if (sqlExplain) {
        ex <- formatExplain(x = x)
        txt <- c(txt, ex)
      }

      file_query <- file.path(logSqlPath, queryFile("query"))
      writeLines(text = txt, con = file_query)

      cli::cli_inform("SQL query saved to {.path {file_query}}.")

      # increase query id
      increaseQueryId()

      logQuery <- list(start = start, file_query = file_query)
    }
  } else {
    logQuery <- NULL
  }
  return(logQuery)
}
finishLogQuery <- function(logQuery) {
  if (length(logQuery) > 0) {
    start <- logQuery$start
    file_query <- logQuery$file_query

    end <- Sys.time()
    time_diff <- sprintf("%.3f seconds", difftime(time1 = end, time2 = start, units = "secs"))
    lines <- readLines(file_query)
    lines <- gsub(pattern = "^time_taken: pending$",
                  replacement = paste0("time_taken: ", time_diff),
                  x = lines)
    writeLines(text = lines, con = file_query)
  }
  invisible()
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
metadata <- function(type, name, temporary, overwrite, logPrefix, src) {
  msg <- paste0("type: ", type)
  schema <- attr(src, "write_schema")
  for (nm in c("catalog", "schema", "prefix")) {
    if (nm %in% names(schema)) {
      msg <- c(msg, paste0(nm, ": ", unname(schema[nm])))
    }
  }
  if (!is.null(name)) {
    if (is.na(name)) {
      msg <- c(msg, "name: NA")
    } else {
      msg <- c(msg, paste0("name: ", name))
    }
  }
  if (!is.null(temporary)) {
    msg <- c(msg, paste0("temporary: ", temporary))
  }
  if (!is.null(overwrite)) {
    msg <- c(msg, paste0("overwrite: ", overwrite))
  }
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
    ex[-1] <- gsub(pattern = "\r|\n", replacement = "", x = ex[-1]) |>
      trimws()
    ex[-1] <- paste0("  ", ex[-1])
  }
  ex
}
summariseLogSqlPath <- function(logSqlPath = getOption("omopgenerics.log_sql_path"),
                                cdmName = "unknown") {
  # input check
  assertCharacter(logSqlPath, length = 1)
  assertCharacter(cdmName, length = 1)

  # check if dir does not exist
  if (!dir.exists(logSqlPath)) {
    cli::cli_warn(c(x = "{.path {logSqlPath}} does not exist."))
    return(emptySummarisedResult())
  }

  # find files
  sqlFiles <- list.files(path = logSqlPath, pattern = "*.txt$")

  # required cols
  cols <- c(
    "type", "name", "temporary", "overwrite", "log_prefix", "catalog",
    "schema", "prefix", "time_taken", "sql", "explain"
  )

  # analyse files
  sqlLogs <- sqlFiles |>
    sort() |>
    purrr::map(\(x) {
      tryCatch({
        res <- dplyr::as_tibble(read.dcf(file.path(logSqlPath, x)))
        if (nrow(res) != 1) stop()
        if (!all(colnames(res) %in% cols)) stop()
        res
      }, error = function(e) {
        cli::cli_inform(c("!" = "File {.pkg {x}} not properly formatted."))
        NULL
      })
    }) |>
    dplyr::bind_rows(.id = "log_id") |>
    purrr::compact()

  # check empty
  if (length(sqlLogs) == 0) {
    cli::cli_warn(c(x = "{.path {logSqlPath}} does not contain any log file."))
    return(emptySummarisedResult())
  }

  # remove unexpected columns
  extraColumns <- colnames(sqlLogs)
  extraColumns <- extraColumns[!extraColumns %in% c(cols, "log_id")]
  if (length(extraColumns) > 0) {
    cli::cli_inform(c(i = "Dropping unexpected columns: {.var {extraColumns}}."))
    sqlLogs <- sqlLogs |>
      dplyr::select(!dplyr::all_of(extraColumns))
  }

  # estimates
  group <- "log_id"
  strata <- c("type", "log_prefix", "name") |>
    purrr::keep(\(x) x %in% colnames(sqlLogs))
  additional <- c("temporary", "overwrite") |>
    purrr::keep(\(x) x %in% colnames(sqlLogs))
  sets <- c("catalog", "schema", "prefix") |>
    purrr::keep(\(x) x %in% colnames(sqlLogs))

  if ("explain" %in% colnames(sqlLogs)) {
    sqlLogs <- sqlLogs |>
      dplyr::rename(variable_level = "explain")
  } else {
    sqlLogs <- sqlLogs |>
      dplyr::mutate(variable_level = NA_character_)
  }

  # format result
  sqlLogs |>
    dplyr::rename(variable_name = "sql") |>
    dplyr::mutate(
      cdm_name = .env$cdmName,
      package_name = "omopgenerics",
      package_version = as.character(utils::packageVersion("omopgenerics")),
      result_type = "summarise_log_sql"
    ) |>
    transformToSummarisedResult(
      group = group,
      strata = strata,
      additional = additional,
      estimates = "time_taken",
      settings = c("package_name", "package_version", "result_type", sets)
    )
}
