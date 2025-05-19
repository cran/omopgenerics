
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
    dplyr::bind_rows(.id = "log_id")

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
      estimates = "date_time",
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
