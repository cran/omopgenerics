test_that("basic logger functionality", {
  # no error if logFile not cretaed
  expect_false(logMessage("gjkd"))

  # if logger is not created it is created
  logFile <- tempfile(fileext = ".txt")
  expect_false(file.exists(logFile))
  expect_true(createLogFile(logFile = logFile))
  expect_true(file.exists(logFile))

  expect_true(logMessage("Start analysis"))

  expect_no_error(res <- summariseLogFile())

  expect_identical(
    tidy(res) |>
      dplyr::select("log_id", "variable_name"),
    dplyr::tibble(
      log_id = c("1", "2", "3"),
      variable_name = c("Log file created", "Start analysis", "Exporting log file")
    )
  )

  unlink(logFile)

  logFile <- tempfile()
  expect_false(file.exists(logFile))
  expect_true(createLogFile(logFile = logFile))
  logFile <- paste0(logFile, ".txt")
  expect_true(createLogFile(logFile = logFile))
  expect_true(file.exists(logFile))
  unlink(logFile)

  # no error if logFile does not exist
  expect_false(logMessage("gjkd"))

  expect_error(summariseLogFile())
})
