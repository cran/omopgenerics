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

test_that("logMessage evaluates cli expressions in the calling environment", {
  withr::local_options(list(cli.num_colors = 8))
  logFile <- tempfile(fileext = ".txt")
  expect_true(file.create(logFile))

  x <- c("a", "b", "c")
  for (i in seq_along(x)) {
    expect_message(
      result <- logMessage("{x[i]}", logFile = logFile),
      regexp = x[[i]],
      fixed = TRUE
    )
    expect_true(result)
  }

  expect_message(
    result <- logMessage("{.pkg jksdkf}", logFile = logFile),
    regexp = "jksdkf",
    fixed = TRUE
  )
  expect_true(result)

  messages <- readLines(logFile)
  expect_identical(
    sub("^.*\\] - ", "", messages),
    c(x, "jksdkf")
  )
  expect_false(any(grepl("\033", messages, fixed = TRUE)))
})

test_that("test sql logging", {
  skip_on_cran()
  cdm <- omock::mockCdmFromDataset(datasetName = "GiBleed", source = "duckdb")

  sqlPath <- file.path(tempdir(), "og_sql_files")
  unlink(sqlPath, recursive = TRUE)
  dir.create(sqlPath)

  options(omopgenerics.log_sql_path = sqlPath)
  options(omopgenerics.log_sql_explain = TRUE)

  cdm$drug_exposure <- cdm$drug_exposure |>
    dplyr::left_join(
      cdm$concept |>
        dplyr::select("drug_concept_id" = "concept_id", "concept_name"),
      by = "drug_concept_id"
    ) |>
    dplyr::compute(name = "drug_exposure")

  expect_true(length(list.files(path = sqlPath)) == 1)

  de <- cdm$drug_exposure |>
    dplyr::collect()

  expect_true(length(list.files(path = sqlPath)) == 2)

  expect_no_error(res1 <- summariseLogSqlPath())
  expect_true(nrow(res1) == 2)

  dropSourceTable(cdm = cdm, name = dplyr::everything())
  cdmDisconnect(cdm = cdm)
  unlink(x = sqlPath, recursive = TRUE)
})
