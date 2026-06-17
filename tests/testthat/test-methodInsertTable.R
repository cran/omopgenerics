test_that("multiplication works", {
  cdm <- emptyCdmReference(cdmName = "test")
  expect_true(!"my_table" %in% names(cdm))
  expect_no_error(cdm <- insertTable(
    cdm = cdm, name = "my_table", table = dplyr::tibble(a = 1)
  ))
  expect_true("my_table" %in% names(cdm))
  expect_true(inherits(cdm$my_table, "cdm_table"))

  expect_no_error(cdm2 <- insertTable(
    cdm = cdm$person, name = "table_from_cdm_table",
    table = dplyr::tibble(b = 1)
  ))
  expect_true("table_from_cdm_table" %in% names(cdm2))
  expect_true(inherits(cdm2$table_from_cdm_table, "cdm_table"))
  expect_identical(cdmName(cdm2), cdmName(cdm))

  x <- dplyr::tibble(a = 1)
  class(x) <- "my_sql_table"
  expect_error(cdm <- insertTable(cdm = cdm, name = "my_table", table = x))
})
