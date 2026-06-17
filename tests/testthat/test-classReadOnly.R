readOnlyCdm <- function() {
  src <- newReadOnlySource()
  cdmTables <- list(
    "person" = dplyr::tibble(
      person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
      race_concept_id = 0L, ethnicity_concept_id = 0L
    ) |>
      newCdmTable(src, "person"),
    "observation_period" = dplyr::tibble(
      observation_period_id = 1L, person_id = 1L,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2023-12-31"),
      period_type_concept_id = 0L
    ) |>
      newCdmTable(src, "observation_period")
  )
  newCdmReference(tables = cdmTables, cdmName = "read_only", .softValidation = TRUE)
}

test_that("read-only source can be constructed and summarised", {
  expect_no_error(src <- newReadOnlySource())
  expect_s3_class(src, "cdm_source")
  expect_s3_class(src, "read_only_source")
  expect_identical(sourceType(src), "read_only")
  expect_identical(listSourceTables(src), character())
  expect_identical(cdmDisconnect(src), invisible(TRUE))

  srcSummary <- summary(src)
  expect_s3_class(srcSummary, "cdm_source_summary")
  expect_identical(srcSummary$type, "read_only")
  expect_identical(srcSummary$package, "omopgenerics")
})

test_that("read-only cdm tables can be read", {
  cdm <- readOnlyCdm()

  expect_true(inherits(cdm, "cdm_reference"))
  expect_identical(sourceType(cdm), "read_only")
  expect_no_error(person <- dplyr::collect(cdm$person))
  expect_identical(person$person_id, 1L)
  expect_no_error(personCount <- cdm$person |> dplyr::tally() |> dplyr::collect())
  expect_identical(personCount$n, 1L)
})

test_that("read-only source rejects source mutations and materialisation", {
  cdm <- readOnlyCdm()
  src <- cdmSource(cdm)
  msg <- "read-only CDM source"

  expect_error(insertTable(cdm, "x", dplyr::tibble(x = 1L)), msg)
  expect_error(insertTable(src, "x", dplyr::tibble(x = 1L)), msg)
  expect_error(dropSourceTable(cdm, "person"), msg)
  expect_error(dropSourceTable(src, "person"), msg)
  expect_error(readSourceTable(cdm, "person"), msg)
  expect_error(readSourceTable(src, "person"), msg)
  expect_error(cdm$person |> dplyr::compute(), msg)
  expect_error(insertFromSource(cdm, dplyr::tibble(x = 1L)), msg)
  expect_error(cdmTableFromSource(src, dplyr::tibble(x = 1L)), msg)
  expect_error(insertCdmTo(cdm, src), msg)
})
