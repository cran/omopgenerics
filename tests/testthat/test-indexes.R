test_that("index helpers accept cdm_table", {
  cdm <- emptyCdmReference("mock")

  expect_no_error(expected <- expectedIndexes(cdm$person))
  expect_identical(expected, expectedIndexes(cdm, name = "person"))

  expect_no_error(existing <- existingIndexes(cdm$person))
  expect_identical(existing, existingIndexes(cdm, name = "person"))

  expect_no_error(status <- statusIndexes(cdm$person))
  expect_identical(status, statusIndexes(cdm, name = "person"))

  expect_no_error(created <- createIndexes(cdm$person))
  expect_true(created)
})
