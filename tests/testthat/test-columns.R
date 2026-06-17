test_that("omop column functions work", {
  # correct values
  expect_identical(
    fieldsTables[["5.3"]] |>
      dplyr::filter(cdm_table_name == "person", type == "cdm_table") |>
      dplyr::pull("cdm_field_name"),
    omopColumns("person")
  )
})

test_that("achilles column functions work", {
  # correct values
  expect_identical(
    fieldsTables[["5.3"]] |>
      dplyr::filter(
        cdm_table_name == "achilles_analysis", type == "achilles",
      ) |>
      dplyr::pull("cdm_field_name"),
    achillesColumns("achilles_analysis")
  )
})

test_that("cohort column functions work", {
  # basic functionality
  expect_no_error(cohortCols <- cohortColumns(table = "cohort"))

  # correct values
  expect_identical(
    fieldsTables[["5.3"]] |>
      dplyr::filter(cdm_table_name == "cohort", type == "cohort") |>
      dplyr::pull("cdm_field_name"),
    cohortCols
  )
})

test_that("summarised result get columns in mock", {
  # group columns
  expect_identical(
    mockSummarisedResult() |> groupColumns(),
    c("cohort_name")
  )

  # strata columns
  expect_identical(
    mockSummarisedResult() |> strataColumns(),
    c("age_group", "sex")
  )

  # additional columns
  expect_identical(
    mockSummarisedResult() |> additionalColumns(),
    character()
  )

  # tidyColumns
  expect_identical(
    colnames(tidy(mockSummarisedResult())),
    tidyColumns(mockSummarisedResult())
  )
  expect_identical(
    colnames(tidy(emptySummarisedResult())),
    tidyColumns(emptySummarisedResult())
  )

  result <- mockSummarisedResult()

  cols <- result |>
    settings() |>
    colnames()

  cols <- cols[cols != "result_id"]

  expect_equal(settingsColumns(result, metadata = TRUE), cols)
})
