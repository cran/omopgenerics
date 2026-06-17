test_that("tidy", {
  mocksum <- mockSummarisedResult()
  expect_no_error(res0 <- tidy(mocksum))
  expect_true(all(c(
    "cdm_name", "cohort_name", "age_group", "sex", "variable_name",
    "variable_level", "count", "mean", "sd", "percentage"
  ) %in% colnames(res0)))
  expect_true(class(res0$percentage) == "numeric")
  expect_true(class(res0$mean) == "numeric")
  expect_true(class(res0$count) == "integer")

  # 2 id's:
  mocksum2 <- mocksum |> bind(mocksum)
  expect_no_error(res1 <- tidy(mocksum2))
  expect_true(all(c(
    "cdm_name", "cohort_name", "age_group", "sex", "variable_name",
    "variable_level", "count", "mean", "sd", "percentage"
  ) %in% colnames(res0)))

  # check no more aguments in tidy method
  expect_warning(tidy(mocksum, splitStrata = FALSE))
})

test_that("tidy, dates", {
  result <- dplyr::tibble(
    "result_id" = integer(1),
    "cdm_name" = "mock",
    "group_name" = "cohort_name",
    "group_level" = c(rep("cohort1", 9), rep("cohort2", 9)),
    "strata_name" = rep(c(
      "overall", rep("age_group &&& sex", 4), rep("sex", 2), rep("age_group", 2)
    ), 2),
    "strata_level" = rep(c(
      "overall", "<40 &&& Male", ">=40 &&& Male", "<40 &&& Female",
      ">=40 &&& Female", "Male", "Female", "<40", ">=40"
    ), 2),
    "variable_name" = "number subjects",
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "integer",
    "estimate_value" = round(10000000 * stats::runif(18)) |> as.character(),
    "additional_name" = "overall",
    "additional_level" = "overall"
  ) |>
    dplyr::union_all(
      dplyr::tibble(
        "result_id" = integer(1),
        "cdm_name" = "mock",
        "group_name" = "cohort_name",
        "group_level" = c(rep("cohort1", 9), rep("cohort2", 9)),
        "strata_name" = rep(c(
          "overall", rep("age_group &&& sex", 4), rep("sex", 2), rep("age_group", 2)
        ), 2),
        "strata_level" = rep(c(
          "overall", "<40 &&& Male", ">=40 &&& Male", "<40 &&& Female",
          ">=40 &&& Female", "Male", "Female", "<40", ">=40"
        ), 2),
        "variable_name" = "start date",
        "variable_level" = NA_character_,
        "estimate_name" = "date",
        "estimate_type" = "date",
        "estimate_value" = as.Date("2020-10-01") |> as.character(),
        "additional_name" = "overall",
        "additional_level" = "overall"
      )
    ) |>
    newSummarisedResult(
      settings = dplyr::tibble(
        "result_id" = integer(1),
        "result_type" = "mock_summarised_result",
        "package_name" = "omopgenerics",
        "package_version" = utils::packageVersion("omopgenerics") |>
          as.character()
      )
    )
  expect_no_error(result_out <- tidy(result))
  expect_true(class(as.Date(result_out |> dplyr::pull(date))) == "Date")
})
