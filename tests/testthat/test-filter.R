test_that("filterSettings", {
  result <- emptySummarisedResult()

  expect_warning(result1 <- result |>
    filterSettings(does_not_exist == "omock"))

  expect_identical(result, result1)

  result <- mockSummarisedResult()
  expect_identical(
    result,
    result |> filterSettings(result_type == "mock_summarised_result")
  )
  expect_warning(
    result0 <- result |> filterSettings(variable_does_not_exist == TRUE)
  )
  expect_true(nrow(result0) == 0)

  x <- dplyr::tibble(
    "result_id" = as.integer(c(1, 2)),
    "cdm_name" = c("cprd", "eunomia"),
    "group_name" = "cohort",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = "Age group",
    "variable_level" = "10 to 50",
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_no_error(res <- newSummarisedResult(
    x = x,
    settings = dplyr::tibble(
      "result_id" = c(1, 2),
      "result_type" = "summarised_characteristics",
      "package_name" = c("omock", "omopgenerics"),
      "package_version" = "0.4.0",
      "custom" = c("A", "B")
    )
  ))

  res1 <- res |> filterSettings(package_name == "omock")
  expect_true(nrow(res1) == 1)
  expect_true(res1$result_id == 1)

  res2 <- res |> filterSettings(custom == "B")
  expect_true(nrow(res2) == 1)
  expect_true(res2$result_id == 2)
})

test_that("filterNameLevel works", {
  x <- dplyr::tibble(
    "result_id" = 1L,
    "cdm_name" = "eunomia",
    "group_name" = "cohort_name",
    "group_level" = "my_cohort",
    "strata_name" = c("sex", "sex &&& age_group", "sex &&& year"),
    "strata_level" = c("Female", "Male &&& <40", "Female &&& 2010"),
    "variable_name" = "number subjects",
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "integer",
    "estimate_value" = c("100", "44", "14"),
    "additional_name" = "overall",
    "additional_level" = "overall"
  ) |>
    newSummarisedResult()

  xn <- x |> filterNameLevel("strata", sex == "Female")
  expect_s3_class(xn, "summarised_result")
  expect_true(nrow(xn) == 2)

  # .data works
  xn <- x |> filterNameLevel("strata", .data$sex == "Female")
  expect_true(nrow(xn) == 2)

  # multiple filters works
  x1 <- x |> filterNameLevel("strata", sex == "Female", year == "2010")
  expect_true(nrow(x1) == 1)
  x2 <- x |> filterNameLevel("strata", sex == "Female" & year == "2010")
  expect_true(nrow(x2) == 1)
  expect_identical(x1, x2)

  # warning if variable does not exist
  expect_warning(xn <- x |> filterNameLevel("strata", does_not_exist == "1"))
  expect_true(nrow(xn) == 0)
})

test_that("filterStrata/Group/Additional works", {
  x <- dplyr::tibble(
    "result_id" = 1L,
    "cdm_name" = "eunomia",
    "group_name" = "cohort_name",
    "group_level" = "my_cohort",
    "strata_name" = c("sex", "sex &&& age_group", "sex &&& year"),
    "strata_level" = c("Female", "Male &&& <40", "Female &&& 2010"),
    "variable_name" = "number subjects",
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "integer",
    "estimate_value" = c("100", "44", "14"),
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  res <- newSummarisedResult(x)

  # strata
  expect_no_error(xs <- res |> filterStrata(sex == "Female"))
  expect_true(nrow(xs) == 2)

  res <- x |>
    dplyr::rename(
      "group_name" = "strata_name", "group_level" = "strata_level",
      "strata_name" = "group_name", "strata_level" = "group_level"
    ) |>
    newSummarisedResult()

  # group
  expect_no_error(xg <- res |> filterGroup(sex == "Female"))
  expect_true(nrow(xg) == 2)

  res <- x |>
    dplyr::rename(
      "strata_name" = "additional_name", "strata_level" = "additional_level",
      "additional_name" = "strata_name", "additional_level" = "strata_level"
    ) |>
    newSummarisedResult()

  # additional
  expect_no_error(xa <- res |> filterAdditional(sex == "Female"))
  expect_true(nrow(xa) == 2)

  xs <- xs |>
    dplyr::select(!dplyr::starts_with(c("strata", "group", "additional")))
  xg <- xg |>
    dplyr::select(!dplyr::starts_with(c("strata", "group", "additional")))
  xa <- xa |>
    dplyr::select(!dplyr::starts_with(c("strata", "group", "additional")))
  attr(xs, "settings") <- NULL
  attr(xg, "settings") <- NULL
  attr(xa, "settings") <- NULL

  expect_identical(xs, xg)
  expect_identical(xs, xa)

  # call works
  expect_error(filterStrata(list()))
})
