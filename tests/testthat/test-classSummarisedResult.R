test_that("test SummarisedResult object", {
  x <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "cprd",
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
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
  expect_no_error(newSummarisedResult(x = x))
  expect_no_warning(newSummarisedResult(x = x))
  expect_identical(
    estimateTypeChoices() |> sort(),
    c(
      "numeric", "integer", "date", "character", "proportion", "percentage",
      "logical"
    ) |>
      sort()
  )
  x |>
    newSummarisedResult() |>
    inherits("summarised_result") |>
    expect_true()

  # check none character
  x <- dplyr::tibble(
    "result_id" = 1L,
    "cdm_name" = 1,
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
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
  expect_message(expect_message(newSummarisedResult(x = x)))

  # check wrong columns
  x <- dplyr::tibble(
    "package" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "cohort_name",
    "group_level" = "my_cohort",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable" = "age_group",
    "variable_level" = "10 to 50",
    "estimate_type" = "count",
    "estimate" = "5"
  )
  expect_error(newSummarisedResult(x = x))

  # check NA
  x <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "cprd",
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "cohort_name",
    "group_level" = "my_cohort",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = "Age group",
    "variable_level" = "10 to 50",
    "estimate_name" = NA_character_,
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_error(newSummarisedResult(x = x))

  # check wrong paired
  x <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "cprd",
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex &&& cohort_name",
    "group_level" = "male",
    "strata_name" = "xxx",
    "strata_level" = "y &&& cohort1",
    "variable_name" = "Age group",
    "variable_level" = "10 to 50",
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_warning(expect_warning(newSummarisedResult(x = x)))

  # check wrong case
  x <- dplyr::tibble(
    "result_id" = 1L,
    "cdm_name" = "cprd",
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex and cohort_Name",
    "group_level" = "male and cohort1",
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
  expect_warning(newSummarisedResult(x = x))

  x <- dplyr::tibble(
    "result_id" = as.integer(c(1, 2)),
    "cdm_name" = c("cprd", "eunomia"),
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
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
  expect_no_error(res <- newSummarisedResult(x = x, settings = dplyr::tibble(
    "result_id" = c(1, 2), "custom" = c("A", "B")
  )))

  expect_identical(
    sort(colnames(settings(res))),
    c(
      "custom", "package_name", "package_version", "result_id",
      "result_type", "group", "strata", "additional", "min_cell_count"
    ) |>
      sort()
  )

  expect_identical(res, res |> newSummarisedResult())

  x <- dplyr::tibble(
    "result_id" = 1L,
    "cdm_name" = "eunomia",
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = rep("number records", 2),
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = c("5", "6"),
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_error(x |> newSummarisedResult())

  x <- dplyr::tibble(
    "result_id" = 1L,
    "cdm_name" = c("eunomia", "cprd"),
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = rep("number records", 2),
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_no_error(x |> newSummarisedResult())

  x <- dplyr::tibble(
    "result_id" = 1,
    "cdm_name" = "eunomia",
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = c("number SUBJECTS", "number_subjects"),
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_error(x |> newSummarisedResult())

  x <- dplyr::tibble(
    "result_id" = 1,
    "cdm_name" = c("eunomia", "cprd"),
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = c("number SUBJECTS", "number_subjects"),
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_no_error(x |> newSummarisedResult())

  x <- dplyr::tibble(
    "result_id" = 1L,
    "cdm_name" = c("cprd", "eunomia"),
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = c("number subjects", "number records"),
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_no_error(x |> newSummarisedResult())

  x <- dplyr::tibble(
    "result_id" = 1L,
    "cdm_name" = "eunomia",
    "group_name" = c("sex", "sex", "sex", "age_group", "age_group", "calendar"),
    "group_level" = c("male", "female", "none", ">=40", "<40", "2020"),
    "strata_name" = "overall",
    "strata_level" = "overall",
    "variable_name" = "xxx",
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_no_error(x |> newSummarisedResult())
  expect_message(x |> dplyr::union_all(x) |> newSummarisedResult())
  expect_error(
    x |>
      dplyr::union_all(x |> dplyr::mutate(estimate_value = "0")) |>
      newSummarisedResult()
  )
  expect_equal(x |> dplyr::union_all(x) |> newSummarisedResult(), x |> newSummarisedResult())

  x <- dplyr::tibble(
    "result_id" = 1L,
    "cdm_name" = "eunomia",
    "group_name" = c("sex", "sex"),
    "group_level" = c("male", "male"),
    "strata_name" = "overall",
    "strata_level" = "overall",
    "variable_name" = "number_subjects",
    "variable_level" = NA_character_,
    "estimate_name" = c("count", "percentage"),
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_no_error(x <- x |> newSummarisedResult())

  y <- bind(x, x)
  expect_identical(y, x)

  x <- dplyr::tibble(
    "result_id" = 1L,
    "cdm_name" = "cprd",
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
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
  expect_message(x <- newSummarisedResult(x = x))

  expect_true(all(
    c("result_type", "package_name", "package_version") %in%
      names(settings(x))
  ))
})

test_that("validateNameLevel", {
  sr <- dplyr::tibble(
    result_id = 1L,
    cdm_name = "mock",
    group_name = "cohort_name &&& age",
    group_level = "acetaminophen &&& between 18 and 50",
    strata_name = "overall",
    strata_level = "overall",
    variable_name = "number records",
    variable_level = NA_character_,
    estimate_name = "count",
    estimate_type = "integer",
    estimate_value = "5",
    additional_name = "overall",
    additional_level = "overall"
  )
  expect_no_error(sr |> newSummarisedResult())
  expect_no_error(
    sr |>
      validateNameLevel(prefix = "group")
  )
  expect_error(validateNameLevel(sr, prefix = "group", sep = " and "))
  expect_warning(expect_warning(validateNameLevel(
    sr,
    prefix = "group", sep = " and ", validation = "warning"
  )))
  expect_warning(validateNameLevel(
    sr,
    prefix = "group", sep = " &&& | and ", validation = "warning"
  ))
})

test_that("validate duplicates", {
  x <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "cprd",
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
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
  expect_no_error(x |> newSummarisedResult())
  sr <- dplyr::bind_rows(x, x |> dplyr::mutate(estimate_value = "6"))
  expect_error(sr |> newSummarisedResult())
})

test_that("eliminate NA settings", {
  x <- dplyr::tibble(
    "result_id" = c(1L, 2L),
    "cdm_name" = "cprd",
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
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
  set <- dplyr::tibble(
    result_id = c(1L, 2L),
    result_type = c("res1", "res2"),
    param_to_eliminate = NA_character_,
    param_not_eliminated = c("1", NA_character_)
  )
  expect_no_error(res <- newSummarisedResult(x = x, settings = set))
  setres <- settings(res)
  expect_true("param_not_eliminated" %in% colnames(setres))
  expect_false("param_to_eliminate" %in% colnames(setres))
})

test_that("transformToSummarisedResult", {
  x <- dplyr::tibble(
    cohort_name = c("cohort1", "cohort2"),
    variable_name = "age",
    mean = c(50, 45.3),
    median = c(55L, 44L)
  )

  expect_no_error(res <- transformToSummarisedResult(
    x = x, group = c("cohort_name"), estimates = c("mean", "median")
  ))
  expect_true(inherits(res, "summarised_result"))

  # column not present
  expect_error(transformToSummarisedResult(
    x = x, group = c("not_present"), estimates = c("mean", "median")
  ))
  # multiple columns
  expect_error(transformToSummarisedResult(
    x = x, group = "cohort_name", strata = "cohort_name",
    estimates = c("mean", "median")
  ))
  # extra column
  expect_warning(transformToSummarisedResult(
    x = x |> dplyr::mutate(extra = 1L),
    group = c("cohort_name"),
    estimates = c("mean", "median")
  ))
  # casted column
  expect_warning(transformToSummarisedResult(
    x = x |> dplyr::mutate(rand_set = 1L),
    group = c("cohort_name"),
    estimates = c("mean", "median"),
    settings = "rand_set"
  ))

  expect_no_error(transformToSummarisedResult(
    x = x |> dplyr::select(!"variable_name"),
    group = c("cohort_name"),
    estimates = c("mean", "median")
  ))

  x <- x |>
    dplyr::mutate("study_start" = as.Date("2020-01-01"), value = T, sex = "F")
  expect_no_error(transformToSummarisedResult(
    x = x,
    group = c("cohort_name"),
    estimates = c("mean", "median", "study_start", "value", "sex")
  ))
})
