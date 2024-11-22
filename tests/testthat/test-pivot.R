test_that("pivotEstimates", {
  result <- mockSummarisedResult()
  res0 <- pivotEstimates(result)
  expect_true(all(c("count", "mean", "sd", "percentage") %in% colnames(res0)))
  expect_true(class(res0$count) == "integer")
  expect_true(class(res0$mean) == "numeric")
  expect_true(class(res0$sd) == "numeric")
  expect_true(class(res0$percentage) == "numeric")
  res1 <- pivotEstimates(result, nameStyle = "hola_{estimate_name}")
  expect_true(all(c("hola_count", "hola_mean", "hola_sd", "hola_percentage") %in% colnames(res1)))
  expect_true(class(res1$hola_count) == "integer")
  expect_true(class(res1$hola_mean) == "numeric")
  res2 <- pivotEstimates(result, pivotEstimatesBy = c("group_name", "estimate_name"))
  expect_true(all(c("cohort_name_count", "cohort_name_mean", "cohort_name_sd", "cohort_name_percentage") %in% colnames(res2)))
  expect_no_error(pivotEstimates(result, pivotEstimatesBy = "variable_name"))

  # expected errors
  expect_error(pivotEstimates(result, pivotEstimatesBy = "estimate_type"))
})

test_that("remove if variable_name is NA", {
  result <- mockSummarisedResult()
  res0 <- pivotEstimates(result, pivotEstimatesBy = c("variable_name", "variable_level", "estimate_name"))
  expect_true(all(!c("age_NA_mean", "number subjects_NA_count", "age_NA_sd") %in% colnames(res0)))
  expect_true(all(c("age_mean", "number subjects_count", "age_sd") %in% colnames(res0)))
})
