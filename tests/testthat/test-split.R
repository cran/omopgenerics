test_that("splitGroup", {
  tib <- dplyr::tibble(
    group_name = c("cohort_name", "cohort_name &&& age", "age &&& sex"),
    group_level = c("acetaminophen", "ibuprofen &&& 10 to 19", "20 to 29 &&& Male"),
    x_name = c("cohort_name", "cohort_name &&& age", "age &&& sex"),
    x_level = c("acetaminophen", "ibuprofen &&& 10 to 19", "20 to 29 &&& Male"),
    z = c(1, 2, 3),
    a = c("a", "b", "c")
  )
  expect_error(tib |> splitGroup(NA_character_))
  expect_no_error(res0 <- tib |> splitGroup())
  expect_false("group_name" %in% colnames(res0))
  expect_false("group_level" %in% colnames(res0))
  expect_true(all(c("cohort_name", "age", "sex") %in% colnames(res0)))
  expect_equal(res0$cohort_name, c("acetaminophen", "ibuprofen", "overall"))
  expect_equal(res0$age, c("overall", "10 to 19", "20 to 29"))
  expect_equal(res0$sex, c("overall", "overall", "Male"))
  expect_no_error(
    res1 <- tib |>
      splitNameLevelInternal(keep = TRUE, prefix = "group", fill = "overall")
  )
  expect_true("group_name" %in% colnames(res1))
  expect_true("group_level" %in% colnames(res1))
  expect_true(all(c("cohort_name", "age", "sex") %in% colnames(res1)))
  expect_warning(
    tib |>
      splitNameLevelInternal(keep = TRUE, prefix = "group", fill = "overall") |>
      splitGroup()
  )
  expect_no_error(
    res2 <- tib |>
      splitNameLevelInternal(prefix = "x", keep = TRUE, fill = "overall")
  )
  cols <- colnames(res1) |> sort()
  expect_equal(
    res1 |> dplyr::select(dplyr::all_of(cols)),
    res2 |> dplyr::select(dplyr::all_of(cols))
  )
  expect_error(tib |> splitNameLevelInternal(prefix = "not_extisting"))
  expect_no_error(res3 <- tib |> splitNameLevelInternal(prefix = "x", keep = TRUE, fill = "Any"))
  expect_true(all(res2$age[res2$age != "overall"] == res3$age[res2$age != "overall"]))
  expect_true(all(res2$sex[res2$sex != "overall"] == res3$sex[res2$sex != "overall"]))
  expect_true(all(res3$age[res2$age == "overall"] == "Any"))
  expect_true(all(res3$sex[res2$sex == "overall"] == "Any"))

  tib <- dplyr::tibble(
    group_name = c("overall", "cohort_name &&& age", "age &&& sex"),
    group_level = c("overall", "ibuprofen &&& 10 to 19", "20 to 29 &&& Male"),
    z = c(1, 2, 3),
    a = c("a", "b", "c")
  )
  expect_false("overall" %in% colnames(splitGroup(tib)))
  expect_true(all(groupColumns(tib) == c("cohort_name", "age", "sex")))

  result <- mockSummarisedResult()
  expect_identical(
    result |> splitAll(),
    result |> splitGroup() |> splitStrata() |> splitAdditional()
  )

  res <- result |>
    splitAll(exclude = c("group", "variable"))
  expect_true(all(c("group_name", "group_level") %in% colnames(res)))
  expect_false(all(c("strata_name", "strata_level") %in% colnames(res)))
  expect_false(all(c("additional_name", "additional_level") %in% colnames(res)))

  result$group_name[1] <- "cohort_name &&& not_present"
  expect_warning(res <- result |> splitAll())
  expect_true(all(c("group_name", "group_level") %in% colnames(res)))
  expect_false(all(c("strata_name", "strata_level") %in% colnames(res)))
  expect_false(all(c("additional_name", "additional_level") %in% colnames(res)))

  # summarised_result
  x <- dplyr::tibble(
    result_id = c(1L, 2L, 3L),
    cdm_name = "mock",
    group_name = "cohort_name",
    group_level = "my_cohort",
    strata_name = c("overall", "age_group", "sex &&& year"),
    strata_level = c("overall", "10 to 20", "Female &&& 2010"),
    variable_name = "number subjects",
    variable_level = NA_character_,
    estimate_name = "count",
    estimate_type = "integer",
    estimate_value = c("500", "35", "12"),
    additional_name = "overall",
    additional_level = "overall"
  )

  res1 <- newSummarisedResult(x)
  res2 <- newSummarisedResult(x, dplyr::tibble(
    result_id = c(1L, 2L, 3L),
    result_type = "my_result",
    x = c("res1", "res2", "res3"),
    strata = c("", "age_group", "sex &&& year &&& dep_index"),
    param = "test"
  ))

  xs <- splitStrata(x)
  res1s <- dplyr::as_tibble(splitStrata(res1))
  attr(res1s, "settings") <- NULL
  expect_identical(xs, res1s)

  expect_true(!"dep_index" %in% strataColumns(res1))
  expect_true("dep_index" %in% strataColumns(res2))

  expect_true("dep_index" %in% colnames(splitStrata(res2)))
  expect_true("dep_index" %in% colnames(tidy(res2)))
  expect_true("dep_index" %in% colnames(splitAll(res2)))
  expect_true(!"param" %in% colnames(splitStrata(res2)))
  expect_true("param" %in% colnames(tidy(res2)))
  expect_true(!"param" %in% colnames(splitAll(res2)))
  expect_true(!"result_type" %in% colnames(splitStrata(res2)))
  expect_true(!"result_type" %in% colnames(tidy(res2)))
  expect_true(!"result_type" %in% colnames(splitAll(res2)))

  expect_no_error(
    dplyr::tibble(
      additional_name = "prevalence_start_date &&& prevalence_end_date",
      additional_level = "2000-01-01 &&& 2000-01-01"
    )  |>
      splitAdditional()
  )
})
