test_that("uniteGroup", {
  tib <- dplyr::tibble(
    age = c(NA, ">40", "<=40", NA, NA, NA, NA, NA, ">40", "<=40"),
    sex = c(NA, NA, NA, "F", "M", NA, NA, NA, "F", "M"),
    region = c(NA, NA, NA, NA, NA, "North", "South", "Center", NA, NA)
  )

  expect_no_error(res0 <- uniteNameLevelInternal(tib,
                                                 c("age", "sex", "region"),
                                                 keep = FALSE))
  expect_equal(colnames(res0),
               c("group_name", "group_level"))
  expect_true(res0[1,1] == "overall")
  expect_true(res0[1,2] == "overall")
  expect_equal(res0$group_name[grepl("40", res0$group_level)] |> unique(),
               c("age", "age &&& sex"))
  expect_equal(res0$group_name[grepl("region", res0$group_name)] |> unique(),
               "region")

  expect_no_error(res1 <- uniteNameLevelInternal(tib,
                                                 c("age", "sex", "region"),
                                                 keep = TRUE))
  expect_equal(colnames(res1),
               c("age", "sex", "region", "group_name", "group_level"))
  expect_true(all(is.na(res1[1,1:3])))

  expect_no_error(res2 <- uniteNameLevelInternal(tib,
                                                 c("age", "sex", "region"),
                                                 keep = FALSE,
                                                 ignore = character()))

  expect_true(res2$group_level[1] == "NA &&& NA &&& NA")
  expect_true(res2$group_level[2] == ">40 &&& NA &&& NA")

  res3 <- tib |> uniteStrata(cols = character())
  expect_true(all(c("strata_name", "strata_level") %in% colnames(res3)))
  expect_true(all(res3$strata_name == "overall"))
  expect_identical(res3$strata_name, res3$strata_level)

  res4 <- uniteGroup(tib, cols = c("age", "sex", "region"))
  expect_identical(res0, res4)

  # grouped input table
  expect_warning(res5 <- uniteAdditional(tib |> dplyr::group_by(age), cols = c("region")))
  expect_warning(res5 <- uniteStrata(tib |> dplyr::group_by(age), cols = c("age")))
})

test_that("test unitNameLevelInternal", {
  x <- mockSummarisedResult()
  expect_error(uniteNameLevelInternal(x, name = "group_name", level = "group_name"))
  expect_error(uniteNameLevelInternal(x, name = "group_name", level = "group_name"))
})
