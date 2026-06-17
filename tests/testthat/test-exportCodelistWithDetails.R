test_that("test export codelist with details json", {
  skip_if_not_installed("jsonlite")

  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  codes <- newCodelistWithDetails(list(
    "disease X" = dplyr::tibble(
      concept_id = c(1L, 2L, 3L),
      concept_name = c("one", "two", "three"),
      domain_id = "Condition"
    ),
    "disease Y" = dplyr::tibble(
      concept_id = c(4L, 5L),
      concept_name = c("four", "five"),
      domain_id = "Drug"
    )
  ))

  expect_no_error(exportCodelistWithDetails(
    x = codes,
    path = cs_path
  ))
  expect_true("disease X.json" %in% list.files(cs_path))
  expect_true("disease Y.json" %in% list.files(cs_path))

  expect_error(exportCodelistWithDetails(
    x = "not codes",
    path = cs_path
  ))
  expect_error(exportCodelistWithDetails(
    x = codes,
    path = "not a path"
  ))

  unlink(cs_path, recursive = TRUE)
})

test_that("test export codelist with details csv", {
  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  codes <- newCodelistWithDetails(list(
    "disease X" = dplyr::tibble(
      concept_id = c(1L, 2L, 3L),
      concept_name = c("one", "two", "three"),
      domain_id = "Condition"
    ),
    "disease Y" = dplyr::tibble(
      concept_id = c(4L, 5L),
      concept_name = c("four", "five"),
      domain_id = "Drug"
    )
  ))

  expect_no_error(exportCodelistWithDetails(
    x = codes,
    path = cs_path,
    type = "csv"
  ))
  expect_true("disease X.csv" %in% list.files(cs_path))
  expect_true("disease Y.csv" %in% list.files(cs_path))

  unlink(cs_path, recursive = TRUE)
})
