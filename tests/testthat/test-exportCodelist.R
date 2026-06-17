test_that("test export codelist json", {
  skip_if_not_installed("jsonlite")

  # single codelist
  codes <- newCodelist(list("disease" = c(1L, 2L, 3L)))
  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  expect_no_error(exportCodelist(
    x = codes,
    path = cs_path
  ))
  expect_true("disease.json" %in% list.files(cs_path))

  # multiple codelists
  codes <- list(
    "disease X" = c(1L, 2L, 3L),
    "disease Y" = c(4L, 5L)
  )
  expect_no_error(exportCodelist(
    x = codes,
    path = cs_path
  ))
  expect_true("disease X.json" %in% list.files(cs_path))
  expect_true("disease Y.json" %in% list.files(cs_path))

  # expect error
  expect_error(exportCodelist(
    x = "not codes",
    path = cs_path
  ))
  expect_error(exportCodelist(
    x = codes,
    path = "not a path"
  ))

  unlink(cs_path, recursive = TRUE)
})

test_that("test export codelist csv", {
  # single codelist
  codes <- newCodelist(list("disease" = c(1L, 2L, 3L)))
  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  expect_no_error(exportCodelist(x = codes, path = cs_path, type = "csv"))
  expect_true("disease.csv" %in% list.files(cs_path))

  # multiple codelists
  codes <- list("disease X" = c(1L, 2L, 3L), "disease Y" = c(4L, 5L))
  expect_no_error(exportCodelist(x = codes, path = cs_path, type = "csv"))
  expect_true("disease X.csv" %in% list.files(cs_path))
  expect_true("disease Y.csv" %in% list.files(cs_path))

  unlink(cs_path, recursive = TRUE)
})
