test_that("import codelist json", {
  skip_if_not_installed("jsonlite")

  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))

  codes <- newCodelist(list(
    "disease X" = c(1L, 2L, 3L),
    "disease Y" = c(4L, 5L)
  ))

  expect_no_error(exportCodelist(x = codes, path = cs_path))
  expect_true("disease X.json" %in% list.files(cs_path))
  expect_true("disease Y.json" %in% list.files(cs_path))

  codes_imported <- importCodelist(path = cs_path)
  expect_identical(codes, codes_imported)

  # we get an error if descendants set to TRUE
  expect_warning(
    x <- importCodelist(path = system.file(
      package = "omopgenerics",
      "concepts_for_mock"
    ))
  )
  expect_true(length(x) == 1)

  unlink(cs_path, recursive = TRUE)
})

test_that("import codelist csv", {
  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))

  codes <- newCodelist(list(
    "disease X" = c(1L, 2L, 3L), "disease Y" = c(4L, 5L)
  ))

  expect_no_error(exportCodelist(x = codes, path = cs_path, type = "csv"))
  expect_true("disease X.csv" %in% list.files(cs_path))
  expect_true("disease Y.csv" %in% list.files(cs_path))

  codes_imported <- importCodelist(path = cs_path, type = "csv")
  expect_identical(codes, codes_imported)

  unlink(cs_path, recursive = TRUE)
})
