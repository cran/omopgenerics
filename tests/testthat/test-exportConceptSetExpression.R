test_that("test export concept set expression json", {
  skip_if_not_installed("jsonlite")

  # single codelist
  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  asthma_cs <- newConceptSetExpression(list(
    "asthma_narrow" = dplyr::tibble(
      "concept_id" = 1,
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    ),
    "asthma_broad" = dplyr::tibble(
      "concept_id" = c(1, 2),
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    )
  ))

  expect_no_error(exportConceptSetExpression(
    x = asthma_cs,
    path = cs_path
  ))
  expect_true("asthma_narrow.json" %in% list.files(cs_path))
  expect_true("asthma_broad.json" %in% list.files(cs_path))

  # expect error
  expect_error(exportCodelist(
    x = "not codes",
    path = cs_path
  ))
  expect_error(exportCodelist(
    x = asthma_cs,
    path = "not a path"
  ))

  unlink(cs_path, recursive = TRUE)
})

test_that("test export concept set expression csv", {
  # single codelist
  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  asthma_cs <- newConceptSetExpression(list(
    "asthma_narrow" = dplyr::tibble(
      "concept_id" = 1,
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    ),
    "asthma_broad" = dplyr::tibble(
      "concept_id" = c(1, 2),
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    )
  ))

  expect_no_error(exportConceptSetExpression(
    x = asthma_cs, path = cs_path, type = "csv"
  ))
  expect_true("asthma_narrow.csv" %in% list.files(cs_path))
  expect_true("asthma_broad.csv" %in% list.files(cs_path))

  unlink(cs_path, recursive = TRUE)
})
