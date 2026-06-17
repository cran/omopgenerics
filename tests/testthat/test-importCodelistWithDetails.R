test_that("import codelist with details json", {
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

  expect_no_error(exportCodelistWithDetails(x = codes, path = cs_path))
  expect_true("disease X.json" %in% list.files(cs_path))
  expect_true("disease Y.json" %in% list.files(cs_path))

  codes_imported <- importCodelistWithDetails(path = cs_path)
  expect_identical(codes, codes_imported)

  expect_warning(
    x <- importCodelistWithDetails(path = system.file(
      package = "omopgenerics",
      "concepts_for_mock"
    ))
  )
  expect_true(length(x) == 1)
  expect_true(inherits(x, "codelist_with_details"))

  unlink(cs_path, recursive = TRUE)
})

test_that("import codelist with details from codelist json", {
  skip_if_not_installed("jsonlite")

  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  codes <- newCodelist(list(
    "disease X" = c(1L, 2L, 3L),
    "disease Y" = c(4L, 5L)
  ))

  expect_no_error(exportCodelist(x = codes, path = cs_path))
  codes_imported <- importCodelistWithDetails(path = cs_path)
  expect_true(inherits(codes_imported, "codelist_with_details"))
  expect_identical(validateConceptSetArgument(codes_imported), codes)

  unlink(cs_path, recursive = TRUE)
})

test_that("import codelist with details from concept set expression json", {
  skip_if_not_installed("jsonlite")

  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  asthma_cs <- newConceptSetExpression(list(
    "asthma_narrow" = dplyr::tibble(
      concept_id = c(1L, 2L),
      excluded = FALSE,
      descendants = FALSE,
      mapped = FALSE
    )
  ))

  expect_no_error(exportConceptSetExpression(x = asthma_cs, path = cs_path))
  codes_imported <- importCodelistWithDetails(path = cs_path)
  expect_true(inherits(codes_imported, "codelist_with_details"))
  expect_identical(
    validateConceptSetArgument(codes_imported),
    newCodelist(list("asthma_narrow" = c(1L, 2L)))
  )

  unlink(cs_path, recursive = TRUE)

  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  asthma_cs <- newConceptSetExpression(list(
    "asthma_narrow" = dplyr::tibble(
      concept_id = 1L,
      excluded = FALSE,
      descendants = TRUE,
      mapped = FALSE
    )
  ))

  expect_no_error(exportConceptSetExpression(x = asthma_cs, path = cs_path))
  expect_warning(codes_imported <- importCodelistWithDetails(path = cs_path))
  expect_identical(codes_imported, emptyCodelistWithDetails())

  unlink(cs_path, recursive = TRUE)
})

test_that("import codelist with details csv", {
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

  expect_no_error(exportCodelistWithDetails(x = codes, path = cs_path, type = "csv"))
  expect_true("disease X.csv" %in% list.files(cs_path))
  expect_true("disease Y.csv" %in% list.files(cs_path))

  codes_imported <- importCodelistWithDetails(path = cs_path, type = "csv")
  expect_identical(codes, codes_imported)

  unlink(cs_path, recursive = TRUE)
})

test_that("import codelist with details discovers csv and json by default", {
  skip_if_not_installed("jsonlite")

  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  json_codes <- newCodelistWithDetails(list(
    "json disease" = dplyr::tibble(
      concept_id = c(1L, 2L),
      concept_name = c("one", "two"),
      domain_id = "Condition"
    )
  ))
  csv_codes <- newCodelistWithDetails(list(
    "csv disease" = dplyr::tibble(
      concept_id = c(3L, 4L),
      concept_name = c("three", "four"),
      domain_id = "Drug"
    )
  ))

  expect_no_error(exportCodelistWithDetails(x = json_codes, path = cs_path))
  expect_no_error(exportCodelistWithDetails(
    x = csv_codes, path = cs_path, type = "csv"
  ))

  codes_imported <- importCodelistWithDetails(path = cs_path)
  expect_identical(
    codes_imported,
    newCodelistWithDetails(list(
      "csv disease" = dplyr::tibble(
        concept_id = c(3L, 4L),
        concept_name = c("three", "four"),
        domain_id = "Drug"
      ),
      "json disease" = dplyr::tibble(
        concept_id = c(1L, 2L),
        concept_name = c("one", "two"),
        domain_id = "Condition"
      )
    ))
  )

  unlink(cs_path, recursive = TRUE)
})

test_that("import codelist with details from codelist csv", {
  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  codes <- newCodelist(list(
    "disease X" = c(1L, 2L, 3L),
    "disease Y" = c(4L, 5L)
  ))

  expect_no_error(exportCodelist(x = codes, path = cs_path, type = "csv"))
  codes_imported <- importCodelistWithDetails(path = cs_path, type = "csv")
  expect_true(inherits(codes_imported, "codelist_with_details"))
  expect_identical(validateConceptSetArgument(codes_imported), codes)

  unlink(cs_path, recursive = TRUE)
})

test_that("import codelist with details from concept set expression csv", {
  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  asthma_cs <- newConceptSetExpression(list(
    "asthma_narrow" = dplyr::tibble(
      concept_id = c(1L, 2L),
      excluded = FALSE,
      descendants = FALSE,
      mapped = FALSE
    )
  ))

  expect_no_error(exportConceptSetExpression(x = asthma_cs, path = cs_path, type = "csv"))
  codes_imported <- importCodelistWithDetails(path = cs_path, type = "csv")
  expect_true(inherits(codes_imported, "codelist_with_details"))
  expect_identical(
    validateConceptSetArgument(codes_imported),
    newCodelist(list("asthma_narrow" = c(1L, 2L)))
  )

  unlink(cs_path, recursive = TRUE)

  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  asthma_cs <- newConceptSetExpression(list(
    "asthma_narrow" = dplyr::tibble(
      concept_id = 1L,
      excluded = FALSE,
      descendants = TRUE,
      mapped = FALSE
    )
  ))

  expect_no_error(exportConceptSetExpression(x = asthma_cs, path = cs_path, type = "csv"))
  expect_warning(codes_imported <- importCodelistWithDetails(path = cs_path, type = "csv"))
  expect_identical(codes_imported, emptyCodelistWithDetails())

  unlink(cs_path, recursive = TRUE)
})
