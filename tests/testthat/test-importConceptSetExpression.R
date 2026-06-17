test_that("import concept set expression json", {
  skip_if_not_installed("jsonlite")

  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  asthma_cs <- newConceptSetExpression(list(
    "asthma_narrow" = dplyr::tibble(
      "concept_id" = 1L,
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    ),
    "asthma_broad" = dplyr::tibble(
      "concept_id" = c(1L, 2L),
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    )
  ))
  expect_no_error(exportConceptSetExpression(
    x = asthma_cs,
    path = cs_path
  ))
  expect_true("asthma_broad.json" %in% list.files(cs_path))
  expect_true("asthma_narrow.json" %in% list.files(cs_path))
  expect_false("asthma_broad.csv" %in% list.files(cs_path))
  expect_false("asthma_narrow.csv" %in% list.files(cs_path))

  codes_imported <- importConceptSetExpression(path = cs_path)
  expect_identical(asthma_cs, codes_imported)

  # example concept sets
  x <- importConceptSetExpression(path = system.file(
    package = "omopgenerics",
    "concepts_for_mock"
  ))
  expect_true(names(x[1]) == "oa_desc")
  expect_true(x[[1]]$excluded == FALSE)
  expect_true(x[[1]]$descendants == TRUE)
  expect_true(x[[1]]$mapped == FALSE)

  expect_true(names(x[2]) == "oa_no_desc")
  expect_true(x[[2]]$excluded == FALSE)
  expect_true(x[[2]]$descendants == FALSE)
  expect_true(x[[2]]$mapped == FALSE)

  # cohort jsons - won't work
  expect_warning(expect_warning(
    x <- importConceptSetExpression(path = system.file(
      package = "omopgenerics",
      "cohorts_for_mock"
    ))
  ))
  expect_true(length(x) == 0)

  # file with both cohorts and concept set jsons
  expect_warning(x <- importConceptSetExpression(path = system.file(
    package = "omopgenerics",
    "not_all_concept_sets"
  )))
  # 2 of the three were concept sets
  expect_true(length(x) == 2)

  unlink(cs_path, recursive = TRUE)
})

test_that("import concept set expression from codelist with details json", {
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
  codes_imported <- importConceptSetExpression(path = cs_path)
  expect_identical(
    codes_imported,
    newConceptSetExpression(validateConceptSetArgument(codes))
  )

  unlink(cs_path, recursive = TRUE)
})

test_that("import concept set expression from codelist json", {
  skip_if_not_installed("jsonlite")

  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  codes <- newCodelist(list(
    "disease X" = c(1L, 2L, 3L),
    "disease Y" = c(4L, 5L)
  ))

  expect_no_error(exportCodelist(x = codes, path = cs_path))
  codes_imported <- importConceptSetExpression(path = cs_path)
  expect_identical(codes_imported, newConceptSetExpression(codes))

  unlink(cs_path, recursive = TRUE)
})

test_that("import concept set expression csv", {
  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  asthma_cs <- newConceptSetExpression(list(
    "asthma_narrow" = dplyr::tibble(
      "concept_id" = 1L,
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    ),
    "asthma_broad" = dplyr::tibble(
      "concept_id" = c(1L, 2L),
      "excluded" = FALSE,
      "descendants" = TRUE,
      "mapped" = FALSE
    )
  ))
  expect_no_error(exportConceptSetExpression(
    x = asthma_cs, path = cs_path, type = "csv"
  ))
  expect_true("asthma_broad.csv" %in% list.files(cs_path))
  expect_true("asthma_narrow.csv" %in% list.files(cs_path))
  expect_false("asthma_broad.json" %in% list.files(cs_path))
  expect_false("asthma_narrow.json" %in% list.files(cs_path))

  codes_imported <- importConceptSetExpression(path = cs_path, type = "csv")
  expect_identical(asthma_cs, codes_imported)

  # example concept sets
  x <- importConceptSetExpression(path = system.file(
    package = "omopgenerics",
    "concepts_for_mock"
  ))
  expect_true(names(x[1]) == "oa_desc")
  expect_true(x[[1]]$excluded == FALSE)
  expect_true(x[[1]]$descendants == TRUE)
  expect_true(x[[1]]$mapped == FALSE)

  expect_true(names(x[2]) == "oa_no_desc")
  expect_true(x[[2]]$excluded == FALSE)
  expect_true(x[[2]]$descendants == FALSE)
  expect_true(x[[2]]$mapped == FALSE)

  # cohort jsons - won't work
  expect_warning(expect_warning(
    x <- importConceptSetExpression(path = system.file(
      package = "omopgenerics",
      "cohorts_for_mock"
    ))
  ))
  expect_true(length(x) == 0)

  # file with both cohorts and concept set jsons
  expect_warning(x <- importConceptSetExpression(path = system.file(
    package = "omopgenerics",
    "not_all_concept_sets"
  )))
  # 2 of the three were concept sets
  expect_true(length(x) == 2)

  unlink(cs_path, recursive = TRUE)
})

test_that("import concept set expression discovers csv and json by default", {
  skip_if_not_installed("jsonlite")

  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  json_cs <- newConceptSetExpression(list(
    "json_asthma" = dplyr::tibble(
      concept_id = 1L,
      excluded = FALSE,
      descendants = TRUE,
      mapped = FALSE
    )
  ))
  csv_cs <- newConceptSetExpression(list(
    "csv_asthma" = dplyr::tibble(
      concept_id = c(2L, 3L),
      excluded = FALSE,
      descendants = FALSE,
      mapped = TRUE
    )
  ))

  expect_no_error(exportConceptSetExpression(x = json_cs, path = cs_path))
  expect_no_error(exportConceptSetExpression(
    x = csv_cs, path = cs_path, type = "csv"
  ))

  codes_imported <- importConceptSetExpression(path = cs_path)
  expect_identical(
    codes_imported,
    newConceptSetExpression(list(
      "csv_asthma" = dplyr::tibble(
        concept_id = c(2L, 3L),
        excluded = FALSE,
        descendants = FALSE,
        mapped = TRUE
      ),
      "json_asthma" = dplyr::tibble(
        concept_id = 1L,
        excluded = FALSE,
        descendants = TRUE,
        mapped = FALSE
      )
    ))
  )

  unlink(cs_path, recursive = TRUE)
})

test_that("import concept set expression from codelist with details csv", {
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
  codes_imported <- importConceptSetExpression(path = cs_path, type = "csv")
  expect_identical(
    codes_imported,
    newConceptSetExpression(validateConceptSetArgument(codes))
  )

  unlink(cs_path, recursive = TRUE)
})

test_that("import concept set expression from codelist csv", {
  dir.create(cs_path <- file.path(tempdir(), uniqueTableName()))
  codes <- newCodelist(list(
    "disease X" = c(1L, 2L, 3L),
    "disease Y" = c(4L, 5L)
  ))

  expect_no_error(exportCodelist(x = codes, path = cs_path, type = "csv"))
  codes_imported <- importConceptSetExpression(path = cs_path, type = "csv")
  expect_identical(codes_imported, newConceptSetExpression(codes))

  unlink(cs_path, recursive = TRUE)
})
