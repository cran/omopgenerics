test_that("emptyCodeSearch returns a code_search object", {
  expect_no_error(cs <- emptyCodeSearch())
  expect_s3_class(cs, "code_search")
  expect_s3_class(cs, "tbl_df")
  expect_equal(nrow(cs), 0L)

  cols <- codeSerachColumns$column[codeSerachColumns$table == "codes"]
  expect_true(all(cols %in% colnames(cs)))

  st <- searchStrategy(cs)
  expect_equal(nrow(st), 0L)
  cols <- codeSearchColumns$column[codeSearchColumns$table == "search_strategy"]
  expect_true(all(cols %in% colnames(st)))

  cs1 <- emptyCodeSearch(searchStrategy = NULL)
  expect_identical(cs, cs1)
})

test_that("newCodeSearch validates and casts code search data", {
  st <- dplyr::tibble(
    strategy_id = 1L,
    strategy_name = c("package_name", "function_name", "package_version"),
    strategy_value = c("CodelistGenerator", "search", "1.0")
  )

  expect_no_error(cs <- emptyCodeSearch(searchStrategy = st))
  expect_equal(nrow(searchStrategy(cs)), 3)
  expect_identical(searchStrategy(cs), st)

  codes_tibble <- function(n = 10) {
    dplyr::tibble(
      concept_id = as.integer(seq_len(n)),
      found_from = "Descendants",
      concept_name = paste0("Concept ", seq_len(n)),
      vocabulary_version = "v1.0",
      domain_id = "Condition",
      vocabulary_id = "SNOMED",
      concept_class_id = "Clinical Finding",
      standard_concept = "S",
      concept_code = paste0("C00", seq_len(n)),
      valid_start_date = as.Date("1970-01-01"),
      valid_end_date = as.Date("2099-12-31"),
      invalid_reason = NA_character_
    )
  }

  expect_no_error(cs <- newCodeSearch(codes = codes_tibble(), searchStrategy = st))
  expect_equal(nrow(cs), 10)
  expect_equal(nrow(searchStrategy(cs)), 3)
  expect_s3_class(cs, "code_search")
  expect_s3_class(cs, "tbl_df")
  expect_s3_class(searchStrategy(cs), "tbl_df")
  expect_identical(searchStrategy(cs), st)

  expect_output(print(cs))
  expect_output(print(emptyCodeSearch()))

  bad_codes <- dplyr::tibble(concept_id = 1L)
  expect_error(newCodeSearch(codes = bad_codes, searchStrategy = st))
  expect_error(searchStrategy(list()))
  expect_error(searchStrategy(dplyr::tibble()))
  expect_error(newCodeSearch(codes = codes_tibble(), searchStrategy = NULL))

  bad_strategy <- dplyr::tibble(strategy_name = "keyword")
  expect_error(newCodeSearch(codes = codes_tibble(), searchStrategy = bad_strategy))

  codes <- codes_tibble()
  codes$concept_id <- as.double(codes$concept_id)
  codes$valid_start_date <- as.double(codes$valid_start_date)
  cs <- newCodeSearch(codes = codes, searchStrategy = st)
  expect_type(cs$concept_id, "integer")
  expect_s3_class(cs$valid_start_date, "Date")
})

test_that("importCodeSearch and exportCodeSearch round trip code_search objects", {
  skip_if_not_installed("openxlsx")

  st <- dplyr::tibble(
    strategy_id = 1L,
    strategy_name = c("package_name", "function_name", "package_version"),
    strategy_value = c("CodelistGenerator", "search", "1.0")
  )
  codes <- dplyr::tibble(
    concept_id = as.integer(seq_len(10)),
    found_from = "Descendants",
    concept_name = paste0("Concept ", seq_len(10)),
    vocabulary_version = "v1.0",
    domain_id = "Condition",
    vocabulary_id = "SNOMED",
    concept_class_id = "Clinical Finding",
    standard_concept = "S",
    concept_code = paste0("C00", seq_len(10)),
    valid_start_date = as.Date("1970-01-01"),
    valid_end_date = as.Date("2099-12-31"),
    invalid_reason = NA_character_
  )
  cs <- newCodeSearch(codes = codes, searchStrategy = st)

  tmp <- tempdir()
  expect_no_error(exportCodeSearch(cs, file = "test_export", path = tmp))
  expect_true(file.exists(file.path(tmp, "test_export.xlsx")))
  expect_no_error(cs0 <- importCodeSearch(path = file.path(tmp, "test_export.xlsx")))
  expect_identical(cs, cs0)

  expect_no_error(exportCodeSearch(cs, file = "test_export2.xlsx", path = tmp))
  expect_true(file.exists(file.path(tmp, "test_export2.xlsx")))
  expect_no_error(cs1 <- importCodeSearch(path = file.path(tmp, "test_export2.xlsx")))
  expect_identical(cs, cs1)

  expect_error(importCodeSearch(path = "/nonexistent/path/file.xlsx"))

  bad_file <- file.path(tmp, "bad.xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "OnlyThisSheet")
  openxlsx::saveWorkbook(wb, bad_file, overwrite = TRUE)
  expect_error(importCodeSearch(path = bad_file))

  file <- file.path(tmp, "test_export2.xlsx")
  wb <- openxlsx::loadWorkbook(file = file)
  openxlsx::addWorksheet(wb = wb, sheetName = "ExtraSheet")
  openxlsx::writeData(wb = wb, sheet = "ExtraSheet", x = cars)
  openxlsx::saveWorkbook(wb = wb, file = file, overwrite = TRUE)
  expect_true(file.exists(file))
  expect_true("ExtraSheet" %in% openxlsx::getSheetNames(file))
  expect_no_error(cs2 <- importCodeSearch(path = file))
  expect_identical(cs, cs2)
})
