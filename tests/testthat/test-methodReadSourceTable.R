mockSource <- function(tables) {
  env <- new.env(parent = emptyenv())
  env$tables <- tables
  structure(
    "mock", tables = env, source_type = "mock",
    class = c("mock_source", "cdm_source")
  )
}

listSourceTables.mock_source <- function(cdm) {
  names(attr(cdm, "tables")$tables)
}

readSourceTable.mock_source <- function(cdm, name) {
  newCdmTable(table = attr(cdm, "tables")$tables[[name]], src = cdm, name = name)
}

insertTable.mock_source <- function(cdm, name, table, ...) {
  attr(cdm, "tables")$tables[[name]] <- table
  newCdmTable(table = table, src = cdm, name = name)
}

registerS3method(
  "listSourceTables", "mock_source", listSourceTables.mock_source,
  envir = asNamespace("omopgenerics")
)
registerS3method(
  "readSourceTable", "mock_source", readSourceTable.mock_source,
  envir = asNamespace("omopgenerics")
)
registerS3method(
  "insertTable", "mock_source", insertTable.mock_source,
  envir = asNamespace("omopgenerics")
)

test_that("readSourceTable", {
  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L, person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2023-12-31"),
    period_type_concept_id = 0L
  )
  cohort <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-01")
  )
  cohort_set <- dplyr::tibble(
    cohort_definition_id = 1L,
    cohort_name = "cohort_1"
  )
  cohort_attrition <- dplyr::tibble(
    cohort_definition_id = 1L,
    number_records = 1L,
    number_subjects = 1L,
    reason_id = 1L,
    reason = "Qualifying initial records",
    excluded_records = 0L,
    excluded_subjects = 0L
  )
  cohort_codelist <- dplyr::tibble(
    cohort_definition_id = 1L,
    codelist_name = "cohort_1",
    concept_id = 1L,
    codelist_type = "index event"
  )
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test",
    cohortTables = list("cohort1" = cohort)
  )

  expect_warning(cdm2 <- readSourceTable(cdm = cdm, name = "my_cohort"))
  expect_identical(cdm, cdm2)

  src <- mockSource(list(
    "person" = person,
    "observation_period" = observation_period,
    "cohort1" = cohort,
    "cohort1_set" = cohort_set,
    "cohort1_attrition" = cohort_attrition,
    "cohort1_codelist" = cohort_codelist
  ))
  cdm <- newCdmReference(
    tables = list(
      "person" = newCdmTable(table = person, src = src, name = "person"),
      "observation_period" = newCdmTable(
        table = observation_period, src = src, name = "observation_period"
      )
    ),
    cdmName = "test"
  )

  expect_no_error(cdm2 <- readSourceTable(cdm = cdm, name = !"cohort1"))
  expect_identical(names(cdm2), c("person", "observation_period"))

  expect_no_error(cdm2 <- readSourceTable(cdm = cdm, name = !dplyr::starts_with("cohort")))
  expect_identical(names(cdm2), c("person", "observation_period"))

  expect_warning(cdm3 <- readSourceTable(cdm = cdm$person, name = "my_cohort"))
  expect_identical(cdm, cdm3)

  expect_no_error(cdm2 <- readSourceTable(cdm = cdm, name = dplyr::starts_with("cohort1")))
  expect_identical(names(cdm2), c("person", "observation_period", "cohort1"))
  expect_true("cohort_table" %in% class(cdm2$cohort1))
  expect_false(any(c(
    "cohort1_set", "cohort1_attrition", "cohort1_codelist"
  ) %in% names(cdm2)))
  expect_identical(tableName(attr(cdm2$cohort1, "cohort_set")), "cohort1_set")
  expect_identical(
    tableName(attr(cdm2$cohort1, "cohort_attrition")), "cohort1_attrition"
  )
  expect_identical(
    tableName(attr(cdm2$cohort1, "cohort_codelist")), "cohort1_codelist"
  )
})
