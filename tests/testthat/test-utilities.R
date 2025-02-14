test_that("test getCohortName and getCohortId", {
  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = Sys.Date(),
    period_type_concept_id = 0L
  )
  x <- dplyr::tibble(
    cohort_definition_id = 1L, subject_id = 1L, cohort_start_date = Sys.Date(),
    cohort_end_date = Sys.Date()
  )
  attr(x, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 3, 4) |> as.integer(),
    cohort_name = c("condition1", "drug1", "covid", "asthma")
  )
  y <- dplyr::tibble(
    cohort_definition_id = 1:5, subject_id = 1L, cohort_start_date = Sys.Date(),
    cohort_end_date = Sys.Date()
  )
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cohortTables = list("my_first_cohort" = x, "my_second_cohort" = y),
    cdmName = "test"
  )

  expect_identical(
    getCohortId(cdm$my_first_cohort),
    c("condition1" = 1L, "drug1" = 2L, "covid" = 3L, "asthma" = 4L)
  )

  expect_identical(
    getCohortId(cdm$my_first_cohort, "drug1"), c("drug1" = 2L)
  )
  expect_identical(
    getCohortId(cdm$my_first_cohort, c("asthma", "covid")),
    c(asthma = 4L, covid = 3L)
  )
  expect_identical(
    getCohortId(cdm$my_first_cohort, c("covid", "asthma")),
    c(covid = 3L, asthma = 4L)
  )
  expect_warning(expect_identical(
    getCohortId(cdm$my_first_cohort, c("covid", "random", "asthma")),
    c(covid = 3L, asthma = 4L)
  ))
  expect_warning(getCohortId(cdm$my_first_cohort, "random"))

  expect_identical(
    getCohortName(cdm$my_first_cohort),
    c("1" = "condition1", "2" = "drug1", "3" = "covid", "4" = "asthma")
  )
  expect_identical(
    getCohortName(cdm$my_second_cohort, 1), c("1" = "cohort_1")
  )
  expect_identical(
    getCohortName(cdm$my_second_cohort, 1L), c("1" = "cohort_1")
  )
  expect_identical(
    getCohortName(cdm$my_second_cohort, c(4, 2)),
    c("4" = "cohort_4", "2" = "cohort_2")
  )
  expect_warning(expect_identical(
    getCohortName(cdm$my_second_cohort, c(6:1)),
    c(
      "5" = "cohort_5", "4" = "cohort_4", "3" = "cohort_3", "2" = "cohort_2",
      "1" = "cohort_1"
    )
  ))
  expect_warning(getCohortName(cdm$my_second_cohort, 8))
  expect_error(getCohortName(cdm$my_second_cohort, "1"))
})

test_that("test getPersonIdentifier", {
  expect_identical(
    getPersonIdentifier(dplyr::tibble("person_id" = 1L, "a" = "a", "sdas" = 3)),
    "person_id"
  )
  expect_identical(
    getPersonIdentifier(dplyr::tibble("subject_id" = 1L, "a" = "a")),
    "subject_id"
  )
  expect_error(
    getPersonIdentifier(dplyr::tibble("subject_id" = 1L, "person_id" = "a"))
  )
  expect_error(getPersonIdentifier(dplyr::tibble("y" = 1L, "x" = "a")))
})

test_that("uniqueId", {
  expect_no_error(uniqueId())
  expect_true(is.character(uniqueId()))
  expect_true(length(uniqueId()) == 1)
  expect_true(length(uniqueId(n = 8)) == 8)
  expect_true(grepl("pref_", uniqueId(prefix = "pref_")))
  expect_true(nchar(uniqueId()) == 6)
  expect_true(nchar(uniqueId(nChar = 4, prefix = "")) == 4)
  xx <- letters[c(-1, -8)]
  expect_identical(
    uniqueId(n = 2, exclude = paste0("m", xx), nChar = 1, prefix = "m"),
    c("ma", "mh")
  )
})

test_that("isTableEmpty", {
  table <- dplyr::tibble(a = "1")

  expect_error(table |> isTableEmpty())

  class(table) <- c("cdm_table", "tbl_df", "tbl", "data.frame")

  expect_false(table |> isTableEmpty())

  table <- dplyr::tibble()

  class(table) <- c("cdm_table", "tbl_df", "tbl", "data.frame")

  expect_true(table |> isTableEmpty())
})

test_that("omopTableFields", {
  expect_no_error(omopTableFields())

  expect_identical(omopTableFields(), omopTableFields("5.3"))

  expect_false(omopTableFields(cdmVersion = "5.4") |> nrow() ==
    omopTableFields(cdmVersion = "5.3") |> nrow())

  expect_error(omopTableFields(cdmVersion = "5.5"))
})

test_that("omop column names", {
  expect_error(omopColumns("observation_periodss"))

  # date
  expect_identical(omopColumns("observation_period", "start_date"), "observation_period_start_date")
  expect_identical(omopColumns("observation_period", "end_date"), "observation_period_end_date")
  expect_identical(omopColumns("procedure_occurrence", "start_date"), "procedure_date")
  expect_identical(omopColumns("procedure_occurrence", "end_date"), "procedure_date")

  # standard concept
  expect_identical(omopColumns("device_exposure", "standard_concept"), "device_concept_id")
  expect_identical(omopColumns("observation_period", "standard_concept"), NA_character_)

  # source_concept
  expect_identical(omopColumns("device_exposure", "source_concept"), "device_source_concept_id")
  expect_identical(omopColumns("observation_period", "source_concept"), NA_character_)

  # type concept
  expect_identical(omopColumns("observation_period", "type_concept"), "period_type_concept_id")
  expect_identical(omopColumns("condition_occurrence", "type_concept"), "condition_type_concept_id")

  # unique id
  expect_identical(omopColumns("observation_period", "unique_id"), "observation_period_id")
  expect_identical(omopColumns("condition_occurrence", "unique_id"), "condition_occurrence_id")

  # domain_id
  expect_identical(omopColumns("measurement", "domain_id"), "measurement")
  expect_identical(omopColumns("condition_occurrence", "domain_id"), "condition")
  expect_identical(omopColumns("drug_exposure", "domain_id"), "drug")
  expect_identical(omopColumns("observation", "domain_id"), "observation")
})

test_that("resultPackageVersion", {
  x <- dplyr::tibble(
    "result_id" = c(1L, 2L),
    "cdm_name" = c("cprd", "omock"),
    "result_type" = c("sc1", "sc2"),
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = "Age group",
    "variable_level" = "10 to 50",
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  ) |>
    newSummarisedResult()

  expect_invisible(x |> resultPackageVersion())

  x <- dplyr::tibble(
    "result_id" = c(as.integer(1), as.integer(2)),
    "cdm_name" = c("cprd", "omock"),
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = c("0.4.0", "0.5.0"),
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = "Age group",
    "variable_level" = "10 to 50",
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  ) |> newSummarisedResult()

  expect_warning(expect_message(x |> resultPackageVersion()))
})

test_that("packages versions", {
  x <- emptySummarisedResult(settings = dplyr::tibble(
    result_id = 1:5,
    result_type = c("rt1", "rt2", "rt3", "rt4", "rt5"),
    package_name = c(
      "PatientProfiles", "CohortCharacteristics", "visOmopResults",
      "PatientProfiles", "CohortCharacteristics"
    ),
    package_version = c("1.2.0", "0.2.2", "0.3.0", "1.2.0", "0.2.2")
  ))
  expect_snapshot(resultPackageVersion(x))

  x <- emptySummarisedResult(settings = dplyr::tibble(
    result_id = 1:5,
    result_type = c("rt1", "rt2", "rt3", "rt4", "rt5"),
    package_name = c(
      "PatientProfiles", "CohortCharacteristics", "visOmopResults",
      "PatientProfiles", "CohortCharacteristics"
    ),
    package_version = c("1.2.0", "0.2.2", "0.3.0", "1.1.0", "0.2.2")
  ))
  expect_snapshot(resultPackageVersion(x))
})

test_that("toSnakeCase", {

  valid_part <- "Hello, world!"  # Valid UTF-8 string

  invalid_part <- rawToChar(as.raw(c(0x80, 0x81, 0x82)))  # Invalid UTF-8 bytes

  invalid_string <- paste0(valid_part, invalid_part)

  expect_no_error(toSnakeCase(invalid_string))

})

test_that("test numberRecords numberSubjects", {
  person <- dplyr::tibble(
    person_id = 1:5L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1:6L,
    person_id = c(1:5L, 1L),
    observation_period_start_date = as.Date(c(rep("2000-01-01", 5), "2020-01-01")),
    observation_period_end_date = as.Date(c(rep("2010-01-01", 5), "2022-01-01")),
    period_type_concept_id = 0L
  )
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test"
  )

  expect_identical(numberRecords(cdm$observation_period), 6L)
  expect_identical(numberSubjects(cdm$observation_period), 5L)
  x <- cdm$observation_period |>
    dplyr::group_by(.data$person_id) |>
    dplyr::filter(.data$observation_period_id == min(.data$observation_period_id, na.rm = TRUE))
  expect_identical(numberRecords(x), 5L)
  expect_identical(numberSubjects(x), 5L)

  x <- cdm$observation_period |>
    dplyr::filter(.data$observation_period_id == 0) |>
    dplyr::compute(name = "observation_period", temporary = FALSE)
  expect_identical(numberRecords(x), 0L)
  expect_identical(numberSubjects(x), 0L)
})
