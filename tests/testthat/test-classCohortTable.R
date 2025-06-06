test_that("test create cohort", {
  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2023-12-31"),
    period_type_concept_id = 0L
  )
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test"
  )
  cdm <- insertTable(
    cdm = cdm,
    name = "cohort1",
    table = dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = 1L,
      cohort_start_date = as.Date("2020-01-01"),
      cohort_end_date = as.Date("2020-01-10")
    )
  )
  set <- defaultCohortSet(cdm$cohort1)
  attrition <- defaultCohortAttrition(cdm$cohort1, set)

  expect_error(newCohortTable(cohortSetRef = set, cohortAttritionRef = attrition))
  expect_no_error(cohort <- newCohortTable(table = cdm$cohort1, cohortSetRef = set, cohortAttritionRef = attrition))
  expect_true("cohort_table" %in% class(cohort))
  expect_true("GeneratedCohortSet" %in% class(cohort)) # to be removed
  expect_true(all(
    c("cohort_set", "cohort_attrition") %in% names(attributes(cohort))
  ))
  expect_equal(
    settings(cohort) |> unclass(),
    attr(cohort, "cohort_set") |> dplyr::collect() |> unclass()
  )
  expect_no_error(cohortCount(cohort))
  expect_equal(
    attrition(cohort) |> unclass(),
    attr(cohort, "cohort_attrition") |> dplyr::collect() |> unclass()
  )

  set$cohort_name <- "Cohort 1"
  expect_warning(x <- newCohortTable(table = cdm$cohort1, cohortSetRef = set, cohortAttritionRef = attrition))
  expect_identical(settings(x)$cohort_name, "cohort_1")

  set$cohort_name <- "name_is_toooooooooooo_laaaaaaaaargeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
  expect_warning(x <- newCohortTable(table = cdm$cohort1, cohortSetRef = set, cohortAttritionRef = attrition))
  expect_identical(settings(x)$cohort_name, substr(set$cohort_name, 1, 100))

  cdm <- insertTable(
    cdm = cdm,
    name = "cohort2",
    table = dplyr::tibble(
      cohort_definition_id = c(1, 2) |> as.integer(),
      subject_id = 1L,
      cohort_start_date = as.Date("2020-01-01"),
      cohort_end_date = as.Date("2020-01-10")
    )
  )
  set <- defaultCohortSet(cdm$cohort2)
  attrition <- defaultCohortAttrition(cdm$cohort2, set)
  expect_no_error(newCohortTable(table = cdm$cohort2, cohortSetRef = set, cohortAttritionRef = attrition))
  set$cohort_name <- "cohort_1"
  expect_error(newCohortTable(table = cdm$cohort2, cohortSetRef = set, cohortAttritionRef = attrition))

  cdm <- insertTable(
    cdm = cdm,
    name = "cohort3",
    table = dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = 1L,
      cohort_start_date = as.Date("2020-01-01"),
      cohort_end_date = as.Date("2020-01-10")
    )
  )
  set <- defaultCohortSet(cdm$cohort3)
  attrition <- defaultCohortAttrition(cdm$cohort3, set)

  expect_no_error(cohort <- newCohortTable(table = cdm$cohort3, cohortSetRef = set, cohortAttritionRef = attrition))
  expect_no_error(cohort <- newCohortTable(table = cdm$cohort3, cohortSetRef = set))
  expect_no_error(cohort <- newCohortTable(table = cdm$cohort3, cohortAttritionRef = attrition))
  expect_error(cohort <- newCohortTable(cohortSetRef = set, cohortAttritionRef = attrition))
  expect_no_error(cohort <- newCohortTable(table = cdm$cohort3, cohortSetRef = set, cohortAttritionRef = attrition))
  expect_no_error(cohort <- newCohortTable(cdm$cohort3))

  expect_true("cohort_table" %in% class(cohort))
  expect_true("GeneratedCohortSet" %in% class(cohort)) # to be removed
  expect_true(all(
    c("cohort_set", "cohort_attrition") %in% names(attributes(cohort))
  ))
  expect_equal(
    settings(cohort) |> unclass(),
    attr(cohort, "cohort_set") |> dplyr::collect() |> unclass()
  )
  expect_no_error(cohortCount(cohort))
  expect_equal(
    attrition(cohort) |> unclass(),
    attr(cohort, "cohort_attrition") |> dplyr::collect() |> unclass()
  )

  # check cohort set
  cohort_set1 <- dplyr::tibble(cohort_definition_id = 1L)
  cohort_set2 <- dplyr::tibble(cohort_definition_id = 1L, cohort_name = "hi")
  cohort_set3 <- dplyr::tibble(
    cohort_definition_id = 1L, cohort_name = "hi", rand = "random"
  )
  cohort_set4 <- dplyr::tibble(
    cohort_definition_id = c(1, 2) |> as.integer(), cohort_name = c("hi", "ha")
  )
  cohort_set5 <- dplyr::tibble(cohort_definition_id = 2L, cohort_name = c("ha"))
  expect_error(cohort1 <- newCohortTable(cdm$cohort3, cohort_set1))
  expect_no_error(cohort2 <- newCohortTable(cdm$cohort3, cohort_set2))
  expect_no_error(cohort3 <- newCohortTable(cdm$cohort3, cohort_set3))
  expect_no_error(cohort4 <- newCohortTable(cdm$cohort3, cohort_set4))
  expect_error(cohort5 <- newCohortTable(cdm$cohort3, cohort_set5))
  x <- settings(cohort2) |> as.data.frame()
  expect_equal(x, cohort_set2 |> as.data.frame())
  x <- settings(cohort3) |> as.data.frame()
  expect_equal(x, cohort_set3 |> as.data.frame())

  # check cohort attrition
  cohort_attrition1 <- dplyr::tibble(cohort_definition_id = 1L)
  cohort_attrition2 <- dplyr::tibble(
    cohort_definition_id = 1L, number_records = 2L, number_subjects = 1L,
    reason_id = 1L, reason = "a", excluded_records = 0L, excluded_subjects = 0L
  )
  cohort_attrition3 <- dplyr::tibble(
    cohort_definition_id = 1L, number_records = 2L, number_subjects = 1L,
    reason_id = 1L, reason = "a", excluded_records = 0L, excluded_subjects = 0L,
    extra_field = "random"
  )
  cohort_attrition4 <- dplyr::tibble(
    cohort_definition_id = c(1, 2) |> as.integer(), number_records = 2L,
    number_subjects = 1L,
    reason_id = 1L, reason = "a", excluded_records = 0L, excluded_subjects = 0L
  )
  cohort_attrition5 <- dplyr::tibble(
    cohort_definition_id = 2L, number_records = 2L, number_subjects = 1L,
    reason_id = 1L, reason = "a", excluded_records = 0L, excluded_subjects = 0L
  )
  expect_error(cohort1 <- newCohortTable(cdm$cohort3, cohortAttritionRef = cohort_attrition1))
  expect_no_error(cohort2 <- newCohortTable(cdm$cohort3, cohortAttritionRef = cohort_attrition2))
  expect_no_error(cohort3 <- newCohortTable(cdm$cohort3, cohortAttritionRef = cohort_attrition3))
  expect_error(cohort4 <- newCohortTable(cdm$cohort3, cohortAttritionRef = cohort_attrition4))
  expect_error(cohort5 <- newCohortTable(cdm$cohort3, cohortAttritionRef = cohort_attrition5))
  x <- attrition(cohort2) |> as.data.frame()
  expect_equal(x, cohort_attrition2 |> as.data.frame())
  x <- attrition(cohort3) |> as.data.frame()
  # extra fields in attrition are not allowed since og 0.2.0
  expect_equal(x, cohort_attrition3 |> dplyr::select(-"extra_field") |> as.data.frame())

  expect_true(is.integer(attrition(cohort2)$cohort_definition_id))
  expect_true(is.integer(attrition(cohort2)$number_records))
  expect_true(is.integer(attrition(cohort2)$number_subjects))
  expect_true(is.integer(attrition(cohort2)$reason_id))
  expect_true(is.character(attrition(cohort2)$reason))
  expect_true(is.integer(attrition(cohort2)$excluded_records))
  expect_true(is.integer(attrition(cohort2)$excluded_subjects))

  expect_equal(
    cohortCount(cohort2),
    dplyr::tibble(
      cohort_definition_id = 1L, number_records = 2L, number_subjects = 1L
    )
  )
  expect_true(is.integer(cohortCount(cohort2)$cohort_definition_id))
  expect_true(is.integer(cohortCount(cohort2)$number_records))
  expect_true(is.integer(cohortCount(cohort2)$number_subjects))

  expect_equal(
    cohortCount(cohort3),
    dplyr::tibble(
      cohort_definition_id = 1L, number_records = 2L, number_subjects = 1L
    )
  )

  # collect
  x <- dplyr::collect(cohort)
  expect_false("cdm_table" %in% class(x))
  expect_false("cdm_table" %in% attr(x, "cohort_set"))
  expect_false("cdm_table" %in% attr(x, "cohort_attrition"))
  expect_true("cohort_table" %in% class(x))

  # remove cols
  expect_no_error(validateGeneratedCohortSet(cohort3))
  attr(cohort3, "cohort_set") <- attr(cohort3, "cohort_set") |>
    dplyr::select(-"cohort_name")
  expect_error(validateGeneratedCohortSet(cohort3))

  # remove attribute
  expect_no_error(cohort2 <- newCohortTable(cdm$cohort3, cohortAttritionRef = cohort_attrition2))
  expect_no_error(validateGeneratedCohortSet(cohort2))
  attr(cohort2, "cohort_set") <- NULL
  expect_error(validateGeneratedCohortSet(cohort2))

  cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
    cohort_definition_id = 1L, subject_id = 1L,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-10")
  ))

  # no cdm_reference
  x <- cdm$cohort1
  attr(x, "cdm_reference") <- NULL
  expect_error(newCohortTable(table = x))

  # no snake name
  set <- defaultCohortSet(cdm$cohort1) |>
    dplyr::mutate("cohort_name" = "COHORT 1")
  expect_warning(
    cdm$cohort1 <- newCohortTable(table = cdm$cohort1, cohortSetRef = set)
  )
  expect_true(settings(cdm$cohort1) |> dplyr::pull("cohort_name") == "cohort_1")

  # wrong naming
  cdm <- insertTable(
    cdm = cdm, name = "cohort1_set", table = defaultCohortSet(cdm$cohort1)
  )
  expect_no_error(newCohortTable(
    table = cdm$cohort1, cohortSetRef = cdm$cohort1_set
  ))

  # empty cohort id
  expect_no_error(newCohortTable(
    table = cdm$cohort1, cohortSetRef = dplyr::tibble(
      cohort_definition_id = 1:2 |> as.integer(),
      cohort_name = c("cohort_1", "cohort_2")
    ), cohortAttritionRef = NULL
  ))

  # test NA
  cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
    cohort_definition_id = 1L, subject_id = 1L,
    cohort_start_date = as.Date(NA),
    cohort_end_date = as.Date("2020-01-10")
  ))
  expect_error(newCohortTable(table = cdm$cohort1))

  # not in observation
  cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
    cohort_definition_id = 1L, subject_id = 1L,
    cohort_start_date = as.Date("1020-01-01"),
    cohort_end_date = as.Date("1020-01-10")
  ))
  expect_error(newCohortTable(table = cdm$cohort1))

  # test overlap
  cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
    cohort_definition_id = 1L, subject_id = 1L,
    cohort_start_date = as.Date(c("2020-01-01", "2020-01-10")),
    cohort_end_date = as.Date(c("2020-01-10", "2020-01-25"))
  ))
  expect_error(newCohortTable(table = cdm$cohort1))

  # test start before end
  cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
    cohort_definition_id = 1L, subject_id = 1L,
    cohort_start_date = as.Date(c("2020-01-01")),
    cohort_end_date = as.Date(c("2019-01-10"))
  ))
  expect_error(newCohortTable(table = cdm$cohort1))
})

test_that("test validateCohortArgument", {
  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2023-12-31"),
    period_type_concept_id = 0L
  )
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test"
  )

  # test temp table
  cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
    cohort_definition_id = 1L, subject_id = 1L,
    cohort_start_date = as.Date(c("2020-01-01", "2020-01-10")),
    cohort_end_date = as.Date(c("2020-01-10", "2020-01-25"))
  ))
  cdm$cohort1 <- newCohortTable(cdm$cohort1, .softValidation = TRUE)
  expect_no_error(validateCohortArgument(cdm$cohort1, checkPermanentTable = TRUE))
  expect_no_error(validateCohortArgument(cdm$cohort1, checkPermanentTable = FALSE))
  # make it temp
  cdm$cohort1 <- cdm$cohort1 |> dplyr::compute()
  expect_error(validateCohortArgument(cdm$cohort1, checkPermanentTable = TRUE))
  expect_no_error(validateCohortArgument(cdm$cohort1, checkPermanentTable = FALSE))

  # test overlap
  cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
    cohort_definition_id = 1L, subject_id = 1L,
    cohort_start_date = as.Date(c("2020-01-01", "2020-01-10")),
    cohort_end_date = as.Date(c("2020-01-10", "2020-01-25"))
  ))
  cdm$cohort1 <- newCohortTable(cdm$cohort1, .softValidation = TRUE)
  expect_error(validateCohortArgument(cdm$cohort1, checkOverlappingEntries = TRUE))
  expect_warning(validateCohortArgument(cdm$cohort1,
    checkOverlappingEntries = TRUE,
    validation = "warning"
  ))
  expect_no_error(validateCohortArgument(cdm$cohort1, checkOverlappingEntries = FALSE))

  # test NA
  cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
    cohort_definition_id = 1L, subject_id = 1L,
    cohort_start_date = as.Date(NA),
    cohort_end_date = as.Date("2020-01-10")
  ))
  cdm$cohort1 <- newCohortTable(cdm$cohort1, .softValidation = TRUE)
  expect_error(validateCohortArgument(cdm$cohort1, checkMissingValues = TRUE))
  expect_warning(
    validateCohortArgument(cdm$cohort1,
      checkMissingValues = TRUE,
      validation = "warning"
    )
  )
  expect_no_error(validateCohortArgument(cdm$cohort1,
    checkMissingValues = FALSE,
    checkInObservation = FALSE
  ))

  # not in observation
  cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
    cohort_definition_id = 1L, subject_id = 1L,
    cohort_start_date = as.Date("1020-01-01"),
    cohort_end_date = as.Date("1020-01-10")
  ))
  cdm$cohort1 <- newCohortTable(cdm$cohort1, .softValidation = TRUE)
  expect_error(validateCohortArgument(cdm$cohort1, checkInObservation = TRUE))
  expect_warning(validateCohortArgument(cdm$cohort1,
    checkInObservation = TRUE, validation = "warning"
  ))
  expect_no_error(validateCohortArgument(cdm$cohort1,
    checkInObservation = FALSE
  ))

  # test start before end
  cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = as.Date(c("2020-01-01")),
    cohort_end_date = as.Date(c("2019-01-10"))
  ))
  cdm$cohort1 <- newCohortTable(cdm$cohort1, .softValidation = TRUE)
  expect_error(validateCohortArgument(cdm$cohort1, checkEndAfterStart = TRUE))
  expect_warning(validateCohortArgument(cdm$cohort1,
    checkEndAfterStart = TRUE, validation = "warning"
  ))
  expect_no_error(validateCohortArgument(cdm$cohort1, checkEndAfterStart = FALSE))

  # all checks switched off - runs without error
  expect_no_error(validateCohortArgument(cdm$cohort1,
    checkEndAfterStart = FALSE,
    checkOverlappingEntries = FALSE,
    checkMissingValues = FALSE,
    checkInObservation = FALSE
  ))

  expect_no_warning(validateCohortArgument(cdm$cohort1,
    checkEndAfterStart = FALSE,
    checkOverlappingEntries = FALSE,
    checkMissingValues = FALSE,
    checkInObservation = FALSE,
    validation = "warning"
  ))

  # not a cohort
  expect_error(validateCohortArgument(cdm))
  expect_error(validateCohortArgument(cdm$person))
  expect_error(validateCohortArgument("a"))

  # dropExtraColumns
  cdm$cohort2 <- cdm$cohort1 |>
    dplyr::mutate(extra_col = "") |>
    dplyr::compute(name = "cohort2", temporary = FALSE)
  expect_identical(
    cdm$cohort2, validateCohortArgument(cdm$cohort2, dropExtraColumns = FALSE)
  )
  expect_warning(expect_identical(
    cdm$cohort2 |>
      dplyr::select(-"extra_col"),
    validateCohortArgument(cdm$cohort2, dropExtraColumns = TRUE)
  ))

  # check attributes
  expect_no_error(validateCohortArgument(cdm$cohort1, checkAttributes = TRUE))

  original <- cdm$cohort1

  # check no settings
  attr(cdm$cohort1, "cohort_set") <- NULL
  expect_error(validateCohortArgument(cdm$cohort1, checkAttributes = TRUE))

  # check missing column
  cdm$cohort1 <- original
  attr(cdm$cohort1, "cohort_codelist") <- attr(cdm$cohort1, "cohort_codelist") |>
    dplyr::select(!c("codelist_name", "codelist_type"))
  expect_error(validateCohortArgument(cdm$cohort1, checkAttributes = TRUE))

  # check no cohortId in attrition
  cdm$cohort1 <- original
  attr(cdm$cohort1, "cohort_attrition") <- attr(cdm$cohort1, "cohort_attrition") |>
    dplyr::filter(.data$cohort_definition_id == 0)
  expect_error(validateCohortArgument(cdm$cohort1, checkAttributes = TRUE))
})

test_that("test error if attributes lost after class creation", {
  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2023-12-31"),
    period_type_concept_id = 0L
  )
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test"
  )

  cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
    cohort_definition_id = 1L, subject_id = 1L,
    cohort_start_date = as.Date(c("2020-01-01")),
    cohort_end_date = as.Date(c("2020-01-10"))
  ))
  cdm$cohort1 <- newCohortTable(cdm$cohort1, .softValidation = TRUE)

  expect_no_error(cdm$cohort1 |> dplyr::collect())

  # remove cohort attrition attribute
  attr(cdm$cohort1, "cohort_attrition") <- NULL
  expect_error(cdm$cohort1 |> dplyr::collect())

  # remove cohort set attribute
  attr(cdm$cohort1, "cohort_set") <- NULL
  expect_error(cdm$cohort1 |> dplyr::collect())
})

test_that("test that tables are casted", {
  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2023-12-31"),
    period_type_concept_id = 0L
  )
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test"
  )
  cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = 1,
    cohort_start_date = as.Date(c("2020-01-01")),
    cohort_end_date = as.Date(c("2020-01-10"))
  ))
  expect_no_warning(cohort1 <- newCohortTable(cdm$cohort1))
  cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = as.Date(c("2020-01-01")),
    cohort_end_date = as.Date(c("2020-01-10"))
  ))
  expect_no_warning(cohort2 <- newCohortTable(cdm$cohort1))
  expect_no_warning(cohort3 <- newCohortTable(
    cdm$cohort1,
    cohortSetRef = dplyr::tibble(
      cohort_definition_id = 1L, cohort_name = "cohort1", set = TRUE
    )
  ))
  expect_no_warning(cohort4 <- newCohortTable(
    cdm$cohort1,
    cohortSetRef = dplyr::tibble(
      cohort_definition_id = 1, cohort_name = "cohort1", set = TRUE
    )
  ))
  expect_no_warning(cohort5 <- newCohortTable(
    cdm$cohort1,
    cohortAttritionRef = dplyr::tibble(
      cohort_definition_id = 1L, number_records = 1L, number_subjects = 1L,
      reason_id = 1L, reason = "Individual of interest", excluded_records = 0L,
      excluded_subjects = 0L
    )
  ))
  expect_no_warning(cohort6 <- newCohortTable(
    cdm$cohort1,
    cohortAttritionRef = dplyr::tibble(
      cohort_definition_id = 1L, number_records = 1L, number_subjects = 1L,
      reason_id = 1L, reason = "Individual of interest", excluded_records = 0,
      excluded_subjects = 0L
    )
  ))
  expect_no_warning(cohort7 <- newCohortTable(
    cdm$cohort1,
    cohortCodelistRef = dplyr::tibble(
      cohort_definition_id = 1L, codelist_name = "covid", concept_id = 1L,
      type = "index event"
    )
  ))
  expect_warning(cohort8 <- newCohortTable(
    cdm$cohort1,
    cohortCodelistRef = dplyr::tibble(
      cohort_definition_id = 1L, codelist_name = "covid", concept_id = "1",
      type = "index event"
    )
  ))
  expect_identical(cohort7, cohort8)
})
