test_that("test validateNameArgument", {
  expect_error(validateNameArgument(name = 1))
  expect_error(validateNameArgument(name = c("sda", "asdfsa")))
  expect_identical("my_name", validateNameArgument("my_name"))
  expect_error(validateNameArgument("myName"))
  expect_warning(expect_identical("my_name", validateNameArgument("myName", validation = "warning")))
  expect_warning(expect_warning(expect_identical(
    "my_name", validateNameArgument("myName", list("my_name" = 1), validation = "warning")
  )))
  expect_no_error(validateNameArgument(name = NULL, null = TRUE))
  expect_error(validateNameArgument(name = NULL, null = FALSE))
  expect_identical(validateNameArgument(character()), character())
  expect_error(validateNameArgument(character(), empty = FALSE))
})

test_that("validateConceptSetArgument checks codelists against cdm concept table", {
  cdm <- mockCdmWithConcept(c(1L, 2L, 3L))

  expect_no_error(validateConceptSetArgument(list(disease = c(1L, 2L)), cdm = cdm))
  expect_warning(
    validateConceptSetArgument(list(disease = c(1L, 4L)), cdm = cdm),
    "not present in `cdm\\$concept`"
  )
})

test_that("test validateCohortIdArgument", {
  # toy cohort
  cohort <- dplyr::tibble(
    cohort_definition_id = 1:4L, subject_id = 1L, cohort_start_date = Sys.Date(),
    cohort_end_date = Sys.Date()
  ) |>
    addClass(c("cohort_table", "cdm_table"))
  attr(cohort, "cohort_set") <- dplyr::tibble(
    "cohort_definition_id" = c(1L, 2L, 3L, 4L),
    "cohort_name" = c("cohort_a", "acetaminophen", "paracetamol", "cohort_ol")
  )

  # numeric behavior
  expect_identical(validateCohortIdArgument(2, cohort), 2L)
  expect_identical(validateCohortIdArgument(c(2, 4), cohort), c(2L, 4L))
  expect_identical(validateCohortIdArgument(c(4L, 2L), cohort), c(4L, 2L))
  expect_error(validateCohortIdArgument(5, cohort))
  expect_warning(expect_identical(
    validateCohortIdArgument(c(2, 8), cohort, validation = "warning"),
    2L
  ))
  expect_warning(expect_identical(
    validateCohortIdArgument(5, cohort, validation = "warning"),
    integer()
  ))

  # character behavior
  expect_identical(validateCohortIdArgument("acetaminophen", cohort), 2L)
  expect_identical(
    validateCohortIdArgument(c("acetaminophen", "paracetamol"), cohort),
    c(2L, 3L)
  )
  expect_identical(
    validateCohortIdArgument(c("paracetamol", "acetaminophen"), cohort),
    c(3L, 2L)
  )
  expect_error(validateCohortIdArgument(c("not_present"), cohort))
  expect_warning(expect_identical(
    validateCohortIdArgument(
      c("paracetamol", "not_present"), cohort,
      validation = "warning"
    ),
    3L
  ))
  expect_warning(expect_identical(
    validateCohortIdArgument(c("not_present"), cohort, validation = "warning"),
    integer()
  ))

  # tidyselect behavior
  expect_identical(
    validateCohortIdArgument(dplyr::starts_with("cohort_"), cohort),
    c(1L, 4L)
  )
  expect_identical(
    validateCohortIdArgument(!dplyr::starts_with("cohort_"), cohort),
    c(2L, 3L)
  )
  expect_identical(
    validateCohortIdArgument(!"cohort_a", cohort),
    c(2L, 3L, 4L)
  )
  expect_identical(
    validateCohortIdArgument(dplyr::ends_with("ol"), cohort),
    c(3L, 4L)
  )
  expect_identical(
    validateCohortIdArgument(dplyr::everything(), cohort),
    c(1L, 2L, 3L, 4L)
  )
  expect_identical(
    validateCohortIdArgument(dplyr::any_of(c("sdfghjk", "dfg")), cohort),
    integer()
  )
  expect_error(
    validateCohortIdArgument(
      dplyr::any_of(c("sdfghjk", "dfg")), cohort, empty = FALSE
    )
  )
  expect_warning(expect_identical(
    validateCohortIdArgument(
      dplyr::any_of(c("sdfghjk", "dfg")), cohort,
      validation = "warning", empty = FALSE
    ),
    integer()
  ))

  # NULL
  expect_identical(validateCohortIdArgument(NULL, cohort), c(1L, 2L, 3L, 4L))

  # error if anything else is provided
  expect_error(validateCohortIdArgument(list(), cohort))
  expect_error(validateCohortIdArgument(list(), cohort, validation = "warning"))

  # check in external function
  filterCohort <- function(x, id) {
    id <- validateCohortIdArgument({{ id }}, x)
    x |>
      dplyr::filter(.data$cohort_definition_id %in% .env$id)
  }
  expect_no_error(filterCohort(cohort, dplyr::starts_with("cohort")))
})

test_that("test validateWindowArgument", {
  window <- c(0, 1)
  expect_no_error(validateWindowArgument(window))
  window <- list(c(0, 1), c(2, 3))
  expect_no_error(validateWindowArgument(window))
  window <- list(c(0, 15.5), c(16, 30))
  expect_error(validateWindowArgument(window))
  expect_no_error(validateWindowArgument(window, integerish = FALSE))
  window <- list(c("a", 1))
  expect_error(validateWindowArgument(window))
  window <- list("window" = c(0, 1), "window2" = c(-1, 1))
  expect_no_error(validateWindowArgument(window))
  window <- list(c(0, -1))
  expect_error(validateWindowArgument(window))
  window <- list(c(-Inf, -Inf))
  expect_error(validateWindowArgument(window))
  window <- list(c(Inf, Inf))
  expect_error(validateWindowArgument(window))

  # window name check
  window <- list(c(-1, 1))

  window <- window |> validateWindowArgument(snakeCase = FALSE)

  expect_true(names(window) == "-1 to 1")

  window <- list(c(-1, 1))

  window <- window |> validateWindowArgument(snakeCase = TRUE)

  expect_true(names(window) == "m1_to_1")


  window <- list("window" = c(-1, 1))
  window <- window |> validateWindowArgument(snakeCase = TRUE)
  expect_true(names(window) == "window")

  window <- list("-Inf to -1" = c(-Inf, -1))
  expect_no_error(window2 <- validateWindowArgument(window, snakeCase = TRUE))
  expect_identical(names(window2), toSnakeCase(names(window)))
  expect_no_error(window3 <- validateWindowArgument(window, snakeCase = FALSE))
  expect_identical(names(window3), names(window))
  expect_identical(validateWindowArgument(numeric()), list())
  expect_error(validateWindowArgument(numeric(), empty = FALSE))
})

test_that("test validateAgeGroup", {
  # test list
  expect_error(validateAgeGroupArgument(c("1", "18")))
  ageGroup <- list(c(0, 18))
  expect_no_error(validateAgeGroupArgument(ageGroup))

  # test name
  ageGroup <- validateAgeGroupArgument(ageGroup)
  expect_true(names(ageGroup) == "age_group")

  # name multiple group
  ageGroup <- list(list(c(0, 19)), list(c(0, 18)))
  ageGroup <- validateAgeGroupArgument(ageGroup)
  expect_true(all(names(ageGroup) == c("age_group_1", "age_group_2")))

  # test overlap
  ageGroup <- list(c(0, 18), c(16, 20))
  expect_error(validateAgeGroupArgument(ageGroup, overlap = FALSE))
  expect_no_error(validateAgeGroupArgument(ageGroup, overlap = TRUE))

  # test order
  ageGroup <- list(c(19, 18), c(21, 20))
  expect_error(validateAgeGroupArgument(ageGroup, overlap = FALSE))

  # test multiple age group
  ageGroup <- list(list(c(0, 19)), list(c(0, 18)))
  expect_error(validateAgeGroupArgument(
    ageGroup,
    overlap = FALSE, multipleAgeGroup = FALSE
  ))
  expect_no_error(validateAgeGroupArgument(
    ageGroup,
    overlap = FALSE, multipleAgeGroup = TRUE
  ))

  # null age group
  expect_no_error(validateAgeGroupArgument(
    ageGroup = NULL, overlap = FALSE, multipleAgeGroup = FALSE
  ))
  expect_error(validateAgeGroupArgument(
    ageGroup = NULL, overlap = FALSE, null = FALSE, multipleAgeGroup = FALSE
  ))
  expect_identical(validateAgeGroupArgument(numeric()), NULL)
  expect_error(validateAgeGroupArgument(numeric(), empty = FALSE))

  # correct naming
  x <- list(0, c(1, 19), c(20, 39), c(40, 59), c(60, 79), c(80, Inf)) |>
    validateAgeGroupArgument()
  expect_identical(
    x,
    list("age_group" = list(
      "0 to 0" = c(0, 0), "1 to 19" = c(1, 19), "20 to 39" = c(20, 39),
      "40 to 59" = c(40, 59), "60 to 79" = c(60, 79), "80 or above" = c(80, Inf)
    ))
  )
  expect_identical(
    validateAgeGroupArgument(c(0, Inf), ageGroupName = "my_age_group"),
    list(my_age_group = list(overall = c(0L, Inf)))
  )
})

test_that("test validateCdmArgument", {
  cdm_object <- 1
  class(cdm_object) <- c("cdm_reference")
  expect_no_error(
    validateCdmArgument(
      cdm_object,
      checkOverlapObservation = FALSE,
      checkStartBeforeEndObservation = FALSE
    )
  )
  expect_error(
    validateCdmArgument(
      cdm_object,
      checkOverlapObservation = TRUE,
      checkStartBeforeEndObservation = TRUE
    )
  )

  cdm_object <- list(
    "observation_period" = dplyr::tibble(
      observation_period_id = 1L, person_id = 1L,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2023-12-31"),
      period_type_concept_id = 0L
    )
  )

  class(cdm_object) <- c("cdm_reference")

  expect_no_error(
    validateCdmArgument(
      cdm_object,
      checkOverlapObservation = TRUE,
      checkStartBeforeEndObservation = TRUE
    )
  )

  cdm_object <- list(
    "observation_period" = dplyr::tibble(
      observation_period_id = 1L, person_id = 1L,
      observation_period_start_date = as.Date("2024-01-01"),
      observation_period_end_date = as.Date("2023-12-31"),
      period_type_concept_id = 0L
    )
  )

  class(cdm_object) <- c("cdm_reference")

  expect_error(
    validateCdmArgument(
      cdm_object,
      checkStartBeforeEndObservation = TRUE
    )
  )


  cdm_object <- list(
    "observation_period" = dplyr::tibble(
      observation_period_id = c(1L, 1L), person_id = c(1L, 1L),
      observation_period_start_date = c(as.Date("2000-01-01"), as.Date("2000-01-01")),
      observation_period_end_date = c(as.Date("2023-12-31"), as.Date("2023-01-01")),
      period_type_concept_id = c(0L, 0L)
    )
  )
  class(cdm_object) <- c("cdm_reference")

  expect_error(
    validateCdmArgument(
      cdm_object,
      checkOverlapObservation = TRUE,
      checkStartBeforeEndObservation = TRUE
    )
  )

  expect_no_error(
    validateCdmArgument(
      cdm_object,
      checkOverlapObservation = FALSE,
      checkStartBeforeEndObservation = FALSE
    )
  )

  # implausible starting observation date
  cdm_object <- list(
    "observation_period" = dplyr::tibble(
      observation_period_id = c(1L, 1L), person_id = c(1L, 1L),
      observation_period_start_date = c(as.Date("1700-01-01"), as.Date("2000-01-01")),
      observation_period_end_date = c(as.Date("2000-01-01"), as.Date("2023-01-01")),
      period_type_concept_id = c(0L, 0L)
    )
  )
  class(cdm_object) <- c("cdm_reference")
  expect_warning(
    validateCdmArgument(
      cdm_object,
      checkPlausibleObservationDates = TRUE
    )
  )
  expect_no_error(
    validateCdmArgument(
      cdm_object,
      checkPlausibleObservationDates = FALSE
    )
  )
  expect_no_error(validateCdmArgument(cdm_object, requiredTables = "observation_period"))
  expect_error(validateCdmArgument(cdm_object, requiredTables = "my_cohort"))
  expect_error(validateCdmArgument(cdm_object, requiredTables = c("my_cohort", "observation_period", "other_table")))

  # implausible ending observation date - currently a warning instead of e
  cdm_object <- list(
    "observation_period" = dplyr::tibble(
      observation_period_id = c(1L, 1L), person_id = c(1L, 1L),
      observation_period_start_date = c(as.Date("2000-01-01"), as.Date("2000-01-02")),
      observation_period_end_date = c(as.Date("2000-01-01"), as.Date("2100-01-01")),
      period_type_concept_id = c(0L, 0L)
    )
  )
  class(cdm_object) <- c("cdm_reference")
  expect_warning(
    validateCdmArgument(
      cdm_object,
      checkPlausibleObservationDates = TRUE
    )
  )
  expect_no_error(
    validateCdmArgument(
      cdm_object,
      checkPlausibleObservationDates = FALSE
    )
  )


  # no errors or warnings if cdm is empty
  expect_no_error(
    validateCdmArgument(
      emptyCdmReference("test"),
      checkOverlapObservation = TRUE,
      checkStartBeforeEndObservation = TRUE,
      checkPlausibleObservationDates = TRUE
    )
  )

  # warnings if clinical table contain person id not in person  table

  cdm_object <- list(
    "observation_period" = dplyr::tibble(
      observation_period_id = c(1L, 1L), person_id = c(1L, 1L),
      observation_period_start_date = c(as.Date("2000-01-01"), as.Date("2000-01-02")),
      observation_period_end_date = c(as.Date("2000-01-01"), as.Date("2100-01-01")),
      period_type_concept_id = c(0L, 0L)
    ),
    "person" = dplyr::tibble(
      person_id = c(1L, 1L),
      gender_concept_id = c(8507L, 8507L),
      year_of_birth = c(1991L, 1992L)
    ),
    "condition_occurrence" = dplyr::tibble(
      condition_occurrence_id = c(1L, 2L),
      person_id = c(8507L, 8507L),
      condition_start_date = c(as.Date("2000-01-01"), as.Date("2000-01-02")),
      condition_end_date = c(as.Date("2000-01-01"), as.Date("2100-01-01"))
    )
  )

  class(cdm_object) <- c("cdm_reference")

  expect_no_warning(
    validateCdmArgument(
      cdm_object,
      checkPerson = FALSE
    )
  )

  expect_warning(
    validateCdmArgument(
      cdm_object,
      checkPerson = TRUE
    )
  )
})

test_that("test validateResults", {
  x <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "cprd",
    "result_type" = "summarised_characteristics",
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
  )



  expect_no_error(x |>
    newSummarisedResult() |>
    validateResultArgument())
})

test_that("test isResultSuppressed", {
  obj <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "mock",
    "group_name" = "overall",
    "group_level" = "overall",
    "strata_name" = c(rep("overall", 6), rep("sex", 3)),
    "strata_level" = c(rep("overall", 6), "male", "female", "female"),
    "variable_name" = c("number records", "age_group", "age_group", "age_group", "age_group", "my_variable", "number records", "age_group", "age_group"),
    "variable_level" = c(NA, "<50", "<50", ">=50", ">=50", NA, NA, "<50", "<50"),
    "estimate_name" = c("count", "count", "percentage", "count", "percentage", "random", "count", "count", "percentage"),
    "estimate_type" = c("integer", "integer", "percentage", "integer", "percentage", "numeric", "integer", "integer", "percentage"),
    "estimate_value" = c("10", "5", "50", "3", "30", "1", "3", "12", "6"),
    "additional_name" = "overall",
    "additional_level" = "overall"
  ) |>
    newSummarisedResult(settings = dplyr::tibble(
      "result_id" = as.integer(1),
      "result_type" = "summarised_characteristics",
      "package_name" = "omopgenerics",
      "package_version" = as.character(utils::packageVersion("omopgenerics"))
    ))

  # Test for no min_cell_count column
  expect_warning(isResultSuppressed(result = obj, minCellCount = 3))

  result <- suppress(obj, minCellCount = 2)

  # Test for correctly specified min_cell_count
  expect_no_warning(isResultSuppressed(result = result, minCellCount = 2))

  # Test for greater actual min_cell_count
  expect_warning(isResultSuppressed(result = result, minCellCount = 1))

  # Test for smaller actual min_cell_count
  expect_warning(isResultSuppressed(result = result, minCellCount = 3))
})

test_that("test validateColumn", {
  x <- dplyr::tibble(a = 1, b = "xxx")
  expect_identical(validateColumn("a", x), "a")
  expect_identical(validateColumn(NULL, x, null = TRUE), NULL)
  expect_error(validateColumn(NULL, x))
  expect_identical(validateColumn(character(), x), character())
  expect_error(validateColumn(character(), x, empty = FALSE))
  expect_error(validateColumn("a", x, type = "character"))
  expect_no_error(validateColumn("a", x, type = "numeric"))
  expect_error(validateColumn("not_existing", x, type = "numeric"))
})

test_that("test validateNameStyle", {
  expect_no_error(validateNameStyle("nothing"))
  expect_no_error(validateNameStyle("prefix_{cohort_name}"))
  expect_no_error(validateNameStyle("prefix_{cohort_name}", cohortName = c("a", "b")))
  expect_no_error(validateNameStyle("prefix_{cohort_name}", cohort_name = c("a", "b")))
  expect_identical(validateNameStyle(character()), character())
  expect_error(validateNameStyle(character(), empty = FALSE))
  expect_error(validateNameStyle("prefix_{x1}_{x2}", cohort_name = c("a", "b")))
  expect_error(validateNameStyle("prefix_{x1}_{x2}", cohortName = c("a", "b")))
})

test_that("test validateStrataArgument", {
  x <- dplyr::tibble(a = 1L, b = 1L)
  expect_no_error(validateStrataArgument(list(), x))
  expect_no_error(validateStrataArgument(list(), dplyr::tibble()))
  expect_identical(validateStrataArgument(character(), x), list())
  expect_error(validateStrataArgument(list(), x, empty = FALSE))
  expect_error(validateStrataArgument(list(), dplyr::tibble(), empty = FALSE))
  expect_error(validateStrataArgument(character(), x, empty = FALSE))
  expect_no_error(validateStrataArgument(list(character()), x))
  expect_no_error(validateStrataArgument(combineStrata(c("a", "b"), TRUE), x))
  expect_no_error(validateStrataArgument(combineStrata(c("a", "b"), FALSE), x))
  expect_identical(list(c("a", "b")), validateStrataArgument(c("a", "b"), x))
  expect_error(validateStrataArgument(combineStrata(c("a", "x"), TRUE), x))
  expect_error(validateStrataArgument(combineStrata(c("a", "x", "y"), TRUE), x))
})

test_that("validate functions use custom names in errors", {
  cohort <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = Sys.Date(),
    cohort_end_date = Sys.Date()
  ) |>
    addClass(c("cohort_table", "cdm_table"))
  attr(cohort, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = 1L,
    cohort_name = "cohort_a"
  )

  conceptSet <- list(1L) |>
    addClass("codelist")
  x <- dplyr::tibble(a = 1L)

  expect_error(validateNameArgument(1, nm = "custom_name"), regexp = "custom_name")
  expect_error(validateCohortArgument(1, nm = "custom_cohort"), regexp = "custom_cohort")
  expect_error(validateCohortIdArgument(list(), cohort, nm = "custom_cohort_id"), regexp = "custom_cohort_id")
  expect_error(validateConceptSetArgument(conceptSet, nm = "custom_concept_set"), regexp = "custom_concept_set")
  expect_error(validateWindowArgument(list(c("a", 1)), nm = "custom_window"), regexp = "custom_window")
  expect_error(validateAgeGroupArgument(c("1", "18"), nm = "custom_age_group"), regexp = "custom_age_group")
  expect_error(validateCdmArgument(1, nm = "custom_cdm"), regexp = "custom_cdm")
  expect_error(validateResultArgument(1, nm = "custom_result"), regexp = "custom_result")
  expect_error(validateNewColumn(x, 1, nm = "custom_new_column"), regexp = "custom_new_column")
  expect_error(validateColumn(1, x, nm = "custom_column"), regexp = "custom_column")
  expect_error(validateNameStyle(1, nm = "custom_name_style"), regexp = "custom_name_style")
  expect_error(validateStrataArgument(1, x, nm = "custom_strata"), regexp = "custom_strata")
  expect_error(validateCdmTable(1, nm = "custom_cdm_table"), regexp = "custom_cdm_table")
  expect_error(validateOmopTable(1, nm = "custom_omop_table"), regexp = "custom_omop_table")
  expect_error(validateAchillesTable(1, nm = "custom_achilles_table"), regexp = "custom_achilles_table")
  expect_error(validateNameLevel(1, prefix = "strata", nm = "custom_name_level"), regexp = "custom_name_level")
})
