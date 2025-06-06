test_that("test codelist from cohort", {
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
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 2) |> as.integer(),
    subject_id = c(1, 1) |> as.integer(),
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-10")
  )
  cdm <- cdmFromTables(
    tables = list(
      "person" = person,
      "observation_period" = observation_period,
      "cohort1" = cohort1
    ),
    cdmName = "test"
  )
  cdm$cohort1 <- newCohortTable(table = cdm$cohort1)
  # empty by default
  expect_warning(cohortCodelist(cdm$cohort1, cohortId = 1))
  expect_warning(cohortCodelist(cdm$cohort1,
    cohortId = 1,
    type = "index event"
  ))

  # with attribute added
  cdm$cohort1 <- newCohortTable(
    table = cdm$cohort1,
    cohortCodelistRef = dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2, 2) |> as.integer(),
      codelist_name = c(
        "disease X", "disease X", "disease X",
        "disease Y", "disease Y"
      ),
      concept_id = c(1, 2, 3, 4, 5) |> as.integer(),
      type = rep("index event", 5)
    )
  )

  # only works for a specific cohort definition id
  codes_used_1 <- cohortCodelist(cdm$cohort1,
    cohortId = 1,
    type = "index event"
  )
  expect_true("codelist" %in% class(codes_used_1))
  expect_equal(
    newCodelist(list("disease X" = c(1L, 2L, 3L))),
    codes_used_1
  )

  expect_warning(cohortCodelist(cdm$cohort1,
    cohortId = 1,
    type = "exit criteria"
  )) # none with this type
  expect_error(cohortCodelist(cdm$cohort1,
    cohortId = 1,
    type = "another criteria"
  ))

  codes_used_2 <- cohortCodelist(cdm$cohort1, cohortId = 2)
  expect_true("codelist" %in% class(codes_used_2))
  expect_equal(
    newCodelist(list("disease Y" = c(4L, 5L))),
    codes_used_2
  )

  # only one id allowed
  expect_error(cohortCodelist(cdm$cohort1, cohortId = c(1, 2)))
  # error as none of the cohorts is present
  expect_error(cohortCodelist(cdm$cohort1, cohortId = 3))
})

test_that("test epected error cohort_codelist in wrong format", {
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
    cohort_definition_id = c(1, 1, 1, 2) |> as.integer(),
    subject_id = 1L,
    cohort_start_date = as.Date(c(
      "2020-01-01", "2021-01-01", "2022-01-01", "2022-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-01-01", "2021-01-01", "2022-01-01", "2022-01-01"
    ))
  )
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "my_example_cdm",
    cohortTables = list("cohort1" = cohort)
  )
  expect_error(cdm$cohort1 <- newCohortTable(
    table = cdm$cohort1,
    cohortCodelistRef = dplyr::tibble(
      not_a_cohort_definition_id = c(1, 1, 1, 2, 2) |> as.integer(),
      a_codelist_name = c(
        "disease X", "disease X", "disease X",
        "disease Y", "disease Y"
      ),
      concept_id = c(1, 2, 3, 4, 5) |> as.integer(),
      type = "index event"
    )
  ))

  expect_error(newCohortTable(
    table = cdm$cohort1,
    cohortCodelistRef = dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2, 2) |> as.integer(),
      codelist_name = c(
        "disease X", "disease X", "disease X",
        "disease Y", "disease Y"
      ),
      concept_id = c(1, 2, 3, 4, 5) |> as.integer(),
      type = "another name"
    )
  ))

  expect_no_error(newCohortTable(
    table = cdm$cohort1,
    cohortCodelistRef = NULL
  ))
})
