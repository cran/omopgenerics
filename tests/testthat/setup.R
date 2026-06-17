mockCdmWithConcept <- function(conceptId = c(1L, 2L, 3L)) {
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
  concept <- dplyr::tibble(
    concept_id = as.integer(conceptId),
    concept_name = paste0("concept ", conceptId),
    domain_id = "Condition",
    vocabulary_id = "SNOMED",
    concept_class_id = "Clinical Finding",
    standard_concept = "S",
    concept_code = as.character(conceptId),
    valid_start_date = as.Date("1970-01-01"),
    valid_end_date = as.Date("2099-12-31"),
    invalid_reason = NA_character_
  )

  cdmFromTables(
    tables = list(
      "person" = person,
      "observation_period" = observation_period,
      "concept" = concept
    ),
    cdmName = "test"
  )
}

mockSummarisedResult <- function() {
  result <- dplyr::tibble(
    "cdm_name" = "mock",
    "group_name" = "cohort_name",
    "group_level" = c(rep("cohort1", 9), rep("cohort2", 9)),
    "strata_name" = rep(c(
      "overall", rep("age_group &&& sex", 4), rep("sex", 2), rep("age_group", 2)
    ), 2),
    "strata_level" = rep(c(
      "overall", "<40 &&& Male", ">=40 &&& Male", "<40 &&& Female",
      ">=40 &&& Female", "Male", "Female", "<40", ">=40"
    ), 2),
    "variable_name" = "number subjects",
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "integer",
    "estimate_value" = round(10000000 * stats::runif(18)) |> as.character(),
    "additional_name" = "overall",
    "additional_level" = "overall"
  ) |>
    # age - mean
    dplyr::union_all(
      dplyr::tibble(
        "cdm_name" = "mock",
        "group_name" = "cohort_name",
        "group_level" = c(rep("cohort1", 9), rep("cohort2", 9)),
        "strata_name" = rep(c(
          "overall", rep("age_group &&& sex", 4), rep("sex", 2), rep("age_group", 2)
        ), 2),
        "strata_level" = rep(c(
          "overall", "<40 &&& Male", ">=40 &&& Male", "<40 &&& Female",
          ">=40 &&& Female", "Male", "Female", "<40", ">=40"
        ), 2),
        "variable_name" = "age",
        "variable_level" = NA_character_,
        "estimate_name" = "mean",
        "estimate_type" = "numeric",
        "estimate_value" = c(100 * stats::runif(18)) |> as.character(),
        "additional_name" = "overall",
        "additional_level" = "overall"
      )
    ) |>
    # age - standard deviation
    dplyr::union_all(
      dplyr::tibble(
        "cdm_name" = "mock",
        "group_name" = "cohort_name",
        "group_level" = c(rep("cohort1", 9), rep("cohort2", 9)),
        "strata_name" = rep(c(
          "overall", rep("age_group &&& sex", 4), rep("sex", 2), rep("age_group", 2)
        ), 2),
        "strata_level" = rep(c(
          "overall", "<40 &&& Male", ">=40 &&& Male", "<40 &&& Female",
          ">=40 &&& Female", "Male", "Female", "<40", ">=40"
        ), 2),
        "variable_name" = "age",
        "variable_level" = NA_character_,
        "estimate_name" = "sd",
        "estimate_type" = "numeric",
        "estimate_value" = c(10 * stats::runif(18)) |> as.character(),
        "additional_name" = "overall",
        "additional_level" = "overall"
      )
    ) |>
    # medication - count
    dplyr::union_all(
      dplyr::tibble(
        "cdm_name" = "mock",
        "group_name" = "cohort_name",
        "group_level" = c(rep("cohort1", 9), rep("cohort2", 9)),
        "strata_name" = rep(c(
          "overall", rep("age_group &&& sex", 4), rep("sex", 2), rep("age_group", 2)
        ), 2),
        "strata_level" = rep(c(
          "overall", "<40 &&& Male", ">=40 &&& Male", "<40 &&& Female",
          ">=40 &&& Female", "Male", "Female", "<40", ">=40"
        ), 2),
        "variable_name" = "Medications",
        "variable_level" = "Amoxiciline",
        "estimate_name" = "count",
        "estimate_type" = "integer",
        "estimate_value" = round(100000 * stats::runif(18)) |> as.character(),
        "additional_name" = "overall",
        "additional_level" = "overall"
      )
    ) |>
    # medication - percentage
    dplyr::union_all(
      dplyr::tibble(
        "cdm_name" = "mock",
        "group_name" = "cohort_name",
        "group_level" = c(rep("cohort1", 9), rep("cohort2", 9)),
        "strata_name" = rep(c(
          "overall", rep("age_group &&& sex", 4), rep("sex", 2), rep("age_group", 2)
        ), 2),
        "strata_level" = rep(c(
          "overall", "<40 &&& Male", ">=40 &&& Male", "<40 &&& Female",
          ">=40 &&& Female", "Male", "Female", "<40", ">=40"
        ), 2),
        "variable_name" = "Medications",
        "variable_level" = "Amoxiciline",
        "estimate_name" = "percentage",
        "estimate_type" = "percentage",
        "estimate_value" = c(100 * stats::runif(18)) |> as.character(),
        "additional_name" = "overall",
        "additional_level" = "overall"
      )
    ) |>
    # medication - count
    dplyr::union_all(
      dplyr::tibble(
        "cdm_name" = "mock",
        "group_name" = "cohort_name",
        "group_level" = c(rep("cohort1", 9), rep("cohort2", 9)),
        "strata_name" = rep(c(
          "overall", rep("age_group &&& sex", 4), rep("sex", 2), rep("age_group", 2)
        ), 2),
        "strata_level" = rep(c(
          "overall", "<40 &&& Male", ">=40 &&& Male", "<40 &&& Female",
          ">=40 &&& Female", "Male", "Female", "<40", ">=40"
        ), 2),
        "variable_name" = "Medications",
        "variable_level" = "Ibuprofen",
        "estimate_name" = "count",
        "estimate_type" = "integer",
        "estimate_value" = round(100000 * stats::runif(18)) |> as.character(),
        "additional_name" = "overall",
        "additional_level" = "overall"
      )
    ) |>
    # medication - percentage
    dplyr::union_all(
      dplyr::tibble(
        "cdm_name" = "mock",
        "group_name" = "cohort_name",
        "group_level" = c(rep("cohort1", 9), rep("cohort2", 9)),
        "strata_name" = rep(c(
          "overall", rep("age_group &&& sex", 4), rep("sex", 2), rep("age_group", 2)
        ), 2),
        "strata_level" = rep(c(
          "overall", "<40 &&& Male", ">=40 &&& Male", "<40 &&& Female",
          ">=40 &&& Female", "Male", "Female", "<40", ">=40"
        ), 2),
        "variable_name" = "Medications",
        "variable_level" = "Ibuprofen",
        "estimate_name" = "percentage",
        "estimate_type" = "percentage",
        "estimate_value" = c(100 * stats::runif(18)) |> as.character(),
        "additional_name" = "overall",
        "additional_level" = "overall"
      )
    ) |>
    dplyr::mutate(result_id = as.integer(1)) |>
    newSummarisedResult(
      settings = dplyr::tibble(
        "result_id" = as.integer(1),
        "result_type" = "mock_summarised_result",
        "package_name" = "omopgenerics",
        "package_version" = utils::packageVersion("omopgenerics") |>
          as.character()
      )
    )
}
