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
