% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classCohortTable.R
\name{newCohortTable}
\alias{newCohortTable}
\title{\code{cohort_table} objects constructor.}
\usage{
newCohortTable(
  table,
  cohortSetRef = attr(table, "cohort_set"),
  cohortAttritionRef = attr(table, "cohort_attrition"),
  cohortCodelistRef = attr(table, "cohort_codelist"),
  .softValidation = FALSE
)
}
\arguments{
\item{table}{cdm_table object with at least: cohort_definition_id,
subject_id, cohort_start_date, cohort_end_date.}

\item{cohortSetRef}{Table with at least: cohort_definition_id, cohort_name}

\item{cohortAttritionRef}{Table with at least: cohort_definition_id,
number_subjects, number_records, reason_id, reason, excluded_subjects,
excluded_records.}

\item{cohortCodelistRef}{Table with at least: cohort_definition_id, codelist_name,
concept_id and codelist_type.}

\item{.softValidation}{Whether to perform a soft validation of consistency.
If set to FALSE four additional checks will be performed: 1) a check that
cohort end date is not before cohort start date,  2) a check that there
are no missing values in required columns, 3) a check that cohort duration is
all within observation period, and 4) that there are no overlapping
cohort entries}
}
\value{
A cohort_table object
}
\description{
\code{cohort_table} objects constructor.
}
\examples{
person <- dplyr::tibble(
  person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
  race_concept_id = 0, ethnicity_concept_id = 0
)
observation_period <- dplyr::tibble(
  observation_period_id = 1, person_id = 1,
  observation_period_start_date = as.Date("2000-01-01"),
  observation_period_end_date = as.Date("2023-12-31"),
  period_type_concept_id = 0
)
cohort1 <- dplyr::tibble(
  cohort_definition_id = 1, subject_id = 1,
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
cdm
cdm$cohort1 <- newCohortTable(table = cdm$cohort1)
cdm
settings(cdm$cohort1)
attrition(cdm$cohort1)
cohortCount(cdm$cohort1)

}
