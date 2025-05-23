% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split.R
\name{splitStrata}
\alias{splitStrata}
\title{Split strata_name and strata_level columns}
\usage{
splitStrata(result, keep = FALSE, fill = "overall")
}
\arguments{
\item{result}{A dataframe with at least the columns strata_name and
strata_level.}

\item{keep}{Whether to keep the original group_name and group_level columns.}

\item{fill}{Optionally, a character that specifies what value should be
filled in with when missing.}
}
\value{
A dataframe.
}
\description{
Pivots the input dataframe so the values of the column strata_name are
transformed into columns that contain values from the strata_level column.
}
\examples{
{
  library(dplyr)
  library(omopgenerics)

  x <- tibble(
    "result_id" = as.integer(c(1, 2)),
    "cdm_name" = c("cprd", "eunomia"),
    "group_name" = "cohort_name",
    "group_level" = "my_cohort",
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
    newSummarisedResult(settings = tibble(
      "result_id" = c(1, 2), "custom" = c("A", "B")
    ))

  x

  x |> splitStrata()
}

}
