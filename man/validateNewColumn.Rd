% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{validateNewColumn}
\alias{validateNewColumn}
\title{Validate a new column of a table}
\usage{
validateNewColumn(table, column, validation = "warning", call = parent.frame())
}
\arguments{
\item{table}{The table to check if the column already exists.}

\item{column}{Character vector with the name(s) of the new column(s).}

\item{validation}{Whether to throw warning or error.}

\item{call}{Passed to cli functions.}
}
\value{
table without conflicting columns.
}
\description{
Validate a new column of a table
}
\examples{
x <- dplyr::tibble(
  column1 = c(1L, 2L),
  column2 = c("a", "b")
)
validateNewColumn(x, "not_exiting_column")
validateNewColumn(x, "column1")

}
