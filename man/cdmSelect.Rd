% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdmSelect.R
\name{cdmSelect}
\alias{cdmSelect}
\title{Restrict the cdm object to a subset of tables.}
\usage{
cdmSelect(cdm, ...)
}
\arguments{
\item{cdm}{A cdm_reference object.}

\item{...}{Selection of tables to use, it supports tidyselect expressions.}
}
\value{
A cdm_reference with only the specified tables.
}
\description{
Restrict the cdm object to a subset of tables.
}
\examples{
cdm <- emptyCdmReference("my cdm")
cdm

cdm |>
  cdmSelect("person")

}
