% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combineStrata.R
\name{combineStrata}
\alias{combineStrata}
\title{Provide all combinations of strata levels.}
\usage{
combineStrata(levels, overall = FALSE)
}
\arguments{
\item{levels}{Vector of all strata levels to combine.}

\item{overall}{Whether to provide an empty element \code{character()}.}
}
\value{
A vector of all combinations of strata.
}
\description{
Provide all combinations of strata levels.
}
\examples{

combineStrata(character())
combineStrata(character(), overall = TRUE)
combineStrata(c("age", "sex"), overall = TRUE)
combineStrata(c("age", "sex", "year"))

}
