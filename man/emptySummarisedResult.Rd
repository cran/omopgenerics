% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classSummarisedResult.R
\name{emptySummarisedResult}
\alias{emptySummarisedResult}
\title{Empty \code{summarised_result} object.}
\usage{
emptySummarisedResult(settings = NULL)
}
\arguments{
\item{settings}{Tibble/data.frame with the settings of the empty
summarised_result. It has to contain at least \code{result_id} column.}
}
\value{
An empty \code{summarised_result} object.
}
\description{
Empty \code{summarised_result} object.
}
\examples{
library(omopgenerics)

emptySummarisedResult()

}
