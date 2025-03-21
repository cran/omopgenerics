% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methodInsertCdmTo.R
\name{insertCdmTo}
\alias{insertCdmTo}
\title{Insert a cdm_reference object to a different source.}
\usage{
insertCdmTo(cdm, to)
}
\arguments{
\item{cdm}{A cdm_reference, if not local it will be collected into memory.}

\item{to}{A cdm_source or another cdm_reference, with a valid cdm_source.}
}
\value{
The first cdm_reference object inserted to the source.
}
\description{
Insert a cdm_reference object to a different source.
}
