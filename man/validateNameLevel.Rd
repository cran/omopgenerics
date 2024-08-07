% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classSummarisedResult.R
\name{validateNameLevel}
\alias{validateNameLevel}
\title{Validate if two columns are valid Name-Level pair.}
\usage{
validateNameLevel(x, nameColumn, levelColumn, sep = " &&& ", warn = FALSE)
}
\arguments{
\item{x}{A tibble.}

\item{nameColumn}{Column name of the \code{name}.}

\item{levelColumn}{Column name of the \code{level}.}

\item{sep}{Separation pattern.}

\item{warn}{Whether to throw a warning (TRUE) or an error (FALSE).}
}
\description{
Validate if two columns are valid Name-Level pair.
}
