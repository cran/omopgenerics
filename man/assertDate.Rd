% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assert.R
\name{assertDate}
\alias{assertDate}
\title{Assert Date}
\usage{
assertDate(
  x,
  length = NULL,
  na = FALSE,
  null = FALSE,
  unique = FALSE,
  named = FALSE,
  call = parent.frame(),
  msg = NULL
)
}
\arguments{
\item{x}{Expression to check.}

\item{length}{Required length.}

\item{na}{Whether it can contain NA values.}

\item{null}{Whether it can be NULL.}

\item{unique}{Whether it has to contain unique elements.}

\item{named}{Whether it has to be named.}

\item{call}{Call argument that will be passed to \code{cli} error message.}

\item{msg}{Custom error message.}
}
\value{
x
}
\description{
Assert Date
}
