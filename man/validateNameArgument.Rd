% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{validateNameArgument}
\alias{validateNameArgument}
\title{Validate name argument. It must be a snake_case character vector. You can add
the a cdm object to check \code{name} is not already used in that cdm.}
\usage{
validateNameArgument(
  name,
  cdm = NULL,
  validation = "error",
  null = FALSE,
  call = parent.frame()
)
}
\arguments{
\item{name}{Name of a new table to be added to a cdm object.}

\item{cdm}{A cdm_reference object. It will check if a table named name
already exists in the cdm.}

\item{validation}{How to perform validation: "error", "warning".}

\item{null}{If TRUE, name can be NULL}

\item{call}{A call argument to pass to cli functions.}
}
\description{
Validate name argument. It must be a snake_case character vector. You can add
the a cdm object to check \code{name} is not already used in that cdm.
}
\examples{
# this is a validate name
name <- "my_new_table"
validateNameArgument(name)

# this is not
name <- "myTableNAME"
validateNameArgument(name, validation = "warning")

}
