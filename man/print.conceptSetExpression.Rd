% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classConceptSetExpression.R
\name{print.conceptSetExpression}
\alias{print.conceptSetExpression}
\title{Print a concept set expression}
\usage{
\method{print}{conceptSetExpression}(x, ...)
}
\arguments{
\item{x}{A concept set expression}

\item{...}{Included for compatibility with generic. Not used.}
}
\value{
Invisibly returns the input
}
\description{
Print a concept set expression
}
\examples{
asthma_cs <- list(
  "asthma_narrow" = dplyr::tibble(
    "concept_id" = 1,
    "excluded" = FALSE,
    "descendants" = TRUE,
    "mapped" = FALSE
  ),
  "asthma_broad" = dplyr::tibble(
    "concept_id" = c(1, 2),
    "excluded" = FALSE,
    "descendants" = TRUE,
    "mapped" = FALSE
  )
)
asthma_cs <- newConceptSetExpression(asthma_cs)
print(asthma_cs)
}
