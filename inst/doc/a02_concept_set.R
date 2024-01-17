## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  message = FALSE,
  error=TRUE
)

## ----setup--------------------------------------------------------------------
library(omopgenerics)

## -----------------------------------------------------------------------------
condition_codes <- list("diabetes" = c(201820, 4087682, 3655269),
                        "asthma" = 317009)
condition_codes <- codelist(condition_codes)

condition_codes

## -----------------------------------------------------------------------------
condition_codes <- list(c(201820, 4087682, 3655269))
codelist(condition_codes)

## -----------------------------------------------------------------------------
condition_codes <- list("diabetes" = c(201820, NA, 3655269),
                        "asthma" = 317009)
codelist(condition_codes)

## -----------------------------------------------------------------------------
condition_cs <- list(
  "diabetes" = dplyr::tibble(
    "concept_id" = c(201820, 4087682),
    "excluded" = c(FALSE, FALSE),
    "descendants" = c(TRUE, FALSE),
    "mapped" = c(FALSE, FALSE)
  ),
  "asthma" = dplyr::tibble(
    "concept_id" = 317009,
    "excluded" = FALSE,
    "descendants" = FALSE,
    "mapped" = FALSE
  )
)
condition_cs <- conceptSetExpression(condition_cs)

condition_cs

## -----------------------------------------------------------------------------
condition_cs <- list(
  dplyr::tibble(
    "concept_id" = c(201820, NA),
    "excluded" = c(FALSE, FALSE),
    "descendants" = c(TRUE, FALSE),
    "mapped" = c(FALSE, FALSE)
  ))
conceptSetExpression(condition_cs)

## -----------------------------------------------------------------------------
condition_cs <- list(
  "diabetes" = dplyr::tibble(
    "concept_id" = c(201820, NA),
    "excluded" = c(FALSE, FALSE),
    "descendants" = c(TRUE, FALSE),
    "mapped" = c(FALSE, FALSE)
  ),
  "asthma" = dplyr::tibble(
    "concept_id" = 317009,
    "excluded" = FALSE,
    "descendants" = FALSE,
    "mapped" = FALSE
  )
)
conceptSetExpression(condition_cs)
