## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE, message = FALSE, warning = FALSE---------------------------
library(omopgenerics)

## ----eval = FALSE-------------------------------------------------------------
#  settings(cdm$cohort)

## ----eval = FALSE-------------------------------------------------------------
#  attrition(cdm$cohort)

## ----eval = FALSE-------------------------------------------------------------
#  cohortCount(cdm$cohort)

## ----eval = FALSE-------------------------------------------------------------
#  cdm <- bind(cdm$cohort1, cdm$cohort2, cdm$cohort3, name = "my_new_cohort")
#  
#  cdm$my_new_cohort
#  
#  settings(cdm$my_new_cohort)
#  
#  attrition(cdm$my_new_cohort)
#  
#  cohortCount(cdm$my_new_cohort)

## ----eval = FALSE-------------------------------------------------------------
#  summary(cdm$cohort)

