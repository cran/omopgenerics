## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(omopgenerics, warn.conflicts = FALSE)

# create the log file
createLogFile(logFile = tempfile(pattern = "log_{date}_{time}"))

# study
logMessage("Generating random numbers")
x <- runif(1e6)

logMessage("Calculating the sum")
result <- sum(x)

# export logger to a `summarised_result`
log <- summariseLogFile()

# content of the log file
readLines(getOption("omopgenerics.logFile")) |>
  cat(sep = "\n")

# `summarised_result` object
log

# `summarised_result` object settings
settings(log)

# tidy version of the `summarised_result`
tidy(log)

## ----echo=FALSE---------------------------------------------------------------
options("omopgenerics.logFile" = NULL)

## -----------------------------------------------------------------------------
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

# create the log file
createLogFile(logFile = tempfile(pattern = "log_{date}_{time}"))

# start analysis
logMessage("Deffining toy data")
n <- 1e5
x <- tibble(person_id = seq_len(n), age = rnorm(n = n, mean = 55, sd = 20))

logMessage("Summarise toy data")
res <- x |>
  summarise(
    `number subjects_count` = n(),
    `age_mean` = mean(age),
    `age_sd` = sd(age),
    `age_median` = median(age),
    `age_q25` = quantile(age, 0.25),
    `age_q75` = quantile(age, 0.75)
  ) |>
  pivot_longer(
    cols = everything(), 
    names_to = c("variable_name", "estimate_name"), 
    names_sep = "_",
    values_to = "estimate_value"
  ) |>
  mutate(
    result_id = 1L,
    cdm_name = "mock data",
    variable_level = NA_character_,
    estimate_type = if_else(estimate_name == "count", "integer", "numeric"),
    estimate_value = as.character(estimate_value)
  ) |>
  uniteGroup() |>
  uniteStrata() |>
  uniteAdditional() |>
  newSummarisedResult()

# res is a summarised_result object that we can export using the `exportSummarisedResult`
tempDir <- tempdir()
exportSummarisedResult(res, path = tempDir)

## -----------------------------------------------------------------------------
result <- importSummarisedResult(tempDir)

## -----------------------------------------------------------------------------
result |>
  settings() |> 
  glimpse()

## -----------------------------------------------------------------------------
result |>
  filterSettings(result_type == "summarise_log_file") |>
  tidy()

