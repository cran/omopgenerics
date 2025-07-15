## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
# myCustomSource <- function(argument1, argument2, ...) {
#   # pre calculation and validation of arguments
#   ...
# 
#   # create the source object
#   obj <- list(x = x, y = y, ...) # this way you would access the attributes like: obj$x
#   # or
#   obj <- structure(.Data = list(), x = x, y = y, ...) # this you would access the attributes like: attr(obj, "x")
# 
#   # assign class
#   class(obj) <- "my_custom_source"
# 
#   # validation
#   omopgenerics::newCdmSource(src = obj, sourceType = "my_custom_type")
# }

## ----eval = FALSE-------------------------------------------------------------
# #' @export
# #' @importFrom omopgenerics insertTable
# insertTable.my_custom_source <- function(cdm, name, table, overwrite, temporary) {
#   # code to insert the table into your source
#   x <- "...." # it must be a reference to your table
# 
#   # validate output
#   omopgenerics::newCdmTable(table = x, src = cdm, name = name)
# }

## ----eval = FALSE-------------------------------------------------------------
# #' @export
# #' @importFrom omopgenerics listSourceTables
# listSourceTables.my_custom_source <- function(cdm) {
#   # code to list the tables present in source (cdm)
#   x <- "...."
# 
#   return(x)
# }

## ----eval = FALSE-------------------------------------------------------------
# #' @export
# #' @importFrom omopgenerics readSourceTable
# readSourceTable.my_custom_source <- function(cdm, name) {
#   # code to read the table 'name' from source.
#   x <- "...."
# 
#   # validate as cdm_table
#   omopgenerics::newCdmTable(table = x, src = cdm, name = name)
# }

## ----eval = FALSE-------------------------------------------------------------
# #' @export
# #' @importFrom omopgenerics dropSourceTable
# dropSourceTable.my_custom_source <- function(cdm, name) {
#   # code to drop the table `name` present in source (cdm)
# 
#   return(invisible(cdm))
# }

## ----eval = FALSE-------------------------------------------------------------
# #' @export
# #' @importFrom omopgenerics dropSourceTable
# insertCdmTo.my_custom_source <- function(cdm, to) {
#   # example of how it can look like:
#   tables <- names(cdm) |>
#     rlang::set_names() |>
#     purrr::map(\(x) omopgenerics::insertTable(cdm = to, name = x, table = dplyr::as_tibble(cdm[[x]])))
# 
#   omopgenerics::newCdmReference(
#     tables = tables,
#     cdmName = omopgenerics::cdmName(x = cdm),
#     cdmVersion = omopgenerics::cdmVersion(x = cdm)
#   )
# }

## ----eval = FALSE-------------------------------------------------------------
# #' @export
# summary.my_custom_source <- function(object, ...) {
#   # extract metadata
#   metadata1 <- "..."
#   metadata2 <- "..."
#   metadata3 <- "..."
# 
#   list(metadata1 = metadata1, metadata2 = metadata2, metadata3 = metadata3)
# }

## ----eval = FALSE-------------------------------------------------------------
# #' @export
# #' @importFrom dplyr compute
# compute.my_custom_source <- function(x, name, overwrite, temporary, ...) {
#   # code to compute the table into your source
#   x <- "...." # it must be a reference to your table
#   return(x)
# }

## ----eval = FALSE-------------------------------------------------------------
# cdmFromMyCustomSource <- function(argument1, argument2, ...) {
#   # read and prepare the cdm tables
#   ...
# 
#   # return the cdm object
#   omopgenerics::newCdmReference(
#     tables = tables, # list of cdm and achilles standard tables
#     cdmName = "...", # usually provided as input, but also you might want to search in the cdm_source
#     cdmVersion = "..." # either "5.3" or "5.4"
#   )
# }

## ----eval = FALSE-------------------------------------------------------------
# # read from source
# cdm <- readSourceTable(cdm = cdm, name = "my_cohort")
# 
# # or insert from local
# cdm <- insertTable(cdm = cdm, name = "my_cohort", table = localCohort)
# cdm$my_cohort <- cdm$my_cohort |>
#   newCohortTable(
#     cohortSetRef = cohort_set, # table with the settings of the cohort_table
#     cohortAttritionRef = cohort_attrition, # table with the attrition of the cohort_table
#     cohortCodelistRef = cohort_codelist # table with the codelists of the cohort_table
#   )

## ----eval = FALSE-------------------------------------------------------------
# cdmFromMyCustomSource <- function(argument1, argument2, ..., cohortTables) {
#   # read and prepare the cdm tables
#   ...
# 
#   # return the cdm object
#   cdm <- omopgenerics::newCdmReference(
#     tables = tables, # list of cdm and achilles standard tables
#     cdmName = "...", # usually provided as input, but also you might want to search in the cdm_source
#     cdmVersion = "..." # either "5.3" or "5.4"
#   )
# 
#   # read cohort tables
#   readSourceTable(cdm = cdm, name = cohortTables)
# }

