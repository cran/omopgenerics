
<!-- README.md is generated from README.Rmd. Please edit that file -->

# omopgenerics

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/darwin-eu/omopgenerics/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/omopgenerics/actions)
[![CRANstatus](https://www.r-pkg.org/badges/version/omopgenerics)](https://CRAN.R-project.org/package=omopgenerics)
[![Codecov test
coverage](https://codecov.io/gh/darwin-eu/omopgenerics/branch/main/graph/badge.svg)](https://app.codecov.io/gh/darwin-eu/omopgenerics?branch=main)
<!-- badges: end -->

## Package overview

The omopgenerics package provides definitions of core classes and
methods used by analytic pipelines that query the OMOP common data
model.

    #> Warning in citation("omopgenerics"): could not determine year for
    #> 'omopgenerics' from package DESCRIPTION file
    #> To cite package 'omopgenerics' in publications use:
    #> 
    #>   Català M, Burn E (2025). _omopgenerics: Methods and Classes for the
    #>   OMOP Common Data Model_. R package version 1.3.2,
    #>   <https://darwin-eu.github.io/omopgenerics/>.
    #> 
    #> A BibTeX entry for LaTeX users is
    #> 
    #>   @Manual{,
    #>     title = {omopgenerics: Methods and Classes for the OMOP Common Data Model},
    #>     author = {Martí Català and Edward Burn},
    #>     note = {R package version 1.3.2},
    #>     url = {https://darwin-eu.github.io/omopgenerics/},
    #>   }

If you find the package useful in supporting your research study, please
consider citing this package.

## Installation

You can install the development version of OMOPGenerics from
[GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pkg_install("darwin-eu/omopgenerics")
```

And load it using the library command:

``` r
library(omopgenerics)
library(dplyr)
```

## Core classes and methods

### CDM Reference

A cdm reference is a single R object that represents OMOP CDM data. The
tables in the cdm reference may be in a database, but a cdm reference
may also contain OMOP CDM tables that are in dataframes/tibbles or in
arrow. In the latter case the cdm reference would typically be a subset
of an original cdm reference that has been derived as part of a
particular analysis.

omopgenerics contains the class definition of a cdm reference and a
dataframe implementation. For creating a cdm reference using a database,
see the CDMConnector package
(<https://darwin-eu.github.io/CDMConnector/>).

A cdm object can contain four type of tables:

- Standard tables:

``` r
omopTables()
#>  [1] "person"                "observation_period"    "visit_occurrence"     
#>  [4] "visit_detail"          "condition_occurrence"  "drug_exposure"        
#>  [7] "procedure_occurrence"  "device_exposure"       "measurement"          
#> [10] "observation"           "death"                 "note"                 
#> [13] "note_nlp"              "specimen"              "fact_relationship"    
#> [16] "location"              "care_site"             "provider"             
#> [19] "payer_plan_period"     "cost"                  "drug_era"             
#> [22] "dose_era"              "condition_era"         "metadata"             
#> [25] "cdm_source"            "concept"               "vocabulary"           
#> [28] "domain"                "concept_class"         "concept_relationship" 
#> [31] "relationship"          "concept_synonym"       "concept_ancestor"     
#> [34] "source_to_concept_map" "drug_strength"         "cohort_definition"    
#> [37] "attribute_definition"  "concept_recommended"
```

Each one of the tables has a required columns. For example, for the
`person` table this are the required columns:

``` r
omopColumns(table = "person")
#>  [1] "person_id"                   "gender_concept_id"          
#>  [3] "year_of_birth"               "month_of_birth"             
#>  [5] "day_of_birth"                "birth_datetime"             
#>  [7] "race_concept_id"             "ethnicity_concept_id"       
#>  [9] "location_id"                 "provider_id"                
#> [11] "care_site_id"                "person_source_value"        
#> [13] "gender_source_value"         "gender_source_concept_id"   
#> [15] "race_source_value"           "race_source_concept_id"     
#> [17] "ethnicity_source_value"      "ethnicity_source_concept_id"
```

- Cohort tables We can see the cohort-related tables and their required
  columns.

``` r
cohortTables()
#> [1] "cohort"           "cohort_set"       "cohort_attrition" "cohort_codelist"
cohortColumns(table = "cohort")
#> [1] "cohort_definition_id" "subject_id"           "cohort_start_date"   
#> [4] "cohort_end_date"
```

In addition, cohorts are defined in terms of a `generatedCohortSet`
class. For more details on this class definition see the corresponding
vignette.

- Achilles tables The Achilles R package generates descriptive
  statistics about the data contained in the OMOP CDM. Again, we can see
  the tables created and their required columns.

``` r
achillesTables()
#> [1] "achilles_analysis"     "achilles_results"      "achilles_results_dist"
achillesColumns(table = "achilles_results")
#> [1] "analysis_id" "stratum_1"   "stratum_2"   "stratum_3"   "stratum_4"  
#> [6] "stratum_5"   "count_value"
```

- Other tables, these other tables can have any format.

Any table to be part of a cdm object has to fulfill 4 conditions:

- All must share a common source.

- The name of the tables must be lowercase.

- The name of the column names of each table must be lowercase.

- `person` and `observation_period` must be present.

### Concept set

A concept set can be represented as either a codelist or a concept set
expression. A codelist is a named list, with each item of the list
containing specific concept IDs.

``` r
condition_codes <- list(
  "diabetes" = c(201820L, 4087682L, 3655269L),
  "asthma" = 317009L
)
condition_codes <- newCodelist(condition_codes)

condition_codes
#> 
#> ── 2 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - asthma (1 codes)
#> - diabetes (3 codes)
```

Meanwhile, a concept set expression provides a high-level definition of
concepts that, when applied to a specific OMOP CDM vocabulary version
(by making use of the concept hierarchies and relationships), will
result in a codelist.

``` r
condition_cs <- list(
  "diabetes" = dplyr::tibble(
    "concept_id" = c(201820L, 4087682L),
    "excluded" = c(FALSE, FALSE),
    "descendants" = c(TRUE, FALSE),
    "mapped" = c(FALSE, FALSE)
  ),
  "asthma" = dplyr::tibble(
    "concept_id" = 317009L,
    "excluded" = FALSE,
    "descendants" = FALSE,
    "mapped" = FALSE
  )
)
condition_cs <- newConceptSetExpression(condition_cs)

condition_cs
#> 
#> ── 2 concept set expressions ───────────────────────────────────────────────────
#> 
#> - asthma (1 concept criteria)
#> - diabetes (2 concept criteria)
```

### A cohort table

A cohort is a set of persons who satisfy one or more inclusion criteria
for a duration of time and, when defined, this table in a cdm reference
has a cohort table class. Cohort tables are then associated with
attributes such as settings and attrition.

``` r
person <- tibble(
  person_id = 1L,
  gender_concept_id = 0L,
  year_of_birth = 1990L,
  race_concept_id = 0L, 
  ethnicity_concept_id = 0L
)
observation_period <- dplyr::tibble(
  observation_period_id = 1L, 
  person_id = 1L,
  observation_period_start_date = as.Date("2000-01-01"),
  observation_period_end_date = as.Date("2023-12-31"),
  period_type_concept_id = 0L
)
diabetes <- tibble(
  cohort_definition_id = 1L, 
  subject_id = 1L,
  cohort_start_date = as.Date("2020-01-01"),
  cohort_end_date = as.Date("2020-01-10")
)

cdm <- cdmFromTables(
  tables = list(
    "person" = person,
    "observation_period" = observation_period,
    "diabetes" = diabetes
  ),
  cdmName = "example_cdm"
)
cdm$diabetes <- newCohortTable(cdm$diabetes)

cdm$diabetes
#> # A tibble: 1 × 4
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                  <int>      <int> <date>            <date>         
#> 1                    1          1 2020-01-01        2020-01-10
settings(cdm$diabetes)
#> # A tibble: 1 × 2
#>   cohort_definition_id cohort_name
#>                  <int> <chr>      
#> 1                    1 cohort_1
attrition(cdm$diabetes)
#> # A tibble: 1 × 7
#>   cohort_definition_id number_records number_subjects reason_id reason          
#>                  <int>          <int>           <int>     <int> <chr>           
#> 1                    1              1               1         1 Initial qualify…
#> # ℹ 2 more variables: excluded_records <int>, excluded_subjects <int>
cohortCount(cdm$diabetes)
#> # A tibble: 1 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1              1               1
```

### Summarised result

A summarised result provides a standard format for the results of an
analysis performed against data mapped to the OMOP CDM.

For example this format is used when we get a summary of the cdm as a
whole

``` r
summary(cdm) |>
  glimpse()
#> Rows: 13
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ cdm_name         <chr> "example_cdm", "example_cdm", "example_cdm", "example…
#> $ group_name       <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ group_level      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "snapshot_date", "person_count", "observation_period_…
#> $ variable_level   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
#> $ estimate_name    <chr> "value", "count", "count", "source_name", "version", …
#> $ estimate_type    <chr> "date", "integer", "integer", "character", "character…
#> $ estimate_value   <chr> "2025-10-12", "1", "1", "", NA, "5.3", "", "", "", ""…
#> $ additional_name  <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ additional_level <chr> "overall", "overall", "overall", "overall", "overall"…
```

and also when we summarise a cohort

``` r
summary(cdm$diabetes) |>
  glimpse()
#> `cohort_definition_id` casted to character.
#> `cohort_definition_id` casted to character.
#> Rows: 6
#> Columns: 13
#> $ result_id        <int> 1, 1, 2, 2, 2, 2
#> $ cdm_name         <chr> "example_cdm", "example_cdm", "example_cdm", "example…
#> $ group_name       <chr> "cohort_name", "cohort_name", "cohort_name", "cohort_…
#> $ group_level      <chr> "cohort_1", "cohort_1", "cohort_1", "cohort_1", "coho…
#> $ strata_name      <chr> "overall", "overall", "reason", "reason", "reason", "…
#> $ strata_level     <chr> "overall", "overall", "Initial qualifying events", "I…
#> $ variable_name    <chr> "number_records", "number_subjects", "number_records"…
#> $ variable_level   <chr> NA, NA, NA, NA, NA, NA
#> $ estimate_name    <chr> "count", "count", "count", "count", "count", "count"
#> $ estimate_type    <chr> "integer", "integer", "integer", "integer", "integer"…
#> $ estimate_value   <chr> "1", "1", "1", "1", "0", "0"
#> $ additional_name  <chr> "overall", "overall", "reason_id", "reason_id", "reas…
#> $ additional_level <chr> "overall", "overall", "1", "1", "1", "1"
```
