# omopgenerics 1.3.0

* write method fro summary.cdm_source by @catalamarti in #719 #720
* Add query id in logging files by @catalamarti in #716
* Expanding omopgenerics vignette by @catalamarti in #721
* Indexes experimental functions by @catalamarti in #722 #723 #724

# omopgenerics 1.2.0

* Remove NA in estimates in transformToSummarisedResult by @catalamarti in #702
* Create logging functions by @catalamarti in #700
* Allow strata to be a character by @catalamarti in #703
* Remove settings that are NA after filterSettings by @catalamarti in #704
* `validateWindowArgument` force snake_case names by @catalamarti in #711
* Keep cohort_table class after collect by @catalamarti in #710
* `dplyr::as_tibble` for codelist by @catalamarti in #712
* `type` -> `codelist_type` by @catalamarti in #709

# omopgenerics 1.1.1

* more general validation for cohorts by @edward-burn in #692
* change `grepl` to `stringr::str_detect` by @catalamarti in #689
* allow `readr::guess_encoding` to fail and default configuration by @catalamarti in #685
* keep codelist class when subsetting by @catalamarti in #693
* export summarised_results always as utf8 by @catalamarti in #690
* add option checkPermanentTable to `validateCohortArgument` by @catalamarti in #694

# omopgenerics 1.1.0

* more general cdm validation checks by @edward-burn in #674
* typo in validateConceptSet by @catalamarti in #673
* fix call argument by @catalamarti in #677
* fix tempdir("...") by @catalamarti in #679
* new function transformToSummarisedResult by @catalamarti in #676

# omopgenerics 1.0.0

* Stable release of the package.
* Added a `NEWS.md` file to track changes to the package.
