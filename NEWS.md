# omopgenerics 1.3.3

* add unique argument in assertLogical by @catalamarti in #782
* Fix non UTF-8 issues (any collected character non UTF-8 is eliminated) by @catalamarti in #778
* concept set to also import codelists by @catalamarti in #787
* logging explain is now TRUE/FALSE option to add in the same log file by @catalamarti in #785

# omopgenerics 1.3.2

* Fix typo in compute timing by @edward-burn in #763
* Add stable badge by @catalamarti in #769
* Allow to extract all codelists of a cohort with `cohortCodelist()` by @catalamarti in #770
* Export sql and explain sql can be added to the result object by @catalamarti in #765
* Create `as_tibble()` method for `concept_set_expression` by @catalamarti in #774
* `newConceptSetExpression()` accepts `codelists` as inputs by @catalamarti in #773
* `exportConceptSetExpression()` exports `codelists` as `concept_set_expression` by @catalamarti in #773

# omopgenerics 1.3.1

* The time a query takes to copute is included in log file by @edward-burn in #737
* Create emptyConceptSetExpression function by @edward-burn #735
* Empty codelist has class codelist by @edward-burn in #734
* Correct typo for condition_era_id by @catalamarti in #727
* Add ... argument to insertTable by @catalamarti in #725
* observation_period and person are no longer required in the cdm object @catalamarti #746
* Create function `omopDataFolder()` for the management of OMOP related data @catalamarti #747
* Remove `tictoc` from suggests and use base R by @catalamarti #748
* Support end and start date for table payer_plan_period by @catalamarti #749
* validate cohortId in recordCohortAttrition by @catalamarti #750
* Add separate vignette for suppression by @catalamarti #751
* Depend on dbplyr 2.5.1 to be able to use the new translations of clock by @catalamarti #754
* Add methods to support local datasets by @catalamarti in #757
* Cast columns for local cdms by @catalamarti #758

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
