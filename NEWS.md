# omopgenerics dev

* Add cohort name to attrition and cohortCount functions by @catalamarti in #836

# omopgenerics 1.4.2

* Use read.csv to import codelsits by @edward-burn in #885
* Add support for OMOP CDM 5.5 by @catalamarti in #887

# omopgenerics 1.4.1

* Insert only the first 5 lines of cars dataset by @catalamarti in #870
* Allow to import files from a url by @catalamarti in #871
* Export `supportedCdmVersions` by @catalamarti in #876
* Insert concept ids as table by @catalamarti in #879
* Ungroup in newSummarisedResult to prepent uninfromative error message by @catalamarti in #881
* Add softValidation options by @catalamarti in #875
* Add `cli` evaluate in log messages by @catalamarti in #883
* Create `guessCdmVersion()` function by @catalamarti in #882

# omopgenerics 1.4.0

* Create importCodelistWithDetails and exportCodelistWithDetails by @catalamarti in #839
* Provide better error message by @catalamarti in #834
* Use type = NULL (json + csv) by default in inport function by @catalamarti in #841
* Add methods for cdm_table (read, drop, insert, list, indexes) by @catalamarti in #837
* Assign method for cdmName by @catalamarti in #833
* Change casted to cast by @ablack3 in #814
* Allow uniqueId(n = 0) to work by @catalamarti in #842
* Add integerish argument to validateWindowArgument by @catalamarti in #844
* Create filterResult function by @catalamarti in #845
* Add argument `nm` to assert and validate functions to allow naming of the input by @catalamarti in #847
* Add null = FALSE argument to validateColumn by @catalamarti in #848
* Correct message for cohort_end_date < cohort_start_date by @catalamarti in #851
* Add recursive = FALSE option to import Concepts functions by @catalamarti in #854
* Create empty argument for assert and validate functions by @catalamarti in #849
* Attributes of cohort_table are not collected if not needed by @catalamarti in #857
* Create readOnly source by @catalamarti in #858
* Create compareOmopTableFields function by @catalamarti in #859
* Register result type by @catalamarti in #781
* Allow to validate codelists with the cdm by @catalamarti in #843
* Allow tidy expressions and negate expressions to work for dropSourecTable, readSourceTable, getCohortId functions by @catalamarti in #850
* Fix issues with NA in summarised_result by @catalamarti in #860
* Consistent documentation of arguments by @catalamarti in #855
* spell check by @catalamarti in #862
* add code_search by @catalamarti in #864

# omopgenerics 1.3.7

* Correct typo in import concept set expression in tibble format by @catalamarti in #808
* Correctly handle different cdm versions by @catalamarti in #811, #812

# omopgenerics 1.3.6

* Allow codelist and concept set import in tibble format by @catalamarti in #806

# omopgenerics 1.3.5

* Eliminate repeated elements from codelists by @catalamarti in #800
* Add methods for empty concept sets by @catalamarti in #802
* Add log query to collect by @catalamarti in #804

# omopgenerics 1.3.4

* Handle edge case in concept set expression import by @catalamarti in #793
* Add elapsed time calculation to log summary by @catalamarti in #795
* Remove specimen_source_concept_id from fieldTablesColumns by @catalamarti in #797
* Add methods for concept classes by @catalamarti in #796

# omopgenerics 1.3.3

* Add `unique` argument to `assertLogical()` by @catalamarti in #782
* Fix non UTF-8 issues (any collected character non UTF-8 is eliminated) by @catalamarti in #778
* concept set to also import codelists by @catalamarti in #787
* logging explain is now TRUE/FALSE option to add in the same log file by @catalamarti in #785

# omopgenerics 1.3.2

* Fix typo in compute timing by @edward-burn in #763
* Add stable badge by @catalamarti in #769
* Allow extraction of all codelists of a cohort with `cohortCodelist()` by @catalamarti in #770
* Export SQL and explain SQL can be added to the result object by @catalamarti in #765
* Create `as_tibble()` method for `concept_set_expression` by @catalamarti in #774
* `newConceptSetExpression()` accepts `codelists` as inputs by @catalamarti in #773
* `exportConceptSetExpression()` exports `codelists` as `concept_set_expression` by @catalamarti in #773

# omopgenerics 1.3.1

* The time a query takes to compute is included in the log file by @edward-burn in #737
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

* Write method for summary.cdm_source by @catalamarti in #719 #720
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
