# Argument descriptions repeated > 1:

#' Helper for consistent documentation of `cdm`.
#'
#' @param cdm A `<cdm_reference>` object.
#'
#' @name cdmDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of CDM names.
#'
#' @param cdmName Name of the `<cdm_reference>` object.
#'
#' @name cdmNameDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of CDM version arguments.
#'
#' @param cdmVersion Version of the OMOP Common Data Model. Supported options
#' are: `r supportedCdmVersionsOptions`.
#'
#' @name cdmVersionArgumentDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `cdm` concept validation.
#'
#' @param cdm A `<cdm_reference>` object. If provided, concept IDs in `x` will
#' be checked against `cdm$concept`.
#'
#' @name conceptCdmDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of cdm reference or table arguments.
#'
#' @param cdm A `<cdm_reference>` or `<cdm_table>` object.
#'
#' @name cdmOrTableDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `cdm_table` objects.
#'
#' @param table A `<cdm_table>` object.
#'
#' @name cdmTableDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `cohort_table` objects.
#'
#' @param cohort A `<cohort_table>` object.
#'
#' @name cohortDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of cohort validation checks.
#'
#' @param checkEndAfterStart If TRUE a check that all cohort end dates come on
#' or after cohort start date will be performed.
#' @param checkOverlappingEntries If TRUE a check that no individuals have
#' overlapping cohort entries will be performed.
#' @param checkMissingValues If TRUE a check that there are no missing values in
#' required fields will be performed.
#' @param checkInObservation If TRUE a check that cohort entries are within
#' the individuals observation periods will be performed.
#'
#' @name cohortValidationChecksDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `codelist` objects.
#'
#' @param x A `<codelist>` object.
#'
#' @name codelistDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `codelist_with_details` objects.
#'
#' @param x A `<codelist_with_details>` object.
#'
#' @name codelistWithDetailsDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `concept_set_expression` objects.
#'
#' @param x A `<concept_set_expression>` object.
#'
#' @name conceptSetExpressionDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `summarised_result` objects.
#'
#' @param result A `<summarised_result>` object.
#'
#' @name summarisedResultDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of CDM index functions.
#'
#' @param x A `<cdm_reference>` or `<cdm_table>` object.
#' @param name Name(s) of the CDM table(s).
#'
#' @name cdmIndexDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of empty table names.
#'
#' @param name Name of the table to create.
#'
#' @name emptyTableNameDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of assigning tables to a CDM reference.
#'
#' @param name Name where the new table will be assigned.
#' @param value Table with the same source as the `cdm_reference` object.
#'
#' @name cdmAssignTableDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of OMOP CDM versions.
#'
#' @param version Version of the OMOP Common Data Model. Supported options are:
#' `r supportedCdmVersionsOptions`.
#'
#' @name omopCdmVersionDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of validation mode.
#'
#' @param validation How to perform validation: "error", "warning".
#'
#' @name validationDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of empty arguments.
#'
#' @param empty Whether it can be empty.
#'
#' @name emptyDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of cast arguments.
#'
#' @param cast Whether to cast columns to the correct type.
#'
#' @name castDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of overwrite arguments.
#'
#' @param overwrite Whether to overwrite an existing table.
#'
#' @name overwriteDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of call arguments.
#'
#' @param call Call argument passed to `cli` functions.
#'
#' @name cliCallDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of assertion functions.
#'
#' @param x Variable to check.
#' @param length Required length. If `NULL` length is not checked.
#' @param na Whether it can contain NA values.
#' @param null Whether it can be NULL.
#' @param empty Whether it can be empty.
#' @param unique Whether it has to contain unique elements.
#' @param named Whether it has to be named.
#' @param nm Name to use in error messages. Defaults to the expression supplied
#' to `x`.
#' @param call Call argument that will be passed to `cli`.
#' @param msg Custom error message.
#'
#' @name assertDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of unused dots.
#'
#' @param ... For compatibility; not used.
#'
#' @name unusedDotsDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of exported files.
#'
#' @param path Path to where files will be created.
#' @param type Type of files to export. Currently 'json' and 'csv' are
#' supported.
#'
#' @name exportFileDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of recursive file imports.
#'
#' @param recursive If TRUE and path is a directory, search for files will
#' recurse into directories.
#'
#' @name recursiveDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of imported files.
#'
#' @param path Path to a file or directory to import.
#' @param type Type of files to import. If `NULL`, all supported file types are
#' imported. Currently 'json' and 'csv' are supported.
#' @param recursive If TRUE and path is a directory, search for files will
#' recurse into directories.
#'
#' @name importFileDoc
#' @keywords internal
NULL
