# Copyright 2023 DARWIN EU (C)
#
# This file is part of omopgenerics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Read a table from the cdm_source and add it to the cdm.
#'
#' @inheritParams cdmOrTableDoc
#' @param name Name of a table to read in the cdm_source space. Tidyselect
#' statements are supported.
#'
#' @export
#'
#' @return A cdm_reference with new table.
#'
readSourceTable <- function(cdm, name) {
  UseMethod("readSourceTable")
}

#' @export
readSourceTable.cdm_reference <- function(cdm, name) {
  name <- rlang::enquo(name)

  if (inherits(cdmSource(cdm), "read_only_source")) {
    abortReadOnlySource("readSourceTable")
  }

  tablesToRead <- listSourceTables(cdm)

  # type selection
  type <- selectType(name)
  if (type == "tidy") {
    name <- tidySelect(tablesToRead, name, "remove")
  } else if (type == "character") {
    name <- rlang::eval_tidy(name)
    notPresent <- name[!name %in% tablesToRead]
    if (length(notPresent) > 0) {
      cli::cli_warn("Unable to find the following tables: {.pkg {notPresent}}. See available tables with {.code listSourceTables(cdm)}.")
    }
    name <- name[name %in% tablesToRead]
  } else {
    cli::cli_abort(c(x = "`name` argument must be a `tidyselect` expression or a `character`."))
  }

  for (nm in name) {
    cdm[[nm]] <- readSourceTable(cdm = cdmSource(cdm), name = nm)
    set <- paste0(nm, "_set")
    atr <- paste0(nm, "_attrition")
    cod <- paste0(nm, "_codelist")
    if (all(c(set, atr, cod) %in% tablesToRead)) {
      cdm[[nm]] <- cdm[[nm]] |>
        newCohortTable(
          cohortSetRef = readSourceTable(cdm = cdmSource(cdm), name = set),
          cohortAttritionRef = readSourceTable(cdm = cdmSource(cdm), name = atr),
          cohortCodelistRef = readSourceTable(cdm = cdmSource(cdm), name = cod)
        )
    }
  }

  return(cdm)
}

#' @export
readSourceTable.cdm_table <- function(cdm, name) {
  cdm <- cdmReference(cdm)
  readSourceTable.cdm_reference(cdm = cdm, name = {{ name }})
}
