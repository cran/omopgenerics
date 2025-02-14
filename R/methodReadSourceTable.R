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

#' Read a table from the cdm_source and add it to to the cdm.
#'
#' @param cdm A cdm reference.
#' @param name Name of a table to read in the cdm_source space.
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
  tablesToRead <- listSourceTables(cdm)

  # is tidy select?
  if (isTidySelect(rlang::enquo(name))) {
    name <- selectTables(tables = tablesToRead, name = name)
  } else {
    assertCharacter(name)
    notPresent <- name[!name %in% tablesToRead]
    if (length(notPresent) > 0) {
      cli::cli_warn("Not able to find the following tables: {.pkg {notPresent}}. See available tables with {.code listSourceTables(cdm)}.")
    }
    name <- name[name %in% tablesToRead]
  }

  for (nm in name) {
    cdm[[nm]] <- readSourceTable(cdm = cdmSource(cdm), name = nm)
  }

  return(cdm)
}
