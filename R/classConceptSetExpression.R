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


#' 'conceptSetExpression' object constructor
#'
#' @param x a named list of tibbles, each of which containing concept set
#' definitions
#'
#' @return A conceptSetExpression
#' @export
#'
newConceptSetExpression <- function(x) {

  #constructor
  x <- constructConceptSetExpression(x)

  # validate
  x <- validateConceptSetExpression(x)

  return(x)
}

constructConceptSetExpression <- function(x) {

  class(x) <- c("conceptSetExpression", class(x)[which(class(x) != "conceptSetExpression")])

  return(x)
}

validateConceptSetExpression <- function(x) {

  assertList(x, named = TRUE, class = c("tbl"))

  for(i in seq_along(x)){
    assertTable(
      x = x[[i]], class = "data.frame",
      columns = c("concept_id", "excluded", "descendants", "mapped")
    )
    assertNumeric(x[[i]]$concept_id, integerish = TRUE)
    assertLogical(x[[i]]$excluded)
    assertLogical(x[[i]]$descendants)
    assertLogical(x[[i]]$mapped)
  }

  return(x)
}


#' Print a concept set expression
#'
#' @param x A concept set expression
#' @param ...  Included for compatibility with generic. Not used.
#'
#' @return  Invisibly returns the input
#' @export
#'
#' @examples
#' asthma_cs <- list("asthma_narrow" = dplyr::tibble(
#'   "concept_id" = 1,
#'   "excluded" = FALSE,
#'   "descendants" = TRUE,
#'   "mapped" = FALSE
#' ),
#' "asthma_broad" = dplyr::tibble(
#'   "concept_id" = c(1,2),
#'   "excluded" = FALSE,
#'   "descendants" = TRUE,
#'   "mapped" = FALSE
#' ))
#' asthma_cs <- newConceptSetExpression(asthma_cs)
#' print(asthma_cs)
print.conceptSetExpression <- function(x, ...) {
  cli::cli_h1("{length(x)} conceptSetExpression{?s}")
  cli::cat_line("")
  if(length(x) <= 6){
    for(i in seq_along(x)){
      cli::cat_line(paste0("- ", names(x)[i], " (", nrow(x[[i]]), " concept criteria)"))
    }
  } else {
    for(i in seq_along(x[1:10])){
      cli::cat_line(paste0("- ", names(x[1:10])[i], " (", nrow(x[[i]]), " concept criteria)"))
    }
    cli::cat_line(paste0("along with ", length(x)-10, " more concept sets"))
  }
  invisible(x)

}
