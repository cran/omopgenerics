
#' Get the `result_type(s)` defined in a certain package
#'
#' @param pkg A character vector of the packages of interest
#'
#' @return A named (package) list of the result_type registered in each package.
#'
#' @export
#'
#' @examples
#' library(omopgenerics)
#'
#' resultType()
#'
resultType <- function(pkg = NULL) {
  UseMethod(generic = "resultType", object = pkg)
}

#' @export
resultType.NULL <- function(pkg) {
  utils::methods("resultType") |>
    as.character() |>
    stringr::str_replace_all(pattern = "^resultType.", replacement = "") |>
    purrr::keep(\(x) !x %in% c("character", "NULL")) |>
    resultType()
}

#' @export
resultType.character <- function(pkg) {
  pkg |>
    rlang::set_names() |>
    purrr::map(\(x) {
      if (!rlang::is_installed(pkg = x)) {
        cli::cli_inform(c("!" = "{.pkg {x}} is not installed."))
        res <- NA_character_
      } else {
        m <- utils::getS3method(f = "resultType", class = x, optional = TRUE)
        if (!is.null(m)) {
          class(x) <- x
          res <- resultType(x)
        } else {
          cli::cli_inform(c("!" = "No registered `result_type` found in {.pkg {x}}."))
          res <- NA_character_
        }
      }
      return(res)
    })
}

#' @export
resultType.omopgenerics <- function(pkg) {
  c("cdm_snapshot", "cohort_count", "cohort_attrition", "summarise_log_file",
    "summarise_log_sql")
}
