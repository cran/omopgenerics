
#' Check or set the OMOP_DATA_FOLDER where the OMOP related data is stored.
#'
#' @param path Path to a folder to store the OMOP related data. If NULL the
#' current `OMOP_DATA_FOLDER` is returned.
#'
#' @return The OMOP data folder.
#' @export
#'
#' @examples
#' \donttest{
#' omopDataFolder()
#' omopDataFolder(file.path(tempdir(), "OMOP_DATA"))
#' omopDataFolder()
#' }
#'
omopDataFolder <- function(path = NULL) {
  key <- "OMOP_DATA_FOLDER"
  if (is.null(path)) {
    if (Sys.getenv(key) == "") {
      tempOmopFolder <- file.path(tempdir(), key)
      dir.create(tempOmopFolder, showWarnings = FALSE)
      if (rlang::is_interactive()) {
        cli::cli_inform(c("i" = "`{key}` temporarily set to {.path {tempOmopFolder}}."))
        cli::cli_inform(c("!" = "Please consider creating a permanent `{key}` location and store it in your `.Renviron` file."))
      }
      arg <- rlang::set_names(x = tempOmopFolder, nm = key)
      do.call(what = Sys.setenv, args = as.list(arg))
    }
    return(Sys.getenv(key))
  } else {
    assertCharacter(x = path, length = 1)
    if (!dir.exists(path)) {
      cli::cli_inform(c("i" = "Creating {.path {path}}."))
      dir.create(path)
    }
    arg <- rlang::set_names(x = path, nm = key)
    do.call(what = Sys.setenv, args = as.list(arg))
    if (rlang::is_interactive()) {
      c("i" = "If you want to create a permanent `{key}` write the following in your `.Renviron` file:",
        "", " " = "{.pkg {key}}=\"{path}\"", "") |>
        cli::cli_inform()
    }
    return(invisible(Sys.getenv(key)))
  }
}
