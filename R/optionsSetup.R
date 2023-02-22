#' Setup for binary Linux repositories
#'
#' Enable use of binary package builds for Linux from the Rstudio Package Manager repo.
#' This will set the `repos` option, affecting the current R session.
#'
#' @param binaryLinux A CRAN repository serving binary Linux packages.
#'
#' @export
setLinuxBinaryRepo <- function(binaryLinux = "https://packagemanager.rstudio.com/") {
  if (Sys.info()["sysname"] == "Linux" && grepl("Ubuntu", utils::osVersion)) {
    if (!grepl("R Under development", R.version.string) && getRversion() >= "4.1") {
      options(
        repos = c(
            paste0(binaryLinux, "all/__linux__/", system("lsb_release -cs", intern = TRUE), "/latest")
        )
      )
    }
  }
}
