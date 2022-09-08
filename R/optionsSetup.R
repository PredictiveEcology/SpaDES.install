#' Setup for binary Linux repositories
#'
#' Enable use of binary package builds for Linux from the Rstudio Package Manager repo.
#' This will set the `repos` option, affecting the current R session.
#'
#' @param CRAN The default CRAN repository to use, when not using a binary Linux repo.
#' @param binaryLinux A CRAN repository serving binary Linux packages.
#'
#' @export
setLinuxBinaryRepo <- function(CRAN = getOption("repos", "https://cloud.r-project.org/"),
                               binaryLinux = "https://packagemanager.rstudio.com/") {
  options("repos" = c(CRAN = CRAN))

  if (Sys.info()["sysname"] == "Linux" && grepl("Ubuntu", utils::osVersion)) {
    options(
      repos = c(
        CRAN = if (!grepl("R Under development", R.version.string) && getRversion() >= "4.1") {
          paste0(binaryLinux, "all/__linux__/", system("lsb_release -cs", intern = TRUE), "/latest")
        } else {
          CRAN
        }
      )
    )
  }
}
