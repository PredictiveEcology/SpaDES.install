#' Setup for binary linux repositories
#'
#' This function is run for its side effect on the options("repos") and the options("HTTPUserAgen")
#'
#' @param CRAN The default CRAN repository to use, when not using a binary Linux repo
#' @param binaryLinux A binary linux repository.
#'
#'
#' @export
setLinuxBinaryRepo <- function(CRAN = "https://cloud.r-project.org", binaryLinux = "https://packagemanager.rstudio.com/") {
  local({
    options("repos" = c(CRAN = CRAN))

    if (Sys.info()["sysname"] == "Linux" && grepl("Ubuntu", utils::osVersion)) {
      .user.agent <- paste0(
        "R/", getRversion(), " R (",
        paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"]),
        ")"
      )

      .os.version <- system("lsb_release -cs", intern = TRUE)
      options(
        repos = c(
          CRAN = if (!grepl("R Under development", R.version.string) && getRversion() >= "4.1") {
            paste0(binaryLinux, "all/__linux__/", .os.version, "/latest")
          } else {
            CRAN
          }
        )
      )

      options(HTTPUserAgent = .user.agent)
    }
  })
}
