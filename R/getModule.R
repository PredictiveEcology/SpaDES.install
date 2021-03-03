#' Simple function to download a SpaDES module as GitHub repository
#'
#' @param gitRepo A github repository, presented in the standard R way, with
#'   `account/repository@branch`
#' @param overwrite Logical. If \code{TRUE}, then the download will delete any
#'   existing folder with the same name as the \code{repository}
#'   provided in \code{gitRepo}
#' @param modulePath A local path in which to place the full module, within
#'   a subfolder ... i.e., the source code will be downloaded to here:
#'   \code{file.path(modulePath, repository)}
#'
#' @export
#' @importFrom utils download.file unzip
getModule <- function(gitRepo, overwrite = FALSE, modulePath = ".") {
  if (!dir.exists(modulePath)) dir.create(modulePath, recursive = TRUE)
  gr <- splitGitRepo(gitRepo)
  ar <- file.path(gr$acct, gr$repo)
  repoFull <- file.path(modulePath, gr$repo)
  zipFileName <- normalizePath(paste0(repoFull, ".zip"), winslash = "/", mustWork = FALSE)
  for (i in 1:2) {
    url <- paste0("http://github.com/", ar, "/archive/", gr$br, ".zip")
    out <- try(download.file(url, destfile = zipFileName))
    if (is(out, "try-error") && identical(gr$br, "master"))
      gr$br <- "main"
    else
      break
  }
  out <- unzip(zipFileName, exdir = modulePath) # unzip it
  if (dir.exists(repoFull))
    if (isTRUE(overwrite)) {
      unlink(repoFull, recursive = TRUE)
    } else {
      stop(repoFull, " directory already exists. Use overwrite = TRUE if you want to overwrite it")
    }
  badDirname <- unique(dirname(out))[1]
  file.rename(badDirname, gsub(basename(badDirname), gr$repo, badDirname)) # it was downloaded with a branch suffix
  unlink(zipFileName)
  message(gitRepo, " downloaded and placed in ", normalizePath(repoFull, winslash = "/"))
  possRmd <- normalizePath(winslash = "/", file.path(repoFull, paste0(gr$repo, ".Rmd")), mustWork = FALSE)
  if (file.exists(possRmd))
    message("To run it, try: \nfile.edit('", possRmd,"')")
  return(normalizePath(repoFull))
}
