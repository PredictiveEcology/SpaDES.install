utils::globalVariables(c("..colsToShow", "compareVersion"))

#' Pre-test for packages in SpaDES modules
#'
#' This function can be put at the start of project code.
#' It will only be necessary if there are multiple `simInit` calls (i.e., the project doesn't use only one call).
#' It will check all modules in `modulePath` for package dependencies.
#' It will prompt.
#'
#' @param modulePath The path to modules, as per `SpaDES.core::setPaths`.
#'
#' @export
#' @importFrom data.table setnames setorderv
#' @importFrom Require getPkgVersions Require
#' @importFrom utils install.packages packageVersion
#'
#' @examples
#' \dontrun{
#' out <- makeSureAllPackagesInstalled(modulePath = "modules")
#' }
makeSureAllPackagesInstalled <- function(modulePath) {
  AllPackagesFile <- "._AllPackages.rds" ## TODO: put this in proper place
  if (!file.exists(AllPackagesFile)) {
    AllModules <- dir(modulePath)
    if (!requireNamespace("SpaDES.core", quietly = TRUE)) {
      ## installing SpaDES.core will also install reproducible
      message("Need to install SpaDES.core from CRAN to continue")
      Require("SpaDES.core")
    }
    AllPackages <- lapply(AllModules, function(mod) {
      print(mod)
      SpaDES.core::packages(modules = mod, paths = modulePath)
    })

    AllPackagesUnlisted <- unname(unlist(AllPackages))
    out <- Require::Require(require = FALSE, AllPackagesUnlisted, install = FALSE, verbose = TRUE)
    out <- attr(out, "Require")
    okVersions <- Require::getPkgVersions(out)
    okInstalled <- all(out$installed)
    okVersion <- all(okVersions$compareVersion >= 0)
    data.table::setorderv(out, "Package")
    data.table::setnames(out, old = "Version", "InstalledVersion")
    colsToShow <- c("packageFullName", "Package", "InstalledVersion", "correctVersion")
    out <- out[compareVersion < 0, ..colsToShow]

    uniquedPkgs <- unique(out$Package)
    anyLoaded <- vapply(uniquedPkgs, function(pkg) isNamespaceLoaded(pkg), FUN.VALUE = logical(length(uniquedPkgs)))

    if (!all(okVersion & okInstalled & anyLoaded)) {
      obj <- list(state = out, AllPackagesUnlisted = AllPackagesUnlisted)
      saveRDS(obj, file = AllPackagesFile)
      message("The following packages are in an incorrect state: ")
      reproducible::messageDF(print(out))
      stop("Restart R; Run this function again immediately.", call. = FALSE)
    }
  } else {
    AllPackagesUnlisted <- readRDS(AllPackagesFile)
    uniquedPkgs <- unique(AllPackagesUnlisted$state$Package)
    anyLoaded <- vapply(uniquedPkgs, function(pkg) isNamespaceLoaded(pkg), FUN.VALUE = logical(length(uniquedPkgs)))

    if (anyLoaded)
      stop("Some packages that need to be updated are still loaded; please restart R.",
           "You may have to change your settings so packages don't get automatically loaded")
    Require::Require(require = FALSE, AllPackagesUnlisted$AllPackagesUnlisted, upgrade = FALSE)
    unlink(AllPackagesFile)
  }
}
