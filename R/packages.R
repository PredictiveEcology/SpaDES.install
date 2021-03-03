utils::globalVariables(c("..colsToShow", "compareVersion"))

#' Pre-test for packages in SpaDES modules
#'
#' This function can be put at the start of project code.
#' It will only be necessary if there are multiple `simInit` calls (i.e., the project doesn't use only one call).
#' It will check all modules in `modulePath` for package dependencies.
#' It will prompt.
#'
#' @param modulePath The path to modules, as per `SpaDES.core::setPaths`. Can
#'   be a vector of multiple character strings representing multiple locations
#'   of modules.
#'
#' @export
#' @importFrom Require getPkgVersions Require
#' @importFrom utils install.packages packageVersion
#'
#' @examples
#' \dontrun{
#' makeSureAllPackagesInstalled(modulePath = "modules")
#'
#' # Multiple modulePath
#' makeSureAllPackagesInstalled(c("~/GitHub/WBI_fireSense/modules/",
#'                                "~/GitHub/SpaDES-modules/modules/"))
#' }
makeSureAllPackagesInstalled <- function(modulePath) {
  AllPackagesFile <- "._AllPackages.rds" ## TODO: put this in proper place
  if (!file.exists(AllPackagesFile)) {
    names(modulePath) <- modulePath
    AllModules <- lapply(modulePath, dir)
    # AllModules <- dir(modulePath)
    if (!requireNamespace("SpaDES.core", quietly = TRUE)) {
      ## installing SpaDES.core will also install reproducible
      message("Need to install SpaDES.core from CRAN to continue")
      Require("SpaDES.core")
    }
    AllPackages <- Map(am = AllModules, mp = names(AllModules), function(am, mp) {
      message(mp)
      lapply(am, function(mod) {
        message("  ", mod)
        SpaDES.core::packages(modules = mod, paths = mp)
      })
    })

    AllPackagesUnlisted <- unname(unlist(AllPackages))
    out <- Require::Require(require = FALSE, AllPackagesUnlisted, install = FALSE, verbose = TRUE)
    out <- attr(out, "Require")
    okVersions <- Require::getPkgVersions(out)
    okInstalled <- all(out$installed)
    okVersion <- all(okVersions$compareVersion >= 0)
    if (!requireNamespace("data.table", quietly = TRUE)) {stop("Please install.packages('data.table')")}
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
    message("All 'reqdPkgs' in the modules in ",paste(modulePath, collapse = ", "),
            " are installed with correct version")
  } else {
    AllPackagesUnlisted <- readRDS(AllPackagesFile)
    uniquedPkgs <- unique(AllPackagesUnlisted$state$Package)
    anyLoaded <- vapply(uniquedPkgs, function(pkg) isNamespaceLoaded(pkg), FUN.VALUE = logical(length(uniquedPkgs)))

    if (anyLoaded)
      stop("Some packages that need to be updated are still loaded; please restart R.",
           "You may have to change your .Rprofile or Rstudio settings so ",
           "packages don't get automatically loaded")
    Require::Require(require = FALSE, AllPackagesUnlisted$AllPackagesUnlisted, upgrade = FALSE)
    unlink(AllPackagesFile)
  }
}
