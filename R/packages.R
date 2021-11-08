utils::globalVariables(c(":=", "..colsToShow", "compareVersion"))

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
#' @importFrom data.table setnames setorderv
#' @importFrom Require getPkgVersions Require
#' @importFrom utils getFromNamespace install.packages packageVersion
#'
#' @examples
#' \dontrun{
#' makeSureAllPackagesInstalled(modulePath = "modules")
#'
#' # Multiple modulePath
#' makeSureAllPackagesInstalled(c(
#'   "~/GitHub/WBI_fireSense/modules/",
#'   "~/GitHub/SpaDES-modules/modules/"
#' ))
#' }
makeSureAllPackagesInstalled <- function(modulePath) {
  if (missing(modulePath)) modulePath <- getOption("spades.modulePath")
  if (is.null(modulePath)) stop("modulePath must be supplied")
  AllPackagesFile <- "._AllPackages.rds" ## TODO: put this in proper place
  if (!file.exists(AllPackagesFile)) {
    names(modulePath) <- modulePath
    AllModules <- lapply(modulePath, dir)
    if (length(unlist(AllModules)) == 0) return("No modules in that modulePath. All clear.")
    # AllModules <- dir(modulePath)
    msg <- capture.output(type = "message",
                          hasSC <- requireNamespace("SpaDES.core"))
    if (!hasSC) {
      ## installing SpaDES.core will also install reproducible
      if (any(grepl("there is no package.*SpaDES.core", msg))) {
        message("Need to install SpaDES.core from CRAN to continue")
      } else if (any(grepl("there is no package", msg))) {
        message(paste(msg, collapse = "\n"))
        message("Some dependencies of SpaDES.core are missing; installing those")
      }

      out <- Require("SpaDES.core", require = FALSE, install = TRUE)
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

    # Note this will return NA if there is no version specification
    okVersions <- Require::getPkgVersions(out, install = FALSE)
    okInstalled <- out$installed
    okVersion <- okVersions$compareVersion >= 0
    if (length(okVersion) == 0)
      okVersion <- rep(TRUE, length(okInstalled))
    if (anyNA(okVersion)) {
      okVersion[is.na(okVersion)] <- TRUE
    }
    data.table::setorderv(out, "Package")
    data.table::setnames(out, old = "Version", "InstalledVersion")
    colsToShow <- c("packageFullName", "Package", "InstalledVersion", "correctVersion")
    toRm <- setdiff(colnames(out), colsToShow)
    data.table::set(out, NULL, toRm, NULL)

    uniquedPkgs <- unique(out$Package)
    anyLoaded <- vapply(uniquedPkgs, function(pkg) isNamespaceLoaded(pkg), FUN.VALUE = logical(1))
    dtLoaded <- data.table::data.table(Package = names(anyLoaded), loaded = anyLoaded)
    out <- dtLoaded[out, on = "Package"]
    needAction <- !okVersion | !okInstalled
    if (any(needAction)) {
      data.table::set(out, NULL, "needAction", FALSE)
      out[needAction, needAction := TRUE]

      doInstallsNow <- FALSE
      if (all((needAction) & out$loaded)) {
        obj <- list(state = out, AllPackagesUnlisted = AllPackagesUnlisted)
        saveRDS(obj, file = AllPackagesFile)
        message("The following packages are in an incorrect state: ")
        reproducible::messageDF(print(out[needAction == TRUE]))
        stop("Restart R; Run this function again immediately.", call. = FALSE)
      }
      Require::Require(out[needAction]$packageFullName,
        require = FALSE,
        upgrade = FALSE
      )
    } else {
      message(
        "All 'reqdPkgs' in the modules in ", paste(modulePath, collapse = ", "),
        " are installed with correct version"
      )
    }
  } else {
    AllPackagesUnlisted <- readRDS(AllPackagesFile)
    uniquedPkgs <- unique(AllPackagesUnlisted$state$Package)
    uniquedPkgs <- setdiff(uniquedPkgs, getFromNamespace(".basePkgs", "Require"))
    anyLoaded <- vapply(uniquedPkgs, function(pkg) isNamespaceLoaded(pkg), FUN.VALUE = logical(1))

    if (any(anyLoaded)) {
      stop(
        "Some packages that need to be updated are still loaded; please restart R.",
        "You may have to change your .Rprofile or Rstudio settings so ",
        "packages don't get automatically loaded"
      )
    }
    Require::Require(require = FALSE, AllPackagesUnlisted$AllPackagesUnlisted, upgrade = FALSE)
    unlink(AllPackagesFile)
  }
}
