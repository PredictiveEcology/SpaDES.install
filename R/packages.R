utils::globalVariables(c(":=", "..colsToShow", "compareVersion", "duplicate"))

.basePkgs <- getFromNamespace(".basePkgs", "Require")

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
#' @param doInstalls logical. If `TRUE`, the default, then this function will attempt to do the
#'   package installations. If `FALSE`, then this function returns the character
#'   vector of packages (and version information) that is unduplicated, i.e.,
#'   the one with a highest minimum version specification.
#'
#' @export
#' @importFrom data.table setnames setorderv
#' @importFrom Require Require
#' @inheritParams Require::Require
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
makeSureAllPackagesInstalled <- function(modulePath, doInstalls = TRUE, verbose = getOption("Require.verbose")) {
  if (missing(modulePath)) modulePath <- getOption("spades.modulePath")
  if (is.null(modulePath)) stop("modulePath must be supplied")
  names(modulePath) <- modulePath
  AllModules <- lapply(modulePath, dir)
  if (length(unlist(AllModules)) == 0) return("No modules in that modulePath. All clear.")
  AllModules[lengths(AllModules) == 0] <- NULL

  AllPackages <- Map(am = AllModules, mp = names(AllModules), function(am, mp) {
    message(mp)
    out <- lapply(am, function(mod) {
      message("  ", mod)
      packagesInModules(modulePath = mp, modules = mod)
    })
    unlist(out[!unlist(lapply(out, function(o) NROW(o) == 0))], recursive = FALSE)
  })

  pkgs <- unique(unname(unlist(AllPackages)))
  if (isTRUE(doInstalls) && length(pkgs) > 0) {
    out <- Require(pkgs, require = FALSE, verbose = max(2, verbose))
    messagePkgsInstalled(out, verbose = verbose)
  }
  return(AllPackages)
}


messagePkgsInstalled <- function(out, verbose = getOption("Require.verbose")) {
  out <- attr(out, "Require")
  browser()
  failed <- out[installed %in% FALSE & isPkgInstalled %in% FALSE]
  basePkgs <- out$Package %in% Require:::.basePkgs
  outUnique <- unique(out, by = "Package")
  numPkgsNeeded <- NROW(outUnique)
  numPkgsInstalled <- NROW(outUnique[isPkgInstalled %in% TRUE])

  messageVerbose(Require:::green("Needed ", numPkgsNeeded - sum(basePkgs), " packages. ",
                                 "Of those, ", numPkgsInstalled, " already installed."),
                 verbose = verbose)
  success <- out[installResult %in% "OK" & isPkgInstalled %in% FALSE]
  if (NROW(success)) {
    messageVerbose(
      Require:::green("Successfully installed: ", paste0(success$packageFullName, collapse = ", ")),
      verbose = verbose)
  }
  if (NROW(failed)) {
    toPrint <- failed[, list(Package, packageFullName, availableVersionOK, installResult)]
    messageDF(toPrint, verbose = verbose)
  }
}


#' Extract element from SpaDES module metadata
#'
#' Parses module code, looking for the `metadataItem` (default = `"reqdPkgs"`)
#' element in the `defineModule` function.
#'
#' @param modules character vector of module names
#'
#' @param modulePath path to directory containing the module(s) named in `modules`
#'
#' @return A character vector of sorted, unique packages that are identified in all named
#' modules, or if `modules` is omitted, then all modules in `modulePath`.
#'
#' @export
#' @rdname metadata
packagesInModules <- function(modules, modulePath = getOption("spades.modulePath")) {
  metadataInModules(modulePath = modulePath, modules = modules, metadataItem = "reqdPkgs")
}

#' @param metadataItem character identifying the metadata field to extract
#'
#' @param needUnlist logical indicating whether to `unlist` the resulting metadata look up
#'
#' @export
#' @rdname metadata
metadataInModules <- function(modules, metadataItem = "reqdPkgs",
                              modulePath = getOption("spades.modulePath"), needUnlist) {
  if (missing(modules))
    modules <- dir(modulePath)
  names(modules) <- modules

  if (any(metadataItem %in% c("inputObjects", "outputObjects", "parameters"))) {
    if (!requireNamespace("SpaDES.core", quietly = TRUE)) {
      stop("To evaluate that metadataItem, please install package 'SpaDES.core'.")
    }
  }
  if (missing(needUnlist)) {
    needUnlistInner <- switch(metadataItem, reqdPkgs = TRUE, version = FALSE, authors = FALSE, FALSE)
    needUnlistOuter <- switch(metadataItem, reqdPkgs = FALSE, version = TRUE, authors = FALSE, FALSE)
  } else {
    needUnlistInner <- needUnlistOuter <- needUnlist
  }

  vals <- lapply(modules, function(mod) {
    for (i in 1:2) {
      modPath <- file.path(modulePath, mod, paste0(mod, ".R"))
      feMP <- file.exists(modPath)
      if (!feMP)
        modulePath <- "."
      else
        break
    }
    if (feMP) {
      pp <- parse(file = modPath)
      wh <- unlist(lapply(pp, grep, pattern = "defineModule"))
      wh2 <- which(unlist(lapply(pp[[1]], function(x)
        any(grepl(pattern = metadataItem, format(x))))))
      if (length(wh2)) {
        val <- eval(pp[[wh]][[wh2]][[metadataItem]])
        if (identical(metadataItem, "version")) {
          val <- lapply(val, as.character)
          hasSpaDES.core <- names(val) == "SpaDES.core"
          val <- unname(val)
          if (any(hasSpaDES.core))
            val <- val[!hasSpaDES.core]
        }
        if (needUnlistInner)
          val <- unlist(val)
      } else {
        message("Skipping ", metadataItem, " in ", modules, "; it is empty")
        val <- NULL
      }
      val
    }
  })
  vals <- vals[!unlist(lapply(vals, is.null))]

  if (needUnlistOuter) {
    vals2 <- unlist(vals, recursive = FALSE)
    dups <- duplicated(vals2)
    vals <- try(sort(vals2[!dups]), silent = TRUE)
    if (is(vals, "try-error"))
      vals <- vals2[!dups]
  }
  vals
}

isRstudio <- function() {
  Sys.getenv("RSTUDIO") == 1 || .Platform$GUI == "RStudio" ||
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      rstudioapi::isAvailable()
    }
  else {
    FALSE
  }
}
