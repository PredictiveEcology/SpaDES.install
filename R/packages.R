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
makeSureAllPackagesInstalled <- function(modulePath, doInstalls = TRUE) {
  if (missing(modulePath)) modulePath <- getOption("spades.modulePath")
  if (is.null(modulePath)) stop("modulePath must be supplied")
  AllPackagesFile <- "._AllPackages.rds" ## TODO: put this in proper place
  if (!file.exists(AllPackagesFile)) {
    names(modulePath) <- modulePath
    AllModules <- lapply(modulePath, dir)
    if (length(unlist(AllModules)) == 0) return("No modules in that modulePath. All clear.")
    # AllModules <- dir(modulePath)
    # msg <- capture.output(type = "message",
    #                       hasSC <- requireNamespace("SpaDES.core"))
    # if (!hasSC) {
    #   ## installing SpaDES.core will also install reproducible
    #   if (any(grepl("there is no package.*SpaDES.core", msg))) {
    #     message("Need to install SpaDES.core from CRAN to continue")
    #   } else if (any(grepl("there is no package", msg))) {
    #     message(paste(msg, collapse = "\n"))
    #     message("Some dependencies of SpaDES.core are missing; installing those")
    #   }
    #
    #   out <- Require("SpaDES.core", require = FALSE, install = TRUE)
    # }
    AllPackages <- Map(am = AllModules, mp = names(AllModules), function(am, mp) {
      message(mp)
      out <- lapply(am, function(mod) {
        message("  ", mod)
        # SpaDES.core::packages(modules = mod, paths = mp)
        packagesInModules(modulePath = mp, modules = mod)
      })
      unlist(out[!unlist(lapply(out, function(o) NROW(o) == 0))], recursive = FALSE)
    })

    AllPackagesUnlisted <- unique(unname(unlist(AllPackages)))

    messageDF <- get("messageDF", envir = asNamespace("Require"))
    rmDuplicatePkgs <- get("rmDuplicatePkgs", envir = asNamespace("Require"))
    toPkgDT <- get("toPkgDT", envir = asNamespace("Require"))

    message("Modules need the following packages: \n",
            paste(sort(AllPackagesUnlisted), collapse = ", "))
    message("  ")
    message("Determining package dependencies...")
    AllPackagesUnlistedRecursive <- Require::pkgDep(AllPackagesUnlisted, recursive = TRUE)
    AllPackagesUnlistedRecursive <- unique(c(names(AllPackagesUnlistedRecursive),
                                             unlist(AllPackagesUnlistedRecursive)))
    unduplicateDT <- data.table::data.table(toPkgDT(sort(AllPackagesUnlistedRecursive)), installed = FALSE,
                                installFrom = "CRAN", installResult = TRUE)
    unduplicated <- suppressMessages(rmDuplicatePkgs(unduplicateDT)[duplicate == FALSE]$packageFullName)
    if (doInstalls) {
      out <- Require::Require(require = FALSE, unduplicated,
                              install = FALSE, verbose = TRUE)
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
        if (any(out[needAction == TRUE]$loaded)) {
          message("The following packages are in an incorrect state: ")
          messageDF(print(out[needAction == TRUE]))
          ans <- readline("Would you like to, unload packages and current environment, then restart R? (Y or N): ")
          if (startsWith(tolower(ans), "y")) {
            #if (!require("Require")) {install.packages("Require"); require("Require")}
            #Require("PredictiveEcology/SpaDES.install@development (>= 0.0.7.9000)")
            ln <- setdiff(unique(c(search(), loadedNamespaces())), .basePkgs)
            a <- suppressMessages(Require::pkgDepTopoSort(ln))
            message("Trying to detach packages and restart; it may cause R to crash; rerun command without loading any packages")
            out <- lapply(rev(names(a)), function(x) {
              try(detach(paste0("package:", x), unload = TRUE, character.only = TRUE), silent = TRUE)
              try(unloadNamespace(x), silent = TRUE)
            })
            rm(list = setdiff(ls(), "modulePath"))
            if (requireNamespace("rstudioapi", quietly = TRUE) && isRstudio()) {
              rstudioapi::restartSession(command = paste0("SpaDES.install::makeSureAllPackagesInstalled('",
                                                          modulePath, "')"))
            }
          }
          obj <- list(state = out, AllPackagesUnlisted = AllPackagesUnlisted)
          saveRDS(obj, file = AllPackagesFile)
          stop("Restart R; Run this function again immediately.", call. = FALSE)
        }
        Require::Require(out[needAction == TRUE]$packageFullName,
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
      return(unduplicated)
    }
  } else {
    AllPackagesUnlisted <- readRDS(AllPackagesFile)
    uniquedPkgs <- unique(AllPackagesUnlisted$state$Package)
    uniquedPkgs <- setdiff(uniquedPkgs, getFromNamespace(".basePkgs", "Require"))
    anyLoaded <- vapply(uniquedPkgs, function(pkg) isNamespaceLoaded(pkg), FUN.VALUE = logical(1))

    pd <- Require::extractPkgName(
      Require::pkgDep("PredictiveEcology/SpaDES.install", recursive = TRUE)[[1]]
    )
    anyLoaded <- anyLoaded[!names(anyLoaded) %in% pd]

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
