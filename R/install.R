.pkgEnv <- new.env(parent = emptyenv())

splitGitRepo <- function(gitRepo) {
  grSplit <- strsplit(gitRepo, "/|@")[[1]]
  grAcct <- strsplit(gitRepo, "/")[[1]] # only account and repo
  if (length(grAcct) == 1) {
    acct <- "PredictiveEcology"
    grSplit <- append(list(acct), grSplit)
  } else {
    acct <- grSplit[[1]]
  }
  repo <- grSplit[[2]]
  if (length(grSplit) > 2) {
    br <- grSplit[[3]]
  } else {
    br <- "main"
  }
  list(acct = acct, repo = repo, br = br)
}

#' Install R Package from GitHub source code
#'
#' A lightweight alternative to `remotes::install_github`.
#' All dependencies must have been installed already for this to work.
#'
#' @param gitRepo A repository in the form: `Account/Repository@Branch` or `Account/Repository@SHA`.
#'
#' @param libPath The folder where you would like the package installed.
#'   Defaults to `.libPaths()[1]`.
#'
#' @export
installGithubPackage <- function(gitRepo, libPath = .libPaths()[1]) {
  gr <- splitGitRepo(gitRepo)
  modulePath <- file.path(tempdir(), paste0(sample(LETTERS, 8), collapse = ""))
  dir.create(modulePath, recursive = TRUE)

  ## deal with dependencies
  workaround <- DESCRIPTIONFileVersionV(Require::getGitHubDESCRIPTION(gitRepo)$DESCFile)
  Deps <- DESCRIPTIONFileDeps(Require::getGitHubDESCRIPTION(gitRepo)$DESCFile)

  Require(Deps, require = FALSE, install = TRUE)

  out <- getModule(gitRepo, overwrite = TRUE, modulePath = modulePath)
  orig <- setwd(modulePath)
  if (nchar(Sys.which("R")) > 0) {
    out1 <- system(paste("R CMD build ", gr$repo), intern = TRUE)
    buildingLine <- grep("building", out1, value = TRUE)
    packageTarName <- strsplit(buildingLine, "'")[[1]][2]
    if (is.na(packageTarName)) { # linux didn't have that character
      packageTarName <- gsub(paste0("^.*(", gr$repo, ".*tar.gz).*$"), "\\1", buildingLine)
    }
    system(paste0("R CMD INSTALL --library=", normalizePath(libPath, winslash = "/"), " ",
                  packageTarName), wait = TRUE)
  } else {
    message("Can't install packages this way because R is not on the search path")
  }
}

#' @rdname installGithubPackage
#' @export
installGitHubPackage <- installGithubPackage

#' Install SpaDES packages, making sure to `update.packages` first
#'
#' @param type passed to both `update.packages` and `install.packages`.
#'   This will be set to `"binary"` on Windows, if not set, to get the binary packages from CRAN.
#'
#' @param fromSource A character vector of packages that must be installed from
#'    source on Linux-alikes, even if the `options("repos")` is pointing to a binary
#'    repository of binary packages. The default is all the spatial packages, plus
#'    a few others. Has no effect if
#'
#' @param libPath Passed to both `update.packages(lib.lob = libPath)` and
#'   `install.packages(lib = libPath, ...)`
#'
#' @param versions named list of package versions to install
#'
#' @param dontUpdate character vector of packages not to update
#'
#' @param upgrade Logical or character ("default", "ask", "never", "always").
#'   If `TRUE` or `"always"`, then `update.packages` will be run on installed packages,
#'   and any packages that are not their most recent will be installed with their
#'   current version on CRAN.
#'
#' @param SpaDES.project logical. If TRUE, the default, then the SpaDES.project will
#'   also be installed. This is not on CRAN. It will first attempt to install from
#'   predictiveecology.r-universe.dev. If that fails, then it will install from source
#'   from github.com/PredictiveEcology/SpaDES.project
#'
#' @param ... Passed to deprecated arguments (currently: `ask`)
#'
#' @export
#' @importFrom utils install.packages installed.packages old.packages packageVersion tail
installSpaDES <- function(type, libPath = .libPaths()[1],
                          fromSource = sourcePkgs(),
                          versions = c("SpaDES.core (>= 1.0.9)", "SpaDES.tools (>= 0.3.9)"),
                          dontUpdate = c("scam"),
                          upgrade = c("default", "ask", "always", "never"),
                          SpaDES.project = TRUE, ...) {
  # srch <- loadedNamespaces()
  basePkgs <- dir(tail(.libPaths(), 1))
  basePkgs <- c(basePkgs, "GlobalEnv", "Autoloads")
  # nonBase <- lapply(basePkgs, function(bp) {
  #   srch <<- grep(bp, srch, value = TRUE, invert = TRUE)
  # })

  SpaDES.installDeps <- Require::extractPkgName(
    Require::pkgDep("PredictiveEcology/SpaDES.install", recursive = TRUE)[[1]]
  )
  # srch <- setdiff(srch,
  #                 paste0(c("SpaDES.install",
  #                            SpaDES.installDeps)))
  #
  # mayNeedRestart <- length(srch) > 0
  # message(
  #   "It looks like you may need to restart your R session to get an R session without ",
  #   "R packages loaded already. SpaDES.install needs to be the only package loaded. ",
  #   "If you are using RStudio and you are unable to restart without",
  #   "lots of R packages being pre-loaded, you may need to run this from a non-RStudio",
  #   " R session."
  # )
  # out <- readline("Do you want to proceed anyway (Y or N)? ")
  # if (!identical("y", tolower(out))) {
  #   stop("Try to restart R with Ctrl-Alt-F10 if you are in RStudio")
  # }
  writeable <- unlist(lapply(.libPaths(), file.access, mode = 2)) == 0

  ask <- isTRUE(list(...)$ask) || upgrade[1] == "ask"
  args <- list(checkBuilt = TRUE, ask = ask)
  if (any(writeable)) {
    libPathsForUpdate <- .libPaths()[writeable]
    args$lib.loc <- libPathsForUpdate
  }
  isWin <- identical("windows", .Platform$OS.type)
  if (isWin && missing(type)) {
    args$type <- "binary"
  }

  hasOlds <- tail(sort(ls(envir = .pkgEnv, pattern = "olds")), 1)
  needNew <- TRUE
  if (length(hasOlds) > 0) {
    now <- Sys.time()
    oldTime <- strsplit(hasOlds, "__")[[1]][2]
    age <- difftime(now, oldTime)
    if (age < 3600) {
      needNew <- FALSE
    }
  }
  if (needNew) {
    olds <- do.call(old.packages, args)
    hasOlds <- paste0("olds", "__", format(Sys.time()))
    assign(hasOlds, olds, envir = .pkgEnv)
  } else {
    olds <- get(hasOlds, envir = .pkgEnv)
  }
  removeCache <- FALSE
  on.exit({
    if (isTRUE(removeCache))
      rm(list = hasOlds, envir = .pkgEnv)
  })

  spadesPkgs <- c("SpaDES.core", "SpaDES.tools")
  deps <- unique(unlist(Require::pkgDep(spadesPkgs, recursive = TRUE)))
  if (!is.null(olds) && (any(upgrade[1] %in% c("default", "always", "ask"))) ||
      isTRUE(upgrade[1])) {
    upgrade <- TRUE
    oldsAreSpaDESDeps <- olds[, "Package"] %in% Require::extractPkgName(deps)
    if (any(oldsAreSpaDESDeps)) {
      olds <- olds[oldsAreSpaDESDeps,, drop = FALSE]
      ## Don't try to update SpaDES.install dependencies (currently: Require, data.table, remotes)
      toUpdate <- setdiff(olds[, "Package"], dontUpdate)
      dontUpdate <- intersect(SpaDES.installDeps, toUpdate)
      if (length(dontUpdate)) {
        toUpdate <- setdiff(toUpdate, dontUpdate)
      }
      if (length(toUpdate)) {
        toUpdateDF <- as.data.frame(olds[olds[,"Package"] %in% toUpdate,, drop = FALSE])
        toUpdateDF <- toUpdateDF[, c("Package", "Installed", "ReposVer")]
        toUpdateDF <- data.frame(toUpdateDF$Package, toUpdateDF$Installed, "-->", toUpdateDF$ReposVer)
        colnames(toUpdateDF) <- c("Package", "Installed Version", "  ", "Available Version")
        messageDF(toUpdateDF)
        if (isTRUE(ask)) {
          answer <- readline("Proceed with upgrades to packages? (Y or N)")
          answer <- tolower(answer)
          if (answer == "n") upgrade = FALSE
        }
        if (isTRUE(upgrade)) {
          loadedNS <- setdiff(loadedNamespaces(), basePkgs)  # rerun because Rstudio loads behind the scenes
          if (length(intersect(loadedNS, toUpdate))) {
            message(restartMess)
            out <- readline("Do you want to proceed anyway (Y or N)? ")
            if (!identical("y", tolower(out))) stop(restartMessAtStop)
          }
          message("updating packages ... ",
                  paste(toUpdate, collapse = ", "))
          Require(toUpdate, libPaths = libPath, dependencies = FALSE,
                  require = FALSE, install = "force")
          removeCache <- TRUE
        }
        # args[["pkgs"]] <- toUpdate
        # args[["dependencies"]] <- FALSE
        # do.call(install.packages, args)
      }
    }
  }

  #  install
  args <- list(spadesPkgs, dependencies = TRUE)
  if (isWin && missing(type)) {
    args$type <- "binary"
  }

  if (!isWin && any(!dir.exists(file.path(.libPaths()[1], fromSource)))) {
    removeCache <- TRUE
    loadedNS <- setdiff(loadedNamespaces(), basePkgs)  # rerun because Rstudio loads behind the scenes
    if (length(intersect(loadedNS, fromSource))) {
      message(restartMess)
      out <- readline("Do you want to proceed anyway (Y or N)? ")
      if (!identical("y", tolower(out))) stop(restartMessAtStop)
    }

    installSourcePackages(fromSource = fromSource, libPath = libPath)
  }

  if (length(args[[1]])) {
    pkgsToInstall <- unique(c(args[[1]], deps))
    if (length(versions) > 0) {
      mm <- match(Require::extractPkgName(versions), pkgsToInstall)
      pkgsToInstall[mm] <- versions
    }

    if (isTRUE(SpaDES.project)) {
      pkgsToInstall <- c(pkgsToInstall, "PredictiveEcology/SpaDES.project")
    }
    # First check if need anything installed
    anything <- Require(pkgsToInstall, require = FALSE, libPaths = libPath,
                        install = FALSE, upgrade = FALSE, verbose = TRUE)
    pkgSituation <- attr(anything, "Require")
    if (!all(pkgSituation$installed)) {
      needInstalled <- pkgSituation[!pkgSituation$installed]
      loadedNS <- setdiff(loadedNamespaces(), basePkgs) # rerun because Rstudio loads behind the scenes
      if (length(intersect(loadedNS, needInstalled$Package))) {
        message(restartMess)
        out <- readline("Do you want to proceed anyway (Y or N)? ")
        if (!identical("y", tolower(out))) stop(restartMessAtStop)
      }
      anything <- Require(pkgsToInstall, require = FALSE, libPaths = libPath, upgrade = FALSE,
                          verbose = TRUE)
      if (any(!is.na(attr(anything, "Require")$installFrom)))
        removeCache <- TRUE
    }
  }

  return(invisible())
}

#' Install spatial packages
#'
#' (Re)install spatial packages that require GDAL/GEOS/PROJ, from source to ensure they are properly
#' liked to these external libraries.
#' This uses \code{Require} internally, so it will will not upgrade existing packages.
#' If upgrades are needed, remove them first, e.g. with
#' \code{remove.packages(c("rgeos", "sp", "raster", "terra", "lwgeom", "sf", "rgdal"))}.
#'
#' @param pkgs Character vector of package names to install using `type = "source"`
#'
#' @param repos URL of CRAN mirror to use to fetch source packages
#'
#' @inheritParams installSpaDES
#'
#' @param forceSourceOnWindows Logical. If `TRUE`, then this will install from source on Windows,
#'   which is often unnecessary for e.g., spatial packages.
#'
#' @note if installing on macOS, homebrew installation of GDAL etc. is required.
#'
#' @export
installSpatialPackages <- function(pkgs = c("rgeos", "sp", "raster", "terra", "lwgeom", "sf", "rgdal"),
                                   repos = "https://cloud.r-project.org",
                                   libPath = .libPaths()[1],
                                   forceSourceOnWindows = FALSE) {
  ## rgdal and sf need additional args for homebrew on macOS
  origPkgs <- pkgs
  if (Sys.info()[["sysname"]] == "Darwin") {
    stopifnot(nzchar(Sys.which("brew")))

    install.packages("rgdal", type = "source", repos = repos,
                     configure.args = c("--with-proj-lib=/usr/local/lib/",
                                        "--with-proj-include=/usr/local/include/"))
    install.packages("sf", type = "source", repos = repos,
                     configure.args = "--with-proj-lib=/usr/local/lib/")
    pkgs <- setdiff(pkgs, c("rgdal", "sf"))
  }
  installSourcePackages(pkgs, repos = repos, libPath = libPath, forceSourceOnWindows = forceSourceOnWindows)

  return(invisible())
}

#' Install source packages
#'
#' (Re)install source packages. Dependencies of these packages will not force source
#' installs, unless those dependencies are listed in \code{fromSource}. Note, this function
#' only has real consequences if the default repository in \code{options(CRAN = "someURL")}
#' is set to a binary package repository, and will not affect Windows machines
#' unless \code{forceSourceOnWindows} is \code{TRUE}.
#'
#' @inheritParams installSpaDES
#'
#' @param repos URL of CRAN mirror to use to fetch source packages
#'
#' @param forceSourceOnWindows Logical. If `TRUE`, then this will install from source on Windows,
#'   which is often unnecessary for e.g., spatial packages.
#'
#' @note if installing on macOS, homebrew installation of GDAL etc. is required.
#'
#' @export
installSourcePackages <- function(fromSource = sourcePkgs(),
                                  libPath = .libPaths()[1], repos = "https://cloud.r-project.org",
                                  forceSourceOnWindows = FALSE) {
  depsClean <- unlist(unname(Require::pkgDep(fromSource, recursive = TRUE)))
  depsCleanUniq <- sort(unique(Require::extractPkgName(depsClean)))
  depsCleanUniq <- setdiff(depsCleanUniq, fromSource)

  repos <- c(CRAN = repos[1])
  if (isWindows() && !isTRUE(forceSourceOnWindows)) {
    Require(unique(c(depsCleanUniq, fromSource)), type = "source", libPaths = libPath, repos = repos,
            dependencies = FALSE, require = FALSE, upgrade = FALSE)
  } else {
    Require(depsCleanUniq, dependencies = FALSE, libPaths = libPath, require = FALSE, upgrade = FALSE)
    # install.packages(depsCleanUniq, dependencies = FALSE, libPaths = libPath)
    # Source second
    # opt <- options("repos" = c(CRAN ="https://cran.rstudio.com", options("repos")$repos))
    # on.exit({
    #   options(opt)
    # }, add = TRUE)
    Require(fromSource, type = "source", libPaths = libPath, repos = repos,
            dependencies = FALSE, require = FALSE, upgrade = FALSE)
  }
  # options(opt)
}

extractDepsOnly <- function(pkgs) {
  depsClean <- unlist(unname(Require::pkgDep(pkgs, recursive = TRUE)))
  depsCleanUniq <- sort(unique(Require::extractPkgName(depsClean)))
  depsCleanUniq <- setdiff(depsCleanUniq, pkgs)
  depsCleanUniq
}

restartMess <- paste(
  "It looks like you may need to restart your R session to get an R session without",
  "R packages loaded already. SpaDES.install needs to be the only package loaded.",
  "If you are using RStudio and you are unable to restart without",
  "lots of R packages being pre-loaded, you may need to run this from a non-RStudio",
  "R session."
)
restartMessAtStop <- paste(
  "Try to restart R with",
  ifelse(identical(Sys.info()[["sysname"]], "Darwin"), "Command-Shift-F10", "Ctrl-Shift-F10"),
  "if you are in RStudio.")

#' Character vector of packages that must be installed from source in Linux-alikes
#'
#' @export
#' @param additional Character string indicating additional packages to add to the
#'   default set (to see default set, run \code{sourcePkgs()}).
#'
sourcePkgs <- function(additional = NULL)
  c("igraph", "rgeos", "rgdal", "terra", "sf", "units", "stringfish",
    "qs", "sp", "Rcpp", "RcppParallel", "cpp11", "lwgeom", additional)
