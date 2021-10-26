splitGitRepo <- function(gitRepo) {
  grSplit <- strsplit(gitRepo, "/|@")[[1]]
  acct <- grSplit[[1]]
  repo <- grSplit[[2]]
  if (length(grSplit) > 2) {
    br <- grSplit[[3]]
  } else {
    br <- "master"
  }
  list(acct = acct, repo = repo, br = br)
}

#' Install R Package from GitHub source code
#'
#' A lightweight alternative to `devtools::install_github`.
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
  out <- getModule(gitRepo, overwrite = TRUE, modulePath = modulePath)
  orig <- setwd(modulePath)
  if (nchar(Sys.which("R")) > 0) {
    out1 <- system(paste("R CMD build ", gr$repo), intern = TRUE)
    buildingLine <- grep("building", out1, value = TRUE)
    packageTarName <- strsplit(buildingLine, "'")[[1]][2]
    if (is.na(packageTarName)) { # linux didn't have that character
      packageTarName <- gsub(paste0("^.*(", gr$repo, ".*tar.gz).*$"), "\\1", buildingLine)
    }
    system(paste0("R CMD INSTALL --library=", normalizePath(libPath, winslash = "/"), " ", packageTarName), wait = TRUE)
  } else {
    message("Can't install packages this way because R is not on the search path")
  }
}

#' @rdname installGithubPackage
#' @export
installGitHubPackage <- installGithubPackage

#' Install SpaDES packages, making sure to `update.packages` first
#'
#' @param ask Passed to `update.packages`.
#'
#' @param type passed to both `update.packages` and `install.packages`.
#'   This will set `"binary"` on Windows, if not set, to get the binary packages from CRAN.
#'
#' @param libPath Passed to both `update.packages(lib.lob = libPath)` and
#'   `install.packages(lib = libPath, ...)`
#'
#' @param versions named list of package versions to install
#'
#' @param dontUpdate character vector of packages not to update
#'
#' @param SpaDES.project logical. If TRUE, the default, then the SpaDES.project will
#'   also be installed. This is not on CRAN. It will first attempt to install from
#'   predictiveecology.r-universe.dev. If that fails, then it will install from source
#'   from github.com/PredictiveEcology/SpaDES.project
#'
#' @export
#' @importFrom utils install.packages installed.packages old.packages packageVersion tail
installSpaDES <- function(ask = FALSE, type, libPath = .libPaths()[1],
                          versions = c(SpaDES.core = "1.0.8", SpaDES.tools = "0.3.6"),
                          dontUpdate = c("scam"), SpaDES.project = TRUE) {
  srch <- search()
  basePkgs <- dir(tail(.libPaths(), 1))
  basePkgs <- c(basePkgs, "GlobalEnv", "Autoloads")
  nonBase <- lapply(basePkgs, function(bp) {
    srch <<- grep(bp, srch, value = TRUE, invert = TRUE)
  })

  SpaDES.installDeps <- Require::extractPkgName(
    Require::pkgDep("SpaDES.install", recursive = TRUE)[[1]]
  )
  srch <- setdiff(srch,
                  paste0("package:",
                         c("SpaDES.install",
                             SpaDES.installDeps)))

  if (length(srch) > 0) {
    message(
      "It looks like you may need to restart your R session to get an R session without ",
      "R packages loaded already. SpaDES.install needs to be the only package loaded. ",
      "If you are using RStudio and you are unable to restart without",
      "lots of R packages being pre-loaded, you may need to run this from a non-RStudio",
      " R session."
    )
    out <- readline("Do you want to proceed anyway? Y or N")
    if (!identical("y", tolower(out))) {
      stop("Try to restart R with Ctrl-Alt-F10 if you are in RStudio")
    }
  }
  writeable <- unlist(lapply(.libPaths(), file.access, mode = 2)) == 0
  args <- list(checkBuilt = TRUE, ask = ask)
  if (any(writeable)) {
    libPathsForUpdate <- .libPaths()[writeable]
    args$lib.loc <- libPathsForUpdate
  }
  isWin <- identical("windows", .Platform$OS.type)
  if (isWin && missing(type)) {
    args$type <- "binary"
  }
  olds <- do.call(old.packages, args)
  if (!is.null(olds)) {

    # Don't try to update the dependencies of SpaDES.install (which are currently Require, data.table, remotes)
    toUpdate <- setdiff(olds[, "Package"], dontUpdate)
    dontUpdate <- intersect(SpaDES.installDeps, toUpdate)
    if (length(dontUpdate)) {
      toUpdate <- setdiff(toUpdate, dontUpdate)
    }
    if (length(toUpdate)) {
      args[["pkgs"]] <- toUpdate
      args[["dependencies"]] <- FALSE
      do.call(install.packages, args)
    }
  }

  #  install
  args <- list(c("SpaDES.core", "SpaDES.tools"), dependencies = TRUE)
  if (isWin && missing(type)) {
    args$type <- "binary"
  }

  if (!isWin && !dir.exists(file.path(.libPaths()[1], "igraph"))) {
    # igraph needs to be installed from source
    install.packages("igraph", type = "source", lib = libPath, repos = "https://cran.rstudio.com")
  }
  ip <- installed.packages()
  versions <- versions[args[[1]]]
  alreadyInstalled <- names(versions) %in% rownames(ip)
  if (any(alreadyInstalled)) {
    versions <- versions[alreadyInstalled]
    whichOK <- unlist(lapply(seq(versions), function(ind) {
      ok <- (!identical(as.character(packageVersion(names(versions)[ind])), versions[ind]))
      if (identical(ok, TRUE)) {
        message("skipping install of ", names(versions)[ind], "; version is OK")
      }
      ok
    }))
    args[[1]] <- args[[1]][!whichOK]
  }

  if (length(args[[1]])) {
    do.call(install.packages, args)
  }
  if (isTRUE(SpaDES.project)) {
    tf <- tempfile();
    download.file("https://raw.githubusercontent.com/r-universe/predictiveecology/master/.remotes.json",
                  destfile = tf)
    out <- readLines(tf)
    if (any(grepl("SpaDES.project", out)))
      install.packages("SpaDES.project", repos = 'https://predictiveecology.r-universe.dev')
    else
      out <- Require::Require("PredictiveEcology/SpaDES.project", require = FALSE, upgrade = FALSE)
  }

  return(invisible())
}

#' Install spatial packages
#'
#' (Re)install spatial packages that require GDAL/GEOS/PROJ, from source to ensure they are properly
#' liked to these external libraries.
#'
#' @param repos URL of CRAN mirror to use to fetch source packages
#'
#' @note if installing on macOS, homebrew installation of GDAL etc. is required.
#'
#' @export
installSpatialPackages <- function(repos = "https://cran.rstudio.com") {
  ## rgdal and sf need additional args for homebrew on macOS
  if (Sys.info()[["sysname"]] == "Darwin") {
    stopifnot(nzchar(Sys.which("brew")))

    install.packages("rgdal", type = "source", repos = repos,
                     configure.args = c("--with-proj-lib=/usr/local/lib/",
                                        "--with-proj-include=/usr/local/include/"))
    install.packages("sf", type = "source", repos = repos,
                     configure.args = "--with-proj-lib=/usr/local/lib/")
  } else {
    install.packages("rgdal", type = "source", repos = repos)
    install.packages("sf", type = "source", repos = "https://cran.rstudio.com")
  }

  # other spatial packages ----------------------------------------------------------------------

  otherSpatialPackages <- c("rgeos", "sp", "raster", "terra", "lwgeom")
  install.packages(otherSpatialPackages, type = "source", repos = repos)
}
