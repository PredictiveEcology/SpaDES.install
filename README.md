The [SpaDES ecosystem](https://spades.predictiveecology.org) is comprised of several packages (the `SpaDES` packages), and with those packages, one can make modules (`SpaDES` modules). There are a large number of package dependencies in both types, and as a result, there are many potential hurdles to getting correctly set up. This package provides a lightweight toolkit to help with setting up the R packages and modules for a project.

There are 4 steps:

1. Decide if R packages are going to installed in a separate folder for this project.
2. Install R packages for SpaDES-family --> these help deal with package dependencies
3. Install any desired or missing *SpaDES modules*
4. Install R packages for modules

We outline each of these below. This package is new and is still being actively tested, updated, maintained.

## Keeping all packages isolated for each project

In many cases, it is desirable to separate the packages that are installed for each project, even on the same computer. 
To do this, use either R's built in tool, `.libPaths("projectRPackages")`, but this must be placed at the top of a (main) project script, or use `Require::setLibPaths("projectRPackages")`, which will set it for the project, reminding the user that it is set each time R restarts.

## 1. Keep R packages separate? (this is not necessary, but can be useful) 

```
userRlib = "~/tempRPackages/4.1" # important to specify version here ... things may not work as expected below if omitted
if (!dir.exists(userRlib)) dir.create(userRlib, recursive = TRUE)
.libPaths(userRlib)
```

## 2. Install R packages for SpaDES-family

`SpaDES` is an R package that has many dependencies. Because of all the dependencies, it may not work to simply use `install.packages("SpaDES")`. Instead, we have made `installSpaDES()` to deal with many of the challenges we have encountered. Rstudio has several features that "automatically" load R packages (without user knowledge). It will be difficult to update these, once they are loaded in RStudio. We recommend, if possible, to do these initial steps outside of RStudio.

```
# optionally set a repo for binary linux packages (works also for Windows, but unnecessary)
#  This will be WAY faster to install everything
# options(repos = c(CRAN = paste0("https://packagemanager.rstudio.com/all/__linux__/focal/latest")))
# If using an older Linux, you will have to replace the "focal" on previous line with the value here:
# version = strsplit(system("lsb_release -c", intern = TRUE), ":\t")[[1]][[2]]

# ------ RESTART R --------
# Install R packages 
# First: Require 
if (!require("Require")) {install.packages("Require"); library(Require)}
# Second: SpaDES.install
Require("PredictiveEcology/SpaDES.install (>= 0.0.5.9006)", dependencies = NA) # install/load this package
# Last -- All others -- this will correctly install from source the spatial R packages + igraph
installSpaDES()
```


## 3. Install desired or missing *SpaDES modules*

You can install modules in various ways. The easiest is to identify their repository (on Github.com), such as `"PredictiveEcology/Biomass_core"`, and decide on a folder (`modulePath`) to install your modules, then install it or them with:


```
# Set a path
modulePath = "modules"
# Download them
if (!dir.exists(file.path(modulePath, "Biomass_core"))) {
  getModule("PredictiveEcology/Biomass_core", modulePath = modulePath) 
  # If you want a specific branch:
	# getModule("PredictiveEcology/Biomass_core@development", modulePath = modulePath) 
	getModule("PredictiveEcology/Biomass_regeneration", modulePath = modulePath)
}
```

These will have been installed in your selected path. 

There is [a wiki that lists many of some modules that we are currently aware of](https://github.com/PredictiveEcology/SpaDES-modules/wiki/Current-modules-in-development).


## 4. Install R packages for modules


*SpaDES modules* are written by different people and have their own R package dependencies, often different than `SpaDES`. These too need to be installed, version numbers checked, and conflicting version number needs identified. We created `makeSureAllPackagesInstalled` to address many of the potential problems that could be faced.

```
# modulePath is defined in previous chunk
makeSureAllPackagesInstalled(modulePath)
```




# Top of each script -- for reproducibility

We have started placing something such as these lines at the top of our scripts, so that we can quickly get everything up and running on any new computer, or with any new person that collaborates on the project. This may take several seconds, depending on the number of modules. This time is mostly for loading `SpaDES.core`, which is a step that will have to happen eventually and so is not "lost" if this block is kept in a script and run every time. Note: the 2nd and subsequent times within a session should be fast, as caching is used.

```
.libPaths("~/tempRPackages/4.1") # put R package path for project here
if (!require("Require")) {install.packages("Require"); library(Require)}
Require("PredictiveEcology/SpaDES.install") # install/load this package
installSpaDES()                             # install missing packages
makeSureAllPackagesInstalled("modules")     # install missing packages
```



