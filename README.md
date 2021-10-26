The [SpaDES ecosystem](https://spades.predictiveecology.org) is comprised of several packages (the `SpaDES` packages), and with those packages, one can make modules (`SpaDES` modules). There are a large number of package dependencies in both types, and as a result, there are many potential hurdles to getting correctly set up. This package provides a lightweight toolkit to help with setting up the R packages and modules for a project.

There are 4 steps:

1. Decide if R packages are going to installed in a separate folder for this project.
2. Install `SpaDES.install` R package --> this helps deal with package dependencies
3. Install any desired or missing *SpaDES modules*
4. Install R packages (the SpaDES R packages and dependencies PLUS the SpaDES modules R packages and dependencies)

We outline each of these below. This package is new and is still being actively tested, updated, maintained.

## Keeping all packages isolated for each project

In many cases, it is desirable to separate the packages that are installed for each project, even on the same computer. 
To do this, use either R's built in tool, `.libPaths("projectRPackages")`, but this must be placed at the top of a (main) project script, or use `Require::setLibPaths("projectRPackages")`, which will set it for the project, reminding the user that it is set each time R restarts.

```
# Optional 
Require::setLibPaths("projectRPackages")
```

## Install `SpaDES.install` R package

```
## RESTART R -- START WITH A CLEAN SESSION ##
if (!require("Require")) {install.packages("Require"); library(Require)}
Require("PredictiveEcology/SpaDES.install (>= 0.0.5)") # install/load this package
installSpaDES()

```


## Install desired or missing *SpaDES modules*

You can install modules in various ways. The easiest is to identify their repository (on Github.com), such as `"PredictiveEcology/Biomass_core"`, and decide on a folder (`modulePath`) to install your modules, then install it or them with:
```
modulePath = "modules"
getModule("PredictiveEcology/Biomass_core", modulePath = modulePath)
getModule("PredictiveEcology/Biomass_regeneration", modulePath = modulePath)
```

These will have been installed in your selected path. 

There is [a wiki that lists many of the modules](https://github.com/PredictiveEcology/SpaDES-modules/wiki/Current-modules-in-development).

## Installing R packages

`SpaDES` is an R package that has many dependencies. Because of all the dependencies, it may not work to simply use `install.packages("SpaDES")`. Instead, we have made `installSpaDES()` to deal with many of the challenges we have encountered.

*SpaDES modules* are written by different people and have their own R package dependencies, often different than `SpaDES`. These too need to be installed, version numbers checked, and conflicting version number needs identified. We created `makeSureAllPackagesInstalled` to address many of the potential problems that could be faced.

```
# these functions are part of SpaDES.install package
installSpaDES() 
makeSureAllPackagesInstalled(modulePath) # modulePath is defined in previous code block
```


# Top of each script -- for reproducibility

We have started placing these lines at the top of our scripts, so that we can quickly get everything up and running on any new computer, or with any new person that collaborates on the project.

```
if (!require("Require")) {install.packages("Require"); library(Require)}
Require("PredictiveEcology/SpaDES.install") # install/load this package
needInstallOrUpdate <- FALSE # change to TRUE for first time or periodic updating
if (needInstallOrUpdate) {
  installSpaDES()
  makeSureAllPackagesInstalled("modules")
}
```



