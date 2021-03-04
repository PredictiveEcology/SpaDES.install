The [SpaDES ecosystem](https://spades.predictiveecology.org) is comprised of several packages (the `SpaDES` packages), and with those packages, one can make modules (`SpaDES` modules). There are a large number of package dependencies in both types, and as a result, there are many potential hurdles to getting correctly set up. This package provides a lightweight toolkit to help with setting up the package and modules for a project.

This package is new and is still being actively tested, updated, maintained.

## Installing SpaDES

```

## RESTART R -- START WITH A CLEAN SESSION ##
if (!require("Require")) {install.packages("Require"); library(Require)}
Require("PredictiveEcology/SpaDES.install") # install/load this package
installSpaDES()
```

## Pre-install all packages in a collection of SpaDES modules

In many cases, this step will not be necessary as the packages that are required in SpaDES modules are installed and loaded during the `simInit` call. However, under certain circumstances, including upgrading to newer versions, modules that have conflicting version, and projects that have more than one `simInit` call, it may be necessary to pre-load packages.

Locate the folder with your SpaDES modules, and pass it to the following function:

```
makeSureAllPackagesInstalled("modules")
```


## Keeping all packages isolated for each project

In many cases, it is desirable to separate the packages that are installed for each project, even on the same computer. 
To do this, use either R's built in tool, `.libPaths("projectRPackages")`, but this must be placed at the top of a (main) project script, or use `Require::setLibPaths("projectRPackages")`, which will set it for the project, reminding the user that it is set each time R restarts.

```
Require::setLibPaths("projectRPackages")
installSpaDES()
makeSureAllPackagesInstalled(modulePath)
```

## Top of each script -- for reproducibility

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



