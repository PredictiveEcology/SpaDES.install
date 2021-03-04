The [SpaDES ecosystem](https://spades.predictiveecology.org) is comprised of several packages (the `SpaDES` packages), and with those packages, one can make modules (`SpaDES` modules). There are a large number of package dependencies in both types, and as a result, there are many potential hurdles to getting correctly set up. This package provides a lightweight toolkit to help with setting up the package and modules for a project.

## Installing SpaDES

```
if (!require("Require")) {install.packages("Require"); library(Require)}
Require("PredictiveEcology/SpaDES.install") # install this package
installSpaDES()
```

## Pre-install all packages in a collection of SpaDES modules

In many cases, this step will not be necessary as the packages that are required in SpaDES modules are installed and loaded during the `simInit` call. However, under certain circumstances, including upgrading to newer versions, modules that have conflicting version, and projects that have more than one `simInit` call, it may be necessary to pre-load packages.

Locate the folder with your SpaDES modules, and pass it to the following function:

```
makeSureAllPackagesInstalled(modulePath)
```


