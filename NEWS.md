Known issues: https://github.com/PredictiveEcology/SpaDES.install/issues

version 0.0.6
=============

* remove `SpaDES.core` and `reproducible` from `Suggests`
* new function: `packagesInModules` that will identify all packages in modules without needing to load `SpaDES.core`
* new function: `installSourcePackages`. When `source` is required, this will force them. NOTE: the dependencies of these will not necessarily be `source`, unless they are specified.
* tweaks to `installSpatialPackages` and `installSpaDES` to deal with more edge cases
* Assume `PredictiveEcology` account when not specified in using `getModule`
* update documentation

version 0.0.5
=============

* Load spatial packages
* Edge cases when some packages already installed but not with latest version


version 0.0.4
=============

* A new package, aiming to aid installing and updating the SpaDES ecosystem of packages and the `reqdPkgs` of SpaDES modules in a project

