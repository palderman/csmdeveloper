## Test environments

* local Ubuntu 22.04.5 LTS, R 4.5.2, GCC
* win-builder (devel and release)
* RHub macos-15 on GitHub (R-devel)
* RHub windows (R-devel)
* RHub Fedora Linux 38 (R-devel)

## R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE:

Maintainer: 'Phillip D Alderman <phillip.alderman@okstate.edu>'

New submission

Found the following (possibly) invalid DOIs:
  DOI: 10.32614/CRAN.package.csmbuilder
    From: inst/CITATION
    Status: 404
    Message: Not Found

The "invalid DOI" is due to the package not being on CRAN yet. Once
the package is posted and the new DOI minted this issue will be
resolved.

## Downstream dependencies

There are currently no downstream dependencies for this package.
