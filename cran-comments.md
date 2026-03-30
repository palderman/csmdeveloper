## Changes in response to feedback from previous CRAN review:

* Added two additional sentences to package Description field
* Added \value{} for csm_get_at_t.Rd, csm_mod_arr.Rd,
  is_data_structure.Rd, is_input.Rd, is_parameter.Rd,
  is_state_variable.Rd and is_transform.Rd
* Removed "<<-" from inst/tinytest/test_simple_wheat.R

## Test environments

* local Ubuntu 22.04.5 LTS, R 4.5.2, GCC
* win-builder (devel and release)
* mac-builder (release)
* RHub windows on GitHub (R-devel)
* RHub Fedora Linux 38 on Github (R-devel)

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
