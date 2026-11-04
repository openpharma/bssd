## Resubmission

This is the second submission.

In response to the CRAN review (Konstanze Lauseker):

* Removed the redundant "with R" from the package title.
* Added return-value documentation for the eight requested `as.data.frame()`,
  `plot()`, `print()`, and `summary()` methods for `bssd_binary` and
  `bssd_binary_map` objects. The documentation describes the returned classes
  and their meaning, including the invisible `NULL` returned by print methods.
  The roxygen2 sources were updated and the Rd files regenerated.

## Test environment

- {os: macos-latest,   r: 'release'}
- {os: ubuntu-latest,   r: 'release'}
- {os: windows-latest, r: 'release'}
- {os: ubuntu-latest,   r: 'devel', http-user-agent: 'release'}

and:

- win-builder: R-devel

## R CMD check results

There were no errors, warnings or notes