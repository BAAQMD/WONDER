# README

`WONDER` is an R package for importing and working with data from the 
Centers for Disease Control (CDC) [WONDER] online databases.

Currently, this package provides the following key functions:

- `import_WONDER_UCD()`
- `import_WONDER_UCD_raceeth()`

... for importing data that are manually downloaded, as text files, 
from the [Underlying Cause of Death (UCD) web interface](https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html).

The results are tibbles with unit awareness, supported by the `units` package.
Incidence rates will have units of `person-1 yr-1`.

Because UCD incidence rates can be obtained as point estimates with 
upper and lower bounds (i.e., confidence intervals), this package also 
provides a class, `ptrg`, that encapsulates these in a tidy way.

For use cases / examples, please have a look at the "UCD" vignette:

> vignette("UCD", package = "WONDER")

Note that for the above to work, you must have passed the `build_vignettes` option when installing, e.g.:

> devtools::install_github("BAAQMD/WONDER", build_vignettes = TRUE)

[WONDER]: https://wonder.cdc.gov
