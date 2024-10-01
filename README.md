# README

`WONDER` is an R package for importing and working with data from the 
Centers for Disease Control (CDC) [WONDER] online databases.

Currently, this package provides the following key function:

- `import_WONDER_UCD()`

... for importing data that are manually downloaded, as text files, 
from the [Underlying Cause of Death (UCD) web interface](https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html).
You typically need to download these files yourself, as there's no way 
to use the WONDER API to get place-specific (e.g. county-specific) data. 
But this package should help with importing, tidying, combining, visualizing, etc.!

Examples of downloaded `.txt` files are provided in [inst/extdata/].
If you use the functios above to import such text files, the results will be `tibble`s with columns that are unit-aware. 
Incidence rates will have units of `person-1 yr-1`.

Because incidence rates can be obtained as point estimates with 
upper and lower bounds (i.e., confidence intervals), this package also 
provides a class, `ptrg` ("point range"), that encapsulates these in a tidy way. Columns `rate_adj` (age-adjusted rates) and `rate_crude` will be like this. You cannot sum these point ranges as-is, but you can extract the point estimates from them using either `as.numeric()` or `est()`.

For use cases / examples, please have a look at the "UCD" vignette:

> vignette("UCD", package = "WONDER")

Note that for the above `vignette(...)` to work, you must have passed the `build_vignettes` option when installing, e.g.:

> devtools::install_github("BAAQMD/WONDER", build_vignettes = TRUE)

[WONDER]: https://wonder.cdc.gov
