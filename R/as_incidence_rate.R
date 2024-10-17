#' as_incidence_rate
#'
#' Standardize incidence rate
#'
#' @param x (numeric)
#' @param units (character) default is "per million" persons per year
#'
#' @return units
#' @export
as_incidence_rate <- function (
    x,
    lower,
    upper,
    units = "person-1 yr-1"
) {

  stopifnot(all(is.na(x) | as.numeric(x) > 0))
  stopifnot(all(is.na(x) | as.numeric(x) < 1))

  u <- set_units(x, units, mode = "character")

  if (isFALSE(missing(lower)) && isFALSE(missing(upper))) {
    u <- ptrg(u, lower, upper)
  }

  return(u)

}
