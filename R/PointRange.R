#'----------------------------------------------------------------------
#'
#' Constructors
#'
#'----------------------------------------------------------------------

#' ptrg
#'
#' Constructor for new `ptrg` objects
#'
#' @param est estimate
#' @param lower lower bound
#' @param upper upper bound
#'
#' @importFrom vctrs vec_cast_common vec_recycle_common
#'
#' @export
ptrg <- function(
    est = double(),
    lower = double(),
    upper = double()
) {

  num <- map(list(est, lower, upper), as.numeric)
  cast <- vctrs::vec_cast_common(num[[1]], num[[2]], num[[3]], .to = double())
  recycled <- vctrs::vec_recycle_common(cast[[1]], cast[[2]], cast[[3]])
  obj <- new_ptrg(recycled[[1]], recycled[[2]], recycled[[3]])

  if (has_units(est)) {
    obj <- set_units(obj, get_units(est), mode = "character")
  }

  return(obj)

}

new_ptrg <- function (
    est   = double(),
    lower = double(),
    upper = double()
) {

  if (!is.numeric(est)) {
    abort("`est` must be a numeric vector.")
  }

  if (!is.numeric(lower)) {
    abort("`lower` must be a numeric vector.")
  }

  if (!is.numeric(upper)) {
    abort("`upper` must be a numeric vector.")
  }

  u <- get_units(est)

  if (isFALSE(is.null(u))) {
    est <- units::set_units(est, u, mode = "character")
    lower <- units::set_units(lower, u, mode = "character")
    upper <- units::set_units(upper, u, mode = "character")
  }

  cls <- "vctrs_ptrg"
  # if (isFALSE(is.null(u))) {
  #   cls <- union(cls, "units")
  # }
  obj <- new_rcrd(list(est = est, lower = lower, upper = upper), class = cls)

  return(obj)

}

#' @export
is_ptrg <- function (x) {
  inherits(x, "vctrs_ptrg")
}

#'----------------------------------------------------------------------
#'
#' Coercion: `vec_cast()` and `vec_ptype2()`
#'
#'----------------------------------------------------------------------

#' @export
vec_cast.vctrs_ptrg.vctrs_ptrg <- function(x, to, ...) x

#' @importFrom vctrs field
#' @export
vec_cast.double.vctrs_ptrg <- function(x, to, ...) {
  return(as.numeric(vctrs::field(x, "est")))
  #return(vctrs::field(x, "est"))
}

#' @importFrom vctrs field
#' @export
vec_cast.units.vctrs_ptrg <- function(x, to, ...) {
  return(vctrs::field(x, "est"))
}

#' @export
vec_ptype2.vctrs_ptrg.vctrs_ptrg <- function(x, y, ...) {
  x
}

#' @export
vec_ptype2.vctrs_ptrg.double <- function(x, y, ...) {
  double()
}

#'----------------------------------------------------------------------
#'
#' Unit-awareness
#'
#'----------------------------------------------------------------------

#' Unit-aware methods
#'
#' @name name ptrg-units
#' @rdname ptrg-units
#'
#' @param x `ptrg` object
#' @param ... (optional)
#'
NULL

#' @export
drop_units.data.frame <- function (x) {
  mutate(x, across(where(has_units), drop_units))
}

#' @rdname ptrg-units
#' @export
drop_units.vctrs_ptrg <- function (x) {
  new_ptrg(
    est = drop_units(field(x, "est")),
    lower = drop_units(field(x, "lower")),
    upper = drop_units(field(x, "upper")))
}

#' @rdname ptrg-units
#' @export
set_units.vctrs_ptrg <- function (x, ...) {
  new_ptrg(
    est = units::set_units(vctrs::field(x, "est"), ...),
    lower = units::set_units(vctrs::field(x, "lower"), ...),
    upper = units::set_units(vctrs::field(x, "upper"), ...))
}

#' @rdname ptrg-units
#' @export
get_units.vctrs_ptrg <- function (x) {
  return(get_units(field(x, "est")))
}

#'----------------------------------------------------------------------
#'
#' Accessors: `est()`, `lower()` and `upper()`
#'
#'----------------------------------------------------------------------

#' @export
est <- function (x) {
  UseMethod("est")
}

#' @export
est.vctrs_ptrg <- function (x) {
  return(vctrs::field(x, "est"))
}

#' @export
lower <- function (x) {
  UseMethod("lower")
}

#' @export
lower.vctrs_ptrg <- function (x) {
  return(vctrs::field(x, "lower"))
}

#' @export
upper <- function (x) {
  UseMethod("upper")
}

#' @export
upper.vctrs_ptrg <- function (x) {
  return(vctrs::field(x, "upper"))
}

#'----------------------------------------------------------------------
#'
#' Formatting methods
#'
#' - `vec_ptype_abbr`
#' - `obj_print_header`
#' - `format`
#' - `pillar_shaft`
#' - `type_sum`
#' - etc.
#'
#'----------------------------------------------------------------------

#' @export
#' @method vec_ptype_abbr vctrs_ptrg
vec_ptype_abbr.vctrs_ptrg <- function(x) {
  u <- get_units(x)
  if (is.null(u)) {
    return("ptrg")
  } else {
    if (u == "") {
      return("1")
    } else {
      return(u)
    }
  }
}

#' @export
#' @method obj_print_header vctrs_ptrg
obj_print_header.vctrs_ptrg <- function(x, ...) {
  line <- paste0("<", vec_ptype_abbr(x), "[", vec_size(x), "]>")
  cli::cat_line(line)
  invisible(x)
}

format_confint <- function (x, ..., sep = ", ", trim = TRUE) {
  lower_str <- format(as.numeric(lower(x)), ..., trim = trim)
  upper_str <- format(as.numeric(upper(x)), ..., trim = trim)
  out <- paste0("(", lower_str, sep, upper_str, ")")
  out[is.na(x)] <- NA_character_
  return(out)
}

#' @export
#' @method format vctrs_ptrg
format.vctrs_ptrg <- function(x, ..., trim = TRUE) {
  est_str <- format(as.numeric(x), ..., trim = trim)
  confint_str <- format_confint(x, ..., trim = trim)
  out <- paste0(est_str, " ", confint_str)
  out[is.na(x)] <- NA_character_
  return(out)
}

#' pillar_shaft
#'
#' @param x `ptrg` object
#' @param ... further arguments to `format()`
#'
#' @importFrom vctrs field
#' @importFrom pillar pillar_shaft style_num style_subtle style_na new_pillar_shaft_simple
#'
#' @export
#' @method pillar_shaft vctrs_ptrg
#' @exportS3Method pillar::pillar_shaft
pillar_shaft.vctrs_ptrg <- function(x, ..., trim = FALSE) {
  est_val <- as.numeric(vctrs::field(x, "est"))
  est_str <- format(as.numeric(x), ..., trim = trim)
  confint_str <- format_confint(x, ..., trim = trim)
  est_styled <- pillar::style_num(est_str, (est_val < 0), rep(TRUE, length(est_val)))
  confint_styled <- pillar::style_subtle(paste0(" ", confint_str))
  out <- paste0(est_styled, confint_styled)
  out[is.na(est_val)] <- pillar::style_na("NA")
  shaft <- pillar::new_pillar_shaft_simple(out, align = "right")
  return(shaft)
}

#' @export
#' @importFrom pillar type_sum
#' @importFrom units units_options
#' @method type_sum vctrs_ptrg
#' @exportS3Method pillar::type_sum
type_sum.vctrs_ptrg <- function(x, ...) {
  gr <- units::units_options("group")
  # see https://github.com/r-lib/pillar/issues/73 : currently the [ and ] mess up things.
  structure(paste0(gr[1], vec_ptype_abbr(x), gr[2]), class = "type_sum_units")
}

#' @export
#' @importFrom pillar format_type_sum
#' @method type_sum vctrs_ptrg
#' @exportS3Method pillar::format_type_sum
format_type_sum.type_sum_vctrs_ptrg <- function(x, width, ...) {
  #pillar::style_subtle(x)
  return(x)
}
