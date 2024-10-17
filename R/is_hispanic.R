is_hispanic <- function (x) {
  str_detect(x, "Hispanic|HspLt") & !str_detect(x, "Not|NonHspLt")
}
