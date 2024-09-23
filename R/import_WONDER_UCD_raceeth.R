#' import_WONDER_UCD_raceeth
#'
#' @param path (character)
#' @param ... passed to [import_WONDER_UCD()]
#'
#' @return tibble
#' @export
import_WONDER_UCD_raceeth <- function (
    path,
    ...
) {

  imported_data <-
    import_WONDER_UCD(path, ...)

  imported_data[["race"]] <-
    imported_data[["race"]] %||% NA_character_

  raceeth_data <-
    mutate(
      imported_data,
      raceeth = if_else(hsplt, "HspLt", race),
      .keep = "unused") %>%
    relocate(
      raceeth,
      .before = which(names(imported_data) == "hsplt"))

  return(raceeth_data)

}
