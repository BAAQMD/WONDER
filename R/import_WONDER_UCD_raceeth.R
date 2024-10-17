#' import_WONDER_UCD_raceeth
#'
#' @param path (character)
#' @param ... passed to [import_WONDER_UCD()]
#'
#' @return tibble
#' @export
#'
#' @examples
#' txt_path <- system.file("extdata", "UCD", "2011-2020", "UCD-2011-2020-SFBA-ByCounty-Hispanic.txt", package = "WONDER")
#' import_WONDER_UCD_raceeth(txt_path)
#'
import_WONDER_UCD_raceeth <- function (
    path,
    ...
) {

  .Defunct("import_WONDER_UCD")

  imported_data <-
    import_WONDER_UCD(path, ...)

  imported_data[["race"]] <-
    imported_data[["race"]] %||% NA_character_

  raceeth_data <-
    mutate(
      imported_data,
      raceeth = if_else(pop_eth, "pop_eth", race),
      .keep = "unused") %>%
    relocate(
      raceeth,
      .before = which(names(imported_data) == "pop_eth"))

  return(raceeth_data)

}
