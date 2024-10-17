#' import_WONDER_UCD
#'
#' @param path (character) path to .txt file
#' @param ...reserved for future use
#' @param na values to replace with NA
#'
#' @importFrom readr read_lines cols read_tsv col_character col_double
#' @importFrom stringr str_detect str_extract
#' @importFrom dplyr mutate filter select
#'
#' @return tibble
#' @export
import_WONDER_UCD <- function (
    path,
    na = c("", "Suppressed", "Not Applicable", "Unreliable"),
    ...
) {

  txt_lines <- readr::read_lines(path)

  metadata_begins <-
    txt_lines %>%
    str_detect("---") %>%
    which() %>%
    min()

  txt_cols <- readr::cols(
    Notes = readr::col_character(),
    Deaths = readr::col_double(),
    Population = readr::col_double())

  txt_data <-
    readr::read_tsv(
      file = path,
      na = na,
      n_max = metadata_begins - 2,
      col_types = txt_cols)

  if ("County" %in% names(txt_data)) {
    f <- function (x) str_remove(x, " County, CA")
    txt_data <- mutate(txt_data, across(County, f))
  }

  filtered_data <-
    txt_data %>%
    filter(
      is.na(Notes) | Notes != "Total") %>%
    select(
      -Notes)

  filtered_data$Interval <-
    str_extract(path, "[0-9]{4}-[0-9]{4}")

  rate_per <-
    txt_lines %>%
    keep(str_detect, "Calculate Rates Per") %>%
    parse_number()

  tidied_data <-
    tidy_WONDER_UCD(
      filtered_data,
      rate_per = rate_per,
      ...)

  if ("pop_age" %in% names(tidied_data)) {
    # age_header <- "^\"(Five|Ten)-Year Age Groups: "
    # age_meta <- txt_lines %>% keep(str_detect, age_header)
    # #age_by <- str_match(age_meta, age_header)[, 2]
    # age_levels <- age_meta %>% str_remove(age_header) %>% str_split(, pattern = "; ") %>% .[[1]]
    # tidied_data <- mutate(tidied_data, pop_age = ordered(pop_age, levels = age_levels))
    tidied_data <- mutate(tidied_data, pop_age = fct_inorder(pop_age, ordered = TRUE))
  }

  comment(tidied_data) <- path
  return(tidied_data)

}
