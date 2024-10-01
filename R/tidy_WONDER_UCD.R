tidy_WONDER_UCD <- function (
    .data,
    rate_per
) {

  rename_codec <- c(
    cnty_name          = "County",
    hsplt              = "Hispanic Origin",
    race               = "Race",
    rate_adj       = "Age Adjusted Rate",
    rate_adj_lower = "Age Adjusted Rate Lower 95% Confidence Interval",
    rate_adj_upper = "Age Adjusted Rate Upper 95% Confidence Interval",
    #rate_adj_se    = "Age Adjusted Rate Standard Error",
    rate_crude         = "Crude Rate",
    rate_crude_lower   = "Crude Rate Lower 95% Confidence Interval",
    rate_crude_upper   = "Crude Rate Upper 95% Confidence Interval")

  i <- match(names(.data), rename_codec)
  renamed <- rename(.data, !!!(rename_codec[i[is.finite(i)]]))

  renamed[["cnty_name"]] <-
    renamed[["cnty_name"]] %||% NA_character_

  result <-
    mutate(
      renamed,
      cnty_name = cnty_name,
      numer     = set_units(Deaths, "1"),
      denom     = set_units(Population, "person*yr")) %>%
    select(
      any_of(c("cnty_name")),
      numer,
      denom)

  if ("hsplt" %in% names(renamed)) {

    result[["hsplt"]] <-
      case_match(
        renamed[["hsplt"]],
        "Hispanic or Latino" ~ TRUE,
        "Not Hispanic or Latino" ~ FALSE,
        .default = NA)

    if ("race" %in% names(renamed)) {

      result[["race"]] <-
        fct_recode(
          renamed[["race"]],
          NatAm = "American Indian or Alaska Native",
          AsnPI = "Asian or Pacific Islander",
          AABlk = "Black or African American",
          White = "White")

    } else {

      result[["race"]] <- NA_character_

    }

    result <-
      mutate(
        result,
        raceeth = if_else(hsplt, "HspLt", race),
        .keep = "unused") %>%
      relocate(
        raceeth,
        .before = which(names(result) == "hsplt"))

  } else {

    result[["raceeth"]] <- NA_character_

  }

  if ("rate_adj_lower" %in% names(renamed)) {
    result[["rate_adj"]] <-
      with(renamed,
           as_incidence_rate(
             rate_adj / rate_per,
             rate_adj_lower / rate_per,
             rate_adj_upper / rate_per))
  } else {
    result[["rate_adj"]] <-
      with(renamed,
           as_incidence_rate(
             rate_adj / rate_per))
  }

  if ("rate_crude_lower" %in% names(renamed)) {
    result[["rate_crude"]] <-
      with(renamed,
           as_incidence_rate(
             rate_crude / rate_per,
             rate_crude_lower / rate_per,
             rate_crude_upper / rate_per))
  } else {
    result[["rate_crude"]] <-
      with(renamed,
           as_incidence_rate(
             crude / rate_per))
  }

  result %>%
    select_last(
      any_of(c("numer", "denom")))

}
