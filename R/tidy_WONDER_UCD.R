tidy_WONDER_UCD <- function (
    .data,
    rate_per
) {

  rename_codec <- c(
    cnty_name          = "County",
    pop_age            = "Age Groups",
    pop_eth            = "Hispanic Origin",
    pop_race           = "Race",
    rate_adj           = "Age Adjusted Rate",
    rate_adj_lower     = "Age Adjusted Rate Lower 95% Confidence Interval",
    rate_adj_upper     = "Age Adjusted Rate Upper 95% Confidence Interval",
    rate_crude         = "Crude Rate",
    rate_crude_lower   = "Crude Rate Lower 95% Confidence Interval",
    rate_crude_upper   = "Crude Rate Upper 95% Confidence Interval")

  renamed <-
    rename_with(
      .data,
      function (x) str_remove(x, "(Five|Ten)-Year "),
      dplyr::matches("Age Groups"))

  i <- match(names(renamed), rename_codec)
  renamed <- rename(renamed, !!!(rename_codec[i[is.finite(i)]]))

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

  if ("pop_age" %in% names(renamed)) {
    result[["pop_age"]] <-
      renamed[["pop_age"]]
      # case_match(
      #   renamed[["pop_eth"]],
      #   "Hispanic or Latino" ~ "HspLt",
      #   "Not Hispanic or Latino" ~ "NonHspLt",
      #   .default = NA)
  }

  if ("pop_eth" %in% names(renamed)) {
    result[["pop_eth"]] <-
      case_match(
        renamed[["pop_eth"]],
        "Hispanic or Latino" ~ "HspLt",
        "Not Hispanic or Latino" ~ "NonHspLt",
        .default = NA)
  }

  if ("pop_race" %in% names(renamed)) {
    result[["pop_race"]] <-
      fct_recode(
        renamed[["pop_race"]],
        NatAm = "American Indian or Alaska Native",
        AsnPI = "Asian or Pacific Islander",
        AABlk = "Black or African American",
        White = "White")
  }

  if (all(c("pop_eth", "pop_race") %in% names(result))) {

    result <-
      mutate(
        result,
        pop_raceeth = if_else(
          is_hispanic(pop_eth),
          true = "HspLt",
          false = pop_race),
        .keep = "unused")

  } else if ("pop_eth" %in% names(result)) {

    result[["pop_raceeth"]] <- result[["pop_eth"]]
    result[["pop_eth"]] <- NULL

  } else if ("pop_race" %in% names(result)) {

    result[["pop_raceeth"]] <- result[["pop_race"]]
    result[["pop_race"]] <- NULL

  } else {

    result[["pop_raceeth"]] <- NA_character_

  }

  if ("rate_adj_lower" %in% names(renamed)) {
    result[["rate_adj"]] <-
      with(renamed,
           as_incidence_rate(
             rate_adj / rate_per,
             rate_adj_lower / rate_per,
             rate_adj_upper / rate_per))
  } else if ("rate_adj" %in% names(renamed)) {
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
  } else if ("rate_crude" %in% names(renamed)) {
    result[["rate_crude"]] <-
      with(renamed,
           as_incidence_rate(
             crude / rate_per))
  }

  result %>%
    select_last(
      any_of(c("numer", "denom")))

}
