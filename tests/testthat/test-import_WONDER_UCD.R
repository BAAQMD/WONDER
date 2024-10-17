library(glue)
library(stringr)

extdata_path <- function (...) {
  system.file("extdata", ..., package = "WONDER")
}

test_that("import_WONDER_UCD() works on inst/extdata examples", {

  for (timespan in c("2003-2012", "2007-2016", "2011-2020")) {
    for (geography in c("ByCounty", "Regional")) {
      for (age in c("AllAge", "ByAge5", "ByAge10")) {
        for (variant in c("Hispanic", "NH-ByRace", "AllRaceEth")) {

          test_fn <- glue::glue("UCD-{timespan}-SFBA-{geography}-{age}-{variant}.txt")
          test_path <- extdata_path("UCD", timespan, test_fn)

          if (file.exists(test_path)) {

            test_data <- import_WONDER_UCD(test_path)

            expect_s3_class(test_data, "tbl_df")
            expect_s3_class(test_data$denom, "units")

            if (str_detect(geography, "ByCounty")) {
              expect_setequal(
                test_data$cnty_name,
                c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", "San Mateo", "Santa Clara", "Solano", "Sonoma"))
            } else {
              expect_setequal(test_data$cnty_name, NA_character_)
            }

            if (str_detect(variant, "Hispanic")) {
              expect_setequal(test_data$pop_raceeth, "HspLt")
            } else if(str_detect(variant, "ByRace")) {
              expect_setequal(test_data$pop_raceeth, c("NatAm", "AsnPI", "AABlk", "White"))
            } else {
              expect_setequal(test_data$pop_raceeth, NA)
            }

            if (str_detect(age, "ByAge")) {
              expect_s3_class(test_data$rate_crude, "vctrs_rcrd")
              expect_true(is.ordered(test_data$pop_age))
              expect_setequal(names(test_data), c("cnty_name", "numer", "denom", "pop_raceeth", "rate_crude", "pop_age"))
            } else {
              expect_s3_class(test_data$rate_adj, "vctrs_rcrd")
              expect_setequal(names(test_data), c("cnty_name", "numer", "denom", "pop_raceeth", "rate_adj", "rate_crude"))
              expect_false("pop_age" %in% names(test_data))
            }

          }
        }
      }
    }
  }

})
