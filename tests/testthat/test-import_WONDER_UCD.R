test_that("import_WONDER_UCD_raceeth works on inst/extdata example", {

  extdata_path <- function (...) {
    system.file("extdata", ..., package = "WONDER")
  }

  hsplt_path <- extdata_path("UCD", "2011-2020", "UCD-2011-2020-Hispanic.txt")
  expect_true(file.exists(hsplt_path))
  hsplt_data <- import_WONDER_UCD_raceeth(hsplt_path)

  expect_s3_class(hsplt_data, "tbl_df")
  expect_setequal(names(hsplt_data), c("cnty_name", "event_n", "pop_qty", "raceeth", "rate_adj", "rate_crude"))
  expect_s3_class(hsplt_data$pop_qty, "units")
  expect_s3_class(hsplt_data$rate_adj, "vctrs_rcrd")

})
