test_that("ptrg class inheritance", {

  x <- ptrg(1, 1, 1)
  expect_s3_class(x, "vctrs_rcrd")
  expect_s3_class(x, "vctrs_ptrg")

})

test_that("ptrg supports recycling", {
  expect_length(ptrg(1:10, 3, 3), 10)
})

test_that("ptrg is unit-aware", {

  est <- 1:3
  lower <- abs(rnorm(length(est)))
  upper <- abs(rnorm(length(est)))

  # create first, then set units
  x <- ptrg(est, lower, upper)
  foo <- set_units(x, "ton/day")
  expect_true(has_units(foo, "ton/day"))

  # create from unit-aware input
  bar <- ptrg(set_units(est, "ton/day"), lower, upper)
  expect_true(has_units(bar, "ton/day"))

  # they should amount to the same thing
  expect_equal(foo, bar)

  # drop units from `foo`
  dropped <- drop_units(foo)
  expect_false(has_units(dropped))

  # use `bar` to restore units
  restored <- restore_units(to = dropped, from = foo)
  expect_equal(get_units(foo), get_units(restored))
  expect_equal(restored, foo)

})

test_that("ptrg is.na()", {

  x <- ptrg(
    est = c(NA, 20, 30),
    lower = c(NA, 9, 7),
    upper = c(NA, 90, 70))

  expect_identical(is.na(x), c(TRUE, FALSE, FALSE))

})

test_that("ptrg c()", {

  x <- set_units(
    ptrg(
      est = c(10, 20, 30),
      lower = c(5, 9, 7),
      upper = c(50, 90, 70)),
    "ton")

  (concat <- c(x, x))
  expect_true(has_units(concat))

})

test_that("ptrg str()", {

  x <- set_units(
    ptrg(
      est = c(10, 20, 30),
      lower = c(5, 9, 7),
      upper = c(50, 90, 70)),
    "ton")

  expect_snapshot(str(x))

})

test_that("ptrg format()", {

  x <- set_units(
    ptrg(
      est = c(10, 20, 30),
      lower = c(5, 9, 7),
      upper = c(50, 90, 70)),
    "ton")

  expect_snapshot(format(x))

})

test_that("ptrg tibble()", {

  x <- set_units(
    ptrg(
      est = c(10, 20, 30),
      lower = c(5, 9, 7),
      upper = c(50, 90, 70)),
    "ton")

  expect_snapshot(tibble::tibble(x = x))

})
