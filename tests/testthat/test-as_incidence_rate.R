test_that("as_incidence_rate works with numeric values", {

  x <- as_incidence_rate(c(0.01, 0.02))
  expected <- set_units(c(0.01, 0.02), "person-1 yr-1")
  expect_equal(x, expected)

})

test_that("as_incidence_rate works with ptrg", {

  x <- as_incidence_rate(
    c(0.01, 0.02),
    c(0.005, 0.019),
    c(0.014, 0.023))

  expect_equal(
    est(x),
    as_incidence_rate(c(0.01, 0.02)))

  expect_equal(
    lower(x),
    as_incidence_rate(c(0.005, 0.019)))

  expect_equal(
    upper(x),
    as_incidence_rate(c(0.014, 0.023)))

})
