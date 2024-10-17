test_that("is_hispanic", {

  expect_true(is_hispanic("Hispanic"))
  expect_true(is_hispanic("Hispanic or Latino"))
  expect_true(is_hispanic("HspLt"))

  expect_false(is_hispanic("White"))
  expect_false(is_hispanic("Not Hispanic"))
  expect_false(is_hispanic("NonHspLt"))

})
