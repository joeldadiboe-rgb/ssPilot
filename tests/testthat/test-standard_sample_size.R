test_that("standard_sample_size returns the expected sample size", {

  result <- standard_sample_size(
    sd = 1,
    effect = 0.50,
    power = 0.90,
    alpha = 0.05
  )

  expect_equal(result, 85)

})
