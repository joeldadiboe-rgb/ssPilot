test_that("nct_sample_size returns the expected sample size", {

  result <- nct_sample_size(
    pilot_n = 12,
    sd = 1,
    effect = 0.50,
    power = 0.90,
    alpha = 0.05
  )

  expect_equal(result$main_n_per_arm, 96)

})
