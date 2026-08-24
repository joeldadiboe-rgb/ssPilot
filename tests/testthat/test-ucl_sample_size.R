test_that("ucl_sample_size returns the expected sample size", {

  result <- ucl_sample_size(
    pilot_n = 16,
    sd = 1,
    effect = 0.50,
    power = 0.90,
    alpha = 0.05,
    conf_level = 0.80
  )

  expect_equal(result$main_n_per_arm, 108)

})
