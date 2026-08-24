test_that("optimized_ucl_sample_size returns the expected optimum", {

  result <- optimized_ucl_sample_size(
    sd = 1,
    effect = 0.50,
    power = 0.90,
    alpha = 0.05,
    conf_level = 0.80
  )

  expect_equal(
    result$optimal_pilot_n_per_arm,
    16
  )

  expect_equal(
    result$main_n_per_arm,
    108
  )

  expect_equal(
    result$total_n_per_arm,
    124
  )

})
