#' Optimized pilot sample size using the UCL method
#'
#' Finds the pilot-trial sample size per treatment arm that minimizes
#' the combined pilot and main-trial sample size using the upper
#' confidence limit approach.
#'
#' @importFrom stats qchisq
#'
#' @param sd Anticipated standard deviation based on prior knowledge,
#'   previous studies, published literature, or other available evidence.
#' @param effect Expected treatment difference.
#' @param power Desired statistical power for the main trial.
#' @param alpha Type I error rate.
#' @param allocation Allocation ratio of experimental to control arm.
#' @param conf_level Confidence level for the upper confidence limit.
#' @param min_pilot Minimum pilot sample size per treatment arm.
#' @param max_pilot Maximum pilot sample size per treatment arm to search.
#'
#' @references Whitehead, A. L., Julious, S. A., Cooper, C. L., & Campbell, M. J. (2016).
#'  Estimating the sample size for a pilot randomised trial to minimise the overall trial sample size for the external pilot and main trial for a continuous outcome variable.
#'  Statistical Methods in Medical Research, 25(3), 1057–1073.
#'
#' @return A list containing the optimal pilot sample size per arm,
#'   the corresponding main-trial sample size per arm, the total
#'   sample size per arm, the confidence level used, and a data frame
#'   containing the optimization results for all candidate pilot
#'   sample sizes.
#'
#' @examples
#' optimized_ucl_sample_size(
#'   sd = 1,
#'   effect = 0.50,
#'   power = 0.90,
#'   alpha = 0.05,
#'   conf_level = 0.80
#' )
#'
#' @export

optimized_ucl_sample_size <- function(
    sd,
    effect,
    power = 0.90,
    alpha = 0.05,
    allocation = 1,
    conf_level = 0.80,
    min_pilot = 10,
    max_pilot = 100
) {

  # Check standard deviation
  if (!is.numeric(sd) ||
      length(sd) != 1 ||
      is.na(sd) ||
      sd <= 0) {
    stop("sd must be a positive numeric value.")
  }

  # Check treatment effect
  if (!is.numeric(effect) ||
      length(effect) != 1 ||
      is.na(effect) ||
      effect <= 0) {
    stop("effect must be a positive numeric value.")
  }

  # Check power
  if (!is.numeric(power) ||
      length(power) != 1 ||
      is.na(power) ||
      power <= 0 ||
      power >= 1) {
    stop("power must be between 0 and 1.")
  }

  # Check alpha
  if (!is.numeric(alpha) ||
      length(alpha) != 1 ||
      is.na(alpha) ||
      alpha <= 0 ||
      alpha >= 1) {
    stop("alpha must be between 0 and 1.")
  }

  # Check allocation ratio
  if (!is.numeric(allocation) ||
      length(allocation) != 1 ||
      is.na(allocation) ||
      allocation <= 0) {
    stop("allocation must be a positive numeric value.")
  }

  # Check confidence level
  if (!is.numeric(conf_level) ||
      length(conf_level) != 1 ||
      is.na(conf_level) ||
      conf_level <= 0 ||
      conf_level >= 1) {
    stop("conf_level must be between 0 and 1.")
  }

  # Check minimum pilot sample size
  if (!is.numeric(min_pilot) ||
      length(min_pilot) != 1 ||
      is.na(min_pilot) ||
      min_pilot < 2 ||
      min_pilot %% 1 != 0) {
    stop("min_pilot must be an integer greater than or equal to 2.")
  }

  # Check maximum pilot sample size
  if (!is.numeric(max_pilot) ||
      length(max_pilot) != 1 ||
      is.na(max_pilot) ||
      max_pilot < min_pilot ||
      max_pilot %% 1 != 0) {
    stop("max_pilot must be an integer greater than or equal to min_pilot.")
  }

  # Candidate pilot sample sizes
  pilot_sizes <- min_pilot:max_pilot

  # Calculate the UCL standard deviation for each candidate pilot size
  sd_ucl <- sapply(pilot_sizes, function(m) {

    k <- 2 * m - 2

    variance_ucl <-
      k * sd^2 /
      qchisq(1 - conf_level, df = k)

    sqrt(variance_ucl)
  })

  # Calculate the corresponding main-trial sample size
  main_sizes <- sapply(sd_ucl, function(s) {

    standard_sample_size(
      sd = s,
      effect = effect,
      power = power,
      alpha = alpha,
      allocation = allocation
    )
  })

  # Total sample size per arm
  total_sizes <- pilot_sizes + main_sizes

  # Identify the pilot size that minimizes total sample size
  optimal_index <- which.min(total_sizes)

  optimal_pilot_n <- pilot_sizes[optimal_index]

  optimal_main_n <- main_sizes[optimal_index]

  optimal_total_n <- total_sizes[optimal_index]

  # Optimization results
  optimization_results <- data.frame(
    pilot_n_per_arm = pilot_sizes,
    main_n_per_arm = main_sizes,
    total_n_per_arm = total_sizes
  )

  # Return results
  list(
    optimal_pilot_n_per_arm = optimal_pilot_n,
    main_n_per_arm = optimal_main_n,
    total_n_per_arm = optimal_total_n,
    conf_level = conf_level,
    optimization_results = optimization_results
  )
}

# Test the Optimised Sample Size
# optimized_ucl_sample_size(
#   sd = 1,
#   effect = 0.50,
#   power = 0.90,
#   alpha = 0.05,
#   conf_level = 0.80
# )
