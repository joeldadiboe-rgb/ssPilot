#' Optimize pilot sample size using the non-central t-distribution
#'
#' Determines the pilot-trial sample size per treatment arm that minimizes
#' the total sample size required for the pilot and main trials using the
#' non-central t-distribution (NCT) approach.
#'
#' The function evaluates a range of possible pilot sample sizes and calls
#' [nct_sample_size()] for each candidate pilot size. The optimal pilot
#' sample size is the one that minimizes the combined pilot and main-trial
#' sample size per arm.
#'
#' @param sd Anticipated standard deviation based on prior knowledge,
#'   previous studies, published literature, or other available evidence.
#' @param effect Expected treatment difference.
#' @param power Desired statistical power.
#' @param alpha Type I error rate.
#' @param allocation Allocation ratio of experimental to control arm.
#' @param min_pilot_n Minimum pilot sample size per treatment arm.
#' @param max_pilot_n Maximum pilot sample size per treatment arm.
#' @param max_main_n Maximum main-trial sample size per treatment arm
#'   passed to [nct_sample_size()].
#'
#' @references Whitehead, A. L., Julious, S. A., Cooper, C. L., & Campbell, M. J. (2016).
#'  Estimating the sample size for a pilot randomised trial to minimise the overall trial sample size for the external pilot and main trial for a continuous outcome variable.
#'  Statistical Methods in Medical Research, 25(3), 1057–1073.
#'
#'
#' @return A list containing:
#' \describe{
#'   \item{optimal_pilot_n_per_arm}{Optimal pilot sample size per arm.}
#'   \item{main_n_per_arm}{Required main-trial sample size per arm
#'   corresponding to the optimal pilot size.}
#'   \item{total_n_per_arm}{Combined pilot and main-trial sample size
#'   per arm.}
#'   \item{pilot_degrees_of_freedom}{Degrees of freedom from the
#'   pilot trial.}
#'   \item{main_degrees_of_freedom}{Degrees of freedom from the
#'   main trial.}
#'   \item{critical_value}{Critical value from the corresponding
#'   main-trial calculation.}
#'   \item{nct_quantile}{Non-central t quantile from the corresponding
#'   calculation.}
#'   \item{equation_value}{Sample-size equation value from the
#'   corresponding calculation.}
#'   \item{optimization_results}{Data frame containing the results
#'   for all candidate pilot sample sizes.}
#' }
#'
#' @examples
#' \donttest{
#' optimized_nct_sample_size(
#'   sd = 1,
#'   effect = 0.50,
#'   power = 0.90,
#'   alpha = 0.05
#' )
#'}
#' @export

optimized_nct_sample_size <- function(
    sd,
    effect,
    power = 0.90,
    alpha = 0.05,
    allocation = 1,
    min_pilot_n = 10,
    max_pilot_n = 100,
    max_main_n = 10000
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

  # Check minimum pilot sample size
  if (!is.numeric(min_pilot_n) ||
      length(min_pilot_n) != 1 ||
      is.na(min_pilot_n) ||
      min_pilot_n < 2 ||
      min_pilot_n %% 1 != 0) {
    stop(
      "min_pilot_n must be an integer greater than or equal to 2."
    )
  }

  # Check maximum pilot sample size
  if (!is.numeric(max_pilot_n) ||
      length(max_pilot_n) != 1 ||
      is.na(max_pilot_n) ||
      max_pilot_n < min_pilot_n ||
      max_pilot_n %% 1 != 0) {
    stop(
      "max_pilot_n must be an integer greater than or equal to ",
      "min_pilot_n."
    )
  }

  # Check maximum main-trial sample size
  if (!is.numeric(max_main_n) ||
      length(max_main_n) != 1 ||
      is.na(max_main_n) ||
      max_main_n < 2 ||
      max_main_n %% 1 != 0) {
    stop(
      "max_main_n must be an integer greater than or equal to 2."
    )
  }

  # Candidate pilot sample sizes
  pilot_sizes <- min_pilot_n:max_pilot_n

  # Calculate the required main-trial sample size
  # for every candidate pilot sample size
  optimization_results <- lapply(
    pilot_sizes,
    function(pilot_n) {

      result <- nct_sample_size(
        pilot_n = pilot_n,
        sd = sd,
        effect = effect,
        power = power,
        alpha = alpha,
        allocation = allocation,
        max_n = max_main_n
      )

      data.frame(
        pilot_n_per_arm = pilot_n,
        main_n_per_arm = result$main_n_per_arm,
        total_n_per_arm =
          pilot_n + result$main_n_per_arm
      )
    }
  )

  # Combine results into one data frame
  optimization_results <- do.call(
    rbind,
    optimization_results
  )

  # Find minimum total sample size
  minimum_total <- min(
    optimization_results$total_n_per_arm
  )

  # Identify all pilot sizes achieving the minimum
  optimal_rows <- which(
    optimization_results$total_n_per_arm == minimum_total
  )

  # If multiple pilot sizes give the same minimum,
  # choose the smallest pilot sample size
  optimal_row <- optimal_rows[1]

  # Extract optimal pilot sample size
  optimal_pilot_n <- optimization_results[
    optimal_row,
    "pilot_n_per_arm"
  ]

  # Calculate the NCT result for the optimal pilot size
  optimal_result <- nct_sample_size(
    pilot_n = optimal_pilot_n,
    sd = sd,
    effect = effect,
    power = power,
    alpha = alpha,
    allocation = allocation,
    max_n = max_main_n
  )

  # Return results
  list(
    optimal_pilot_n_per_arm = optimal_pilot_n,
    main_n_per_arm = optimal_result$main_n_per_arm,
    total_n_per_arm = minimum_total,
    pilot_degrees_of_freedom =
      optimal_result$pilot_degrees_of_freedom,
    main_degrees_of_freedom =
      optimal_result$main_degrees_of_freedom,
    critical_value =
      optimal_result$critical_value,
    nct_quantile =
      optimal_result$nct_quantile,
    equation_value =
      optimal_result$equation_value,
    optimization_results =
      optimization_results
  )
}
