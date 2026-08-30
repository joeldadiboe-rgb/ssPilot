#' Main trial sample size using the non-central t-distribution
#'
#' Calculates the required main-trial sample size per treatment
#' arm using the non-central t-distribution (NCT) approach
#' described by Julious and Owen.
#'
#' @importFrom stats qt
#'
#' @param pilot_n Pilot trial sample size per treatment arm.
#' @param sd Pilot estimate of the standard deviation.
#' @param effect Expected treatment difference.
#' @param power Desired statistical power.
#' @param alpha Type I error rate.
#' @param allocation Allocation ratio of experimental to control arm.
#' @param max_n Maximum main-trial sample size per arm to search.
#'
#' @return A list containing the pilot-trial sample size per arm,
#'   pilot-trial degrees of freedom, required main-trial sample size
#'   per arm, main-trial degrees of freedom, central t critical value,
#'   non-central t quantile, and the right-hand side of Equation (4).
#'
#' @references Julious SA and Owen RJ. Sample size calculations for
#' clinical studies allowing for uncertainty about the variance.
#' Pharmaceut Stat 2006; 5: 29–37.
#'
#' @examples
#' nct_sample_size(
#'   pilot_n = 12,
#'   sd = 1,
#'   effect = 0.50,
#'   power = 0.90,
#'   alpha = 0.05
#' )
#'
#' @export
nct_sample_size <- function(
    pilot_n,
    sd,
    effect,
    power = 0.90,
    alpha = 0.05,
    allocation = 1,
    max_n = 10000
) {

  # Check pilot sample size
  if (!is.numeric(pilot_n) ||
      length(pilot_n) != 1 ||
      is.na(pilot_n) ||
      pilot_n < 2 ||
      pilot_n %% 1 != 0) {
    stop(
      "pilot_n must be an integer greater than or equal to 2."
    )
  }

  # Check standard deviation
  if (!is.numeric(sd) ||
      length(sd) != 1 ||
      is.na(sd) ||
      sd <= 0) {
    stop(
      "sd must be a positive numeric value."
    )
  }

  # Check treatment effect
  if (!is.numeric(effect) ||
      length(effect) != 1 ||
      is.na(effect) ||
      effect <= 0) {
    stop(
      "effect must be a positive numeric value."
    )
  }

  # Check power
  if (!is.numeric(power) ||
      length(power) != 1 ||
      is.na(power) ||
      power <= 0 ||
      power >= 1) {
    stop(
      "power must be between 0 and 1."
    )
  }

  # Check alpha
  if (!is.numeric(alpha) ||
      length(alpha) != 1 ||
      is.na(alpha) ||
      alpha <= 0 ||
      alpha >= 1) {
    stop(
      "alpha must be between 0 and 1."
    )
  }

  # Check allocation ratio
  if (!is.numeric(allocation) ||
      length(allocation) != 1 ||
      is.na(allocation) ||
      allocation <= 0) {
    stop(
      "allocation must be a positive numeric value."
    )
  }

  # Check maximum sample size
  if (!is.numeric(max_n) ||
      length(max_n) != 1 ||
      is.na(max_n) ||
      max_n < 2 ||
      max_n %% 1 != 0) {
    stop(
      "max_n must be an integer greater than or equal to 2."
    )
  }

  # Pilot-trial degrees of freedom
  #
  # For a two-arm pilot trial with equal allocation,
  # Whitehead et al. define:
  #
  # k = 2m - 2
  #
  # where m is the pilot sample size per arm.
  k <- 2 * pilot_n - 2

  # Type II error probability
  beta <- 1 - power

  # Candidate main-trial sample sizes per arm
  candidate_n <- 2:max_n

  # Calculate the right-hand side of Whitehead et al.
  # Equation (4) for each candidate main-trial sample size.
  equation_value <- sapply(candidate_n, function(n) {

    # Main-trial degrees of freedom
    df_main <- n * (allocation + 1) - 2

    # Central t critical value for the main trial
    critical_value <- qt(
      1 - alpha / 2,
      df = df_main
    )

    # Non-central t quantile
    #
    # The non-centrality parameter of the pilot-stage
    # non-central t distribution is the main-trial
    # central t critical value.
    nct_quantile <- qt(
      1 - beta,
      df = k,
      ncp = critical_value
    )

    # Right-hand side of Whitehead et al. Equation (4)
    (
      (allocation + 1) *
        nct_quantile^2 *
        sd^2
    ) / (
      allocation * effect^2
    )
  })

  # Find the first integer main-trial sample size
  # satisfying Equation (4):
  #
  # n >= right-hand side of Equation (4)
  adequate <- which(
    candidate_n >= equation_value
  )

  # Stop if the requested sample size was not found
  if (length(adequate) == 0) {
    stop(
      "The required main-trial sample size was not found ",
      "within max_n. Increase max_n and try again."
    )
  }

  # Smallest adequate main-trial sample size
  index <- adequate[1]

  required_n <- candidate_n[index]

  # Recalculate quantities for the selected sample size
  df_main <- required_n * (allocation + 1) - 2

  critical_value <- qt(
    1 - alpha / 2,
    df = df_main
  )

  nct_quantile <- qt(
    1 - beta,
    df = k,
    ncp = critical_value
  )

  # Right-hand side of Equation (4)
  rhs <- (
    (allocation + 1) *
      nct_quantile^2 *
      sd^2
  ) / (
    allocation * effect^2
  )

  # Return results
  list(
    pilot_n_per_arm = pilot_n,
    pilot_degrees_of_freedom = k,
    main_n_per_arm = required_n,
    main_degrees_of_freedom = df_main,
    critical_value = critical_value,
    nct_quantile = nct_quantile,
    equation_value = rhs
  )
}
