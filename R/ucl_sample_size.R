#' Main trial sample size using the upper confidence limit method
#'
#' Calculates the required main-trial sample size per treatment
#' arm using the upper confidence limit (UCL) approach described
#' by Browne.
#'
#'@importFrom stats qchisq
#'
#' @param pilot_n Pilot trial sample size per treatment arm.
#' @param sd Pilot estimate of the standard deviation.
#' @param effect Expected treatment difference.
#' @param power Desired statistical power.
#' @param alpha Type I error rate.
#' @param allocation Allocation ratio of experimental to control arm.
#' @param conf_level Confidence level for the upper confidence limit.
#'
#' @details  Browne recommends an 80% upper confidence
#' level. However, Sim and Lewis, set X at 0.95 or
#' the 95% level.
#'
#' @references  Browne RH. On the use of a pilot sample for sample size
#' determination. Stat Med 1995; 14: 1933–1940.
#' @references Sim J and Lewis M. The size of a pilot study for a clinical
#'  should be calculated in relation to considerations of
#' precision and efficiency. J Clin Epidemiol 2012; 65:
#' 301–308.
#'
#' @return A list containing the degrees of freedom, upper confidence
#'   limit for the variance, upper confidence limit for the standard
#'   deviation, and main-trial sample size per treatment arm.
#'
#' @examples
#' ucl_sample_size(
#'   pilot_n = 16,
#'   sd = 1,
#'   effect = 0.50,
#'   power = 0.90,
#'   alpha = 0.05,
#'   conf_level = 0.80
#' )
#'
#' @export


ucl_sample_size <- function(
    pilot_n,
    sd,
    effect,
    power = 0.90,
    alpha = 0.05,
    allocation = 1,
    conf_level = 0.80
) {

  # Check pilot sample size
  if (!is.numeric(pilot_n) ||
      length(pilot_n) != 1 ||
      is.na(pilot_n) ||
      pilot_n < 2 ||
      pilot_n %% 1 != 0) {
    stop("pilot_n must be a positive integer greater than or equal to 2.")
  }

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

  # Degrees of freedom for a two-arm equal-allocation pilot
  k <- 2 * pilot_n - 2

  # Upper confidence limit for the variance
  variance_ucl <-
    k * sd^2 /
    qchisq(1 - conf_level, df = k)

  # Upper confidence limit for the standard deviation
  sd_ucl <- sqrt(variance_ucl)

  # Main-trial sample size
  main_n <- standard_sample_size(
    sd = sd_ucl,
    effect = effect,
    power = power,
    alpha = alpha,
    allocation = allocation
  )

  # Return results
  list(
    pilot_n_per_arm = pilot_n,
    degrees_of_freedom = k,
    variance_ucl = variance_ucl,
    sd_ucl = sd_ucl,
    main_n_per_arm = main_n
  )
}


# Test the UCL function
# ucl_sample_size(
 # pilot_n = 16,
  # sd = 1,
  # effect = 0.50,
  # power = 0.90,
  # alpha = 0.05,
  # conf_level = 0.80
# )
