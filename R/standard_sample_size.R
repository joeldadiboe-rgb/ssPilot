#' Standard sample size for a two-arm continuous outcome trial
#'
#' Calculates the required main-trial sample size per treatment arm
#' assuming the population standard deviation is known.
#'
#' @importFrom stats qnorm
#'
#' @param sd Population standard deviation.
#' @param effect Expected treatment difference.
#' @param power Desired statistical power.
#' @param alpha Type I error rate.
#' @param allocation Allocation ratio of experimental to control arm.
#'
#' @return Required sample size per treatment arm.
#'
#' @examples
#' standard_sample_size(
#'   sd = 1,
#'   effect = 0.5,
#'   power = 0.90,
#'   alpha = 0.05
#' )
#'
#' @export

standard_sample_size <- function(
    sd,
    effect,
    power = 0.90,
    alpha = 0.05,
    allocation = 1
) {

  z_alpha <- qnorm(1 - alpha / 2)
  z_beta  <- qnorm(power)

  n <- ((allocation + 1) *
          (z_beta + z_alpha)^2 *
          sd^2) /
    (allocation * effect^2)

  ceiling(n)
}
