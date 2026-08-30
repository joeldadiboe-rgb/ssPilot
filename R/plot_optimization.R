#' Plot pilot and main-trial sample size relationship
#'
#' Creates a plot showing the relationship between pilot-trial
#' sample size and the corresponding main-trial sample size from
#' an optimized sample size calculation. The combination that
#' minimizes the total sample size is highlighted.
#'
#' @param optimization_results A data frame containing the optimization
#'   results. It must contain columns named `pilot_n_per_arm`,
#'   `main_n_per_arm`, and `total_n_per_arm`.
#'
#' @return A ggplot object showing pilot-trial sample size against
#'   main-trial sample size, with the optimal combination highlighted.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme_minimal
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' ucl_results <- optimized_ucl_sample_size(
#'   sd = 1,
#'   effect = 0.50,
#'   power = 0.90,
#'   alpha = 0.05
#' )
#'
#' plot_optimization(ucl_results$optimization_results)
#' }
#'
#' @export

plot_optimization <- function(optimization_results) {

  # Check that optimization_results is a data frame
  if (!is.data.frame(optimization_results)) {
    stop("optimization_results must be a data frame.")
  }

  # Required columns
  required_columns <- c(
    "pilot_n_per_arm",
    "main_n_per_arm",
    "total_n_per_arm"
  )

  # Check for missing columns
  missing_columns <- setdiff(
    required_columns,
    names(optimization_results)
  )

  if (length(missing_columns) > 0) {
    stop(
      "optimization_results must contain the columns: ",
      paste(required_columns, collapse = ", ")
    )
  }

  # Identify the optimal combination
  optimal_index <- which.min(
    optimization_results[["total_n_per_arm"]]
  )

  optimal_result <- optimization_results[optimal_index, , drop = FALSE]

  # Create the plot
  ggplot2::ggplot(
    optimization_results,
    ggplot2::aes(
      x = .data$main_n_per_arm,
      y = .data$pilot_n_per_arm
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_point(
      data = optimal_result,
      size = 3
    ) +
    ggplot2::labs(
      x = "Main-trial sample size per arm",
      y = "Pilot-trial sample size per arm"
    ) +
    ggplot2::theme_minimal()
}
