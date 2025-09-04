
#'
#' Compute r^2 value
#'
#' @param x Observations
#' @param y Predictions
#'
#' @keywords internal
#'
compute_r2 <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Cannot compute r2: x/y lengths differ")
  }
  if (length(x) == 0) {
    stop("Cannot compute r2: input data contains no elements")
  }

  # Square of Pearson correlation coefficient.
  r2 <- cor(x, y)^2

  # # Total sum of squares.
  # ss_tot <- sum((x - mean(x)) ^ 2)

  # # Residual sum of squares.
  # ss_res <- sum((x - y) ^ 2)

  # r2 <- 1 - (ss_res / ss_tot)

  if (is.na(r2)) {
    return(0)
  }
  return(r2)
}

#'
#' Compute root mean square error.
#'
#' This is in the units of the variable.
#'
#' @param x Observations
#' @param y Predictions
#'
#' @keywords internal
#'
compute_rmse <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Cannot compute rmse: x/y lengths differ")
  }
  if (length(x) == 0) {
    stop("Cannot compute rmse: input data contains no elements")
  }
  return(sqrt(mean((y - x)^2)))
}

#'
#' Compute nash-sutcliffe efficiency (-Inf - 1)
#'
#' This is one minus the ratio of the error variance in the predictions to the
#' variance in the observations.
#'
#' An efficiency of 1 means the predictions perfectly match the observations.
#' An efficiency of 0 means that the predictions are equally accurate predictors
#' as the mean of the observed data. An efficiency of < 0 means that the mean of
#' the observations is a better predictor than the model.
#'
#' @param x Observations
#' @param y Predictions
#'
#' @keywords internal
#'
compute_nse <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Cannot compute nse: x/y lengths differ")
  }
  if (length(x) == 0) {
    stop("Cannot compute nse: input data contains no elements")
  }
  return(1 - (sum((y - x) ^ 2)) / sum((x - mean(x)) ^ 2))
}

#'
#' Computes RSR of predictions to observed values.
#'
#' Compute ratio of RMSE to standard deviation of observations (0 - Inf).
#'
#' Value of 0 means 0 RMSE (ie perfect fit).
#'
#' @param x Observations
#' @param y Predictions
#'
#' @keywords internal
#'
compute_rsr <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Cannot compute rsr: x/y lengths differ")
  }
  if (length(x) == 0) {
    stop("Cannot compute rsr: input data contains no elements")
  }
  return(compute_rmse(x, y) / sd(x))
}

#'
#' Compute bias of predictions to observations.
#'
#' @param observations Observed data.
#' @param predictions Predicted data.
#'
#' @keywords internal
#'
compute_bias <- function(observations, predictions) {
  return(mean(predictions - observations))
}
