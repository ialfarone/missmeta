#' Convert correlation to covariance
#'
#' Computes the covariance between two variables given their standard deviations and correlation.
#'
#' @param sd1 Standard deviation of the first variable (numeric).
#' @param sd2 Standard deviation of the second variable (numeric).
#' @param rho Correlation between the two variables (numeric).
#'
#' @return Covariance (numeric).
#' @export
#'
#' @examples
#' CorCov(2, 3, 0.5)  # returns 3
CorCov <- function(sd1, sd2, rho) {
  sd1 * sd2 * rho
}
