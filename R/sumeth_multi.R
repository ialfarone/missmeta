#' Summarizes the results of multivariate meta-analyses conducted with imputed datasets
#'
#' Analyzes the results of multiple imputed datasets by using Rubin's Rules.
#'
#' @param Q_mat A matrix (m × p) of estimates from m imputed datasets and p outcomes
#' @param U_list A list of m covariance matrices (p × p) for each imputation
#' @param method A character string indicating the imputation method used (for reporting)
#'
#' @return A data.frame with pooled estimates, SEs, 95% CIs, and outcome labels
#' @export
#'
#' @examples
#' # Simulate 5 imputations of 3 outcomes
#' set.seed(123)
#' Q_mat <- matrix(rnorm(15, mean = 3, sd = 1), nrow = 5, ncol = 3)
#' colnames(Q_mat) <- c("CR", "SR", "HRQoL")
#'
#' # Create a list of within-imputation covariance matrices
#' U_list <- lapply(1:5, function(i) {
#'   matrix(c(0.2, 0.05, 0.01,
#'            0.05, 0.3, 0.02,
#'            0.01, 0.02, 0.25), nrow = 3)
#' })
#'
#' # Pool using Rubin's Rules
#' res_multi <- sumeth_multi(Q_mat, U_list, method = "Normal imputation")
#'
#' # Print, summarize, and plot
#' print(res_multi)
#' summary(res_multi)
#' plot(res_multi)

sumeth_multi <- function(Q_mat, U_list, method) {
  m <- nrow(Q_mat)
  p <- ncol(Q_mat)

  Q_bar <- colMeans(Q_mat)
  B <- cov(Q_mat)

  U_bar <- Reduce("+", U_list) / m
  Tmat <- U_bar + (1 + 1/m) * B
  se <- sqrt(diag(Tmat))

  df <- (m - 1) * (diag(U_bar) / ((1 + 1/m) * diag(B)) + 1)^2

  ci_mat <- t(sapply(1:p, function(j) {
    Q_bar[j] + c(-1, 1) * qt(0.975, df[j]) * se[j]
  }))

  out <- list(
    method = method,
    estimates = Q_bar,
    se = se,
    ci = ci_mat,
    df = df,
    m = m,
    results = data.frame(
      outcome = paste0("eff", 1:p),
      estimate = Q_bar,
      se = se,
      ci_lb = ci_mat[, 1],
      ci_ub = ci_mat[, 2],
      df = df
    )
  )

  class(out) <- "sumeth_multi"
  out

}
