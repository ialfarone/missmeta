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

  out <- data.frame(
    method = method,
    outcome = paste0("eff", 1:p),
    estimate = Q_bar,
    se = se,
    ci_lb = ci_mat[,1],
    ci_ub = ci_mat[,2],
    df = df
  )

  return(out)
}
