#' Summarizes the results of bivariate meta-analyses conducted with imputed datasets
#'
#' Analyzes the results of multiple imputed datasets by using Rubin's Rules.
#'
#' @param eff1 Character string. Column name in  for the first effect size to impute (e.g., "EstCR").
#' @param eff2 Character string. Column name in  for the second effect size to impute (e.g., "EstSR").
#'
#' @param se1 Character string. Column name in  for the standard error associated with `eff1`.
#' @param se2 Character string. Column name in  for the standard error associated with `eff2`.
#'
#' @param cov12 Character string. Column name in for the covariance between effect sizes.
#'
#' @param method Character string. Name of the distribution used for imputing data.
#'
#'
#' @return A data frame containing the pooled results.
#' @export
#'
#' @importFrom stats cov qt
#'
#'
#' @examples
#'
#'  res <- data.frame(
#'   eff1 = c(3.7, 3.5),
#'   eff2 = c(3.2, 3.2),
#'   se1 = c(1.5, 1.3),
#'   se2 = c(2.7, 1.8),
#'   cov12 = c(0.02, 0.5),
#'   method = "Normal (1, 6)"
#' )
#'
#' sumeth(eff1 = res$eff1, eff2 = res$eff2,
#' se1 = res$se1, se2 = res$se2,
#' cov12 = res$cov12, method = "Normal (1, 6)")
#'

sumeth <- function(eff1, eff2, se1, se2, cov12, method) {
  m = length(eff1)
  Q_mat = cbind(eff1, eff2)
  Q_bar = colMeans(Q_mat)
  B = cov(Q_mat)

  U_list = lapply(1:m, function(i) {
    matrix(c(se1[i]^2, cov12[i], cov12[i], se2[i]^2), nrow = 2)
  })
  U_bar = Reduce("+", U_list) / m
  Tmat = U_bar + (1 + 1/m) * B
  se = sqrt(diag(Tmat))

  df = (m - 1) * (1 + diag(U_bar) / ((1 + 1/m) * diag(B)))^2

  ci1 = Q_bar[1] + c(-1, 1) * qt(0.975, df[1]) * se[1]
  ci2 = Q_bar[2] + c(-1, 1) * qt(0.975, df[2]) * se[2]

  df = data.frame(
    method = method,
    eff1 = Q_bar[1], eff2 = Q_bar[2],
    ci1_lb = ci1[1], ci1_ub = ci1[2],
    ci2_lb = ci2[1], ci2_ub = ci2[2]
  )

  print(df, row.names = F)

}
