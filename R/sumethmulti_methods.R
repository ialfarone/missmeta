#' Methods for \code{sumeth_multi} objects
#'
#' These methods provide printing, summarizing, and plotting facilities for
#' objects of class \code{sumeth_multi}, created by \code{\link{sumeth_multi}}.
#'
#' @param x,object An object of class \code{sumeth_multi}.
#' @param ... Further arguments passed to or from other methods (currently ignored).
#'
#' @return
#' - \code{print.sumeth_multi}: prints a short description of the pooled results.
#' - \code{summary.sumeth_multi}: returns an object of class \code{summary.sumeth_multi}.
#' - \code{print.summary.sumeth_multi}: prints the summary in a readable format.
#' - \code{plot.sumeth_multi}: creates a forest-style plot of pooled effect sizes with 95\% CIs.
#'
#' @examples
#' set.seed(123)
#' Q_mat <- matrix(rnorm(15, mean = 3, sd = 1), nrow = 5, ncol = 3)
#' colnames(Q_mat) <- c("CR", "SR", "HRQoL")
#'
#' U_list <- lapply(1:5, function(i) {
#'   matrix(c(0.2, 0.05, 0.01,
#'            0.05, 0.3, 0.02,
#'            0.01, 0.02, 0.25), nrow = 3)
#' })
#'
#' res_multi <- sumeth_multi(Q_mat, U_list, method = "Normal imputation")
#' res_multi        # calls print.sumeth_multi
#' summary(res_multi)  # calls summary.sumeth_multi
#' plot(res_multi)     # calls plot.sumeth_multi
#'
#' @details
#' A \code{sumeth_multi} object is a structure containing:
#' \itemize{
#'   \item \code{method}: the imputation method name
#'   \item \code{m}: number of imputations
#'   \item \code{p}: number of outcomes
#'   \item \code{estimates}: pooled effect size estimates
#'   \item \code{se}: pooled standard errors
#'   \item \code{ci}: confidence interval matrix
#'   \item \code{results}: tidy data frame with all results
#' }
#'
#' @name sumeth-multi-methods
#' @rdname sumeth-multi-methods
#' @method print sumeth_multi
#' @export
print.sumeth_multi <- function(x, ...) {
  cat("Multivariate Rubin's Rules object\n")
  cat(" - Method:", x$method, "\n")
  cat(" - Number of imputations:", x$m, "\n")
  cat(" - Outcomes:", x$p, "\n\n")
  print(x$results, row.names = FALSE)
  invisible(x)
}

#' @rdname sumeth-multi-methods
#' @method summary sumeth_multi
#' @export
summary.sumeth_multi <- function(object, ...) {
  res <- list(
    method = object$method,
    m = object$m,
    p = object$p,
    table = object$results
  )
  class(res) <- "summary.sumeth_multi"
  res
}

#' @rdname sumeth-multi-methods
#' @export
print.summary.sumeth_multi <- function(x, ...) {
  cat("Summary of Multivariate Rubin's Rules object\n")
  cat(" - Method:", x$method, "\n")
  cat(" - Number of imputations:", x$m, "\n")
  cat(" - Outcomes:", x$p, "\n\n")
  print(x$table, row.names = FALSE)
  invisible(x)
}

#' @rdname sumeth-multi-methods
#' @method plot sumeth_multi
#' @export
plot.sumeth_multi <- function(x, ...) {


  est <- as.numeric(x$estimates)
  ci <- as.matrix(x$ci)
  outcomes <- x$results$outcome

  graphics::plot.default(est, seq_along(outcomes),
                         xlim = range(ci), ylim = c(0.5, length(outcomes) + 0.5),
                         yaxt = "n", pch = 19,
                         xlab = "Estimate (95% CI)", ylab = "",
                         main = paste("Pooled estimates -", x$method), ...)

  axis(2, at = seq_along(outcomes), labels = outcomes, las = 1)

  for (i in seq_along(outcomes)) {
    segments(ci[i, 1], i, ci[i, 2], i, lwd = 2)
  }

  invisible(x)
}

