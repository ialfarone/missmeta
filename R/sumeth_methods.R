#' Methods for \code{sumeth} objects
#'
#' Provides printing and summarizing for objects returned by \code{\link{sumeth}}.
#'
#' @param x,object An object of class \code{sumeth}.
#' @param ... Further arguments passed to or from other methods (currently ignored).
#'
#' @return
#' - \code{print.sumeth}: prints a compact summary table of the pooled results.
#' - \code{summary.sumeth}: returns an object of class \code{summary.sumeth}.
#' - \code{print.summary.sumeth}: prints the detailed summary including degrees of freedom.
#'
#' @examples
#' res <- data.frame(
#'   eff1 = c(3.7, 3.5),
#'   eff2 = c(3.2, 3.2),
#'   se1 = c(1.5, 1.3),
#'   se2 = c(2.7, 1.8),
#'   cov12 = c(0.02, 0.5)
#' )
#' out <- sumeth(
#'   eff1 = res$eff1, eff2 = res$eff2,
#'   se1 = res$se1, se2 = res$se2,
#'   cov12 = res$cov12, method = "Normal (1, 6)"
#' )
#' out
#' summary(out)
#'
#' @name sumeth-methods
NULL

#' @rdname sumeth-methods
#' @method print sumeth
#' @export
print.sumeth <- function(x, ...) {
  df <- data.frame(
    method = x$method,
    eff1 = x$Q_bar[1], se1 = x$se[1],
    ci1_lb = x$ci[1, 1], ci1_ub = x$ci[1, 2],
    eff2 = x$Q_bar[2], se2 = x$se[2],
    ci2_lb = x$ci[2, 1], ci2_ub = x$ci[2, 2]
  )
  print(df, row.names = FALSE)
  invisible(x)
}

#' @rdname sumeth-methods
#' @method summary sumeth
#' @export
summary.sumeth <- function(object, ...) {
  res <- list(
    method = object$method,
    estimates = object$Q_bar,
    se = object$se,
    ci = object$ci,
    df = object$df,
    imputations = object$m
  )
  class(res) <- "summary.sumeth"
  res
}

#' @rdname sumeth-methods
#' @export
print.summary.sumeth <- function(x, ...) {
  cat("Summary of sumeth object\n")
  cat(" - Method:", x$method, "\n")
  cat(" - Number of imputations:", x$imputations, "\n")
  cat("\nEstimates and 95% CIs:\n")
  tab <- data.frame(
    outcome = c("eff1","eff2"),
    estimate = x$estimates,
    se = x$se,
    ci_lb = x$ci[,1],
    ci_ub = x$ci[,2],
    df = x$df
  )
  print(tab, row.names = FALSE)
  invisible(x)
}

#' @rdname sumeth-methods
#' @method plot sumeth
#' @export
plot.sumeth <- function(x, ...) {
  stopifnot(inherits(x, "sumeth"))

  outcomes <- c("eff1", "eff2")
  est <- as.numeric(x$Q_bar)
  ci <- as.matrix(x$ci)

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

