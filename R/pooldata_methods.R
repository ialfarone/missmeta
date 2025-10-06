#' Methods for \code{pooldata} objects
#'
#' These methods provide printing, summarizing, and plotting facilities for
#' objects of class \code{pooldata}, created by \code{\link{makepooldata}}.
#'
#' @param x,object An object of class \code{pooldata}.
#' @param ... Further arguments passed to or from other methods (currently ignored).
#'
#' @return
#' - \code{print.pooldata}: prints a short description of the pooldata object.
#' - \code{summary.pooldata}: returns an object of class \code{summary.pooldata}.
#' - \code{print.summary.pooldata}: prints the summary.
#' - \code{plot.pooldata}: creates a simple plot of effect sizes by outcome.
#'
#' @examples
#' dat <- data.frame(
#'   eff1 = rnorm(5, 0, 1),
#'   eff2 = rnorm(5, 0.5, 1),
#'   se1 = runif(5, 0.1, 0.3),
#'   se2 = runif(5, 0.1, 0.3),
#'   cov12 = runif(5, -0.05, 0.05)
#' )
#'
#' pd <- makepooldata(dat)
#' pd            # calls print.pooldata
#' summary(pd)   # calls summary.pooldata
#' plot(pd)      # calls plot.pooldata
#'
#' @details
#' A \code{pooldata} object is a helper structure containing:
#' \itemize{
#'   \item Q_mat: a matrix (studies × outcomes) of effect size estimates
#'   \item U_list: a list of covariance matrices (one per study)
#'   \item K: number of outcomes
#'   \item m: number of studies
#' }
#'
#' @name pooldata-methods
NULL

#' @rdname pooldata-methods
#' @method print pooldata
#' @export
print.pooldata <- function(x, ...) {
  cat("Pooldata object\n")
  cat(" - Studies:", x$m, "\n")
  cat(" - Outcomes:", x$K, "\n")
  cat(" - Use summary() for details\n")
  invisible(x)
}

#' @rdname pooldata-methods
#' @method summary pooldata
#' @export
summary.pooldata <- function(object, ...) {
  means <- colMeans(object$Q_mat, na.rm = TRUE)
  avg_se <- mean(sapply(object$U_list, function(u) mean(sqrt(diag(u)))))
  res <- list(
    m = object$m,
    K = object$K,
    mean_effects = means,
    mean_se = avg_se
  )
  class(res) <- "summary.pooldata"
  res
}

#' @rdname pooldata-methods
#' @export
print.summary.pooldata <- function(x, ...) {
  cat("Summary of Pooldata object\n")
  cat(" - Studies:", x$m, "\n")
  cat(" - Outcomes:", x$K, "\n")
  cat(" - Mean effect sizes:\n")
  print(x$mean_effects)
  cat(" - Average SE across studies:", round(x$mean_se, 3), "\n")
  invisible(x)
}

#' @rdname pooldata-methods
#' @method plot pooldata
#' @importFrom graphics axis boxplot segments
#' @export
plot.pooldata <- function(x, ...) {
  boxplot(x$Q_mat,
          main = "Effect sizes by outcome",
          names = paste0("eff", 1:x$K),
          ylab = "Estimate", ...)
  invisible(x)
}
