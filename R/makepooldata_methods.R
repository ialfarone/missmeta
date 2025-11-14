#' Methods for \code{makepooldata} objects
#'
#' These methods provide printing, summarizing, and plotting facilities for
#' objects of class \code{makepooldata}, created by \code{\link{makepooldata}}.
#'
#' @param x,object An object of class \code{makepooldata}.
#' @param ... Additional arguments passed to or from methods.
#'
#' @return
#' - \code{print.makepooldata}: prints a short description of the makepooldata object.
#' - \code{summary.makepooldata}: returns an object of class \code{summary.makepooldata}.
#' - \code{print.summary.makepooldata}: prints the summary.
#' - \code{plot.makepooldata}: creates a simple plot of effect sizes by outcome.
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
#' pd            # calls print.makepooldata
#' summary(pd)   # calls summary.makepooldata
#' plot(pd)      # calls plot.makepooldata
#'
#' @details
#' A \code{makepooldata} object is a helper structure containing:
#' \itemize{
#'   \item Q_mat: a matrix (studies × outcomes) of effect size estimates
#'   \item U_list: a list of covariance matrices (one per study)
#'   \item K: number of outcomes
#'   \item m: number of studies
#' }
#'
#' @name makepooldata-methods
NULL

#' @rdname makepooldata-methods
#' @method print makepooldata
#' @export
print.makepooldata <- function(x, ...) {
  cat("Makepooldata object\n")
  cat(" - Studies:", x$m, "\n")
  cat(" - Outcomes:", x$K, "\n")
  cat(" - Use summary() for details\n")
  invisible(x)
}

#' @rdname makepooldata-methods
#' @method summary makepooldata
#' @export
summary.makepooldata <- function(object, ...) {
  means <- colMeans(object$Q_mat, na.rm = TRUE)
  avg_se <- mean(sapply(object$U_list, function(u) mean(sqrt(diag(u)))))
  res <- list(
    m = object$m,
    K = object$K,
    mean_effects = means,
    mean_se = avg_se
  )
  class(res) <- "summary.makepooldata"
  res
}

#' @rdname makepooldata-methods
#' @export
print.summary.makepooldata <- function(x, ...) {
  cat("Summary of Makepooldata object\n")
  cat(" - Studies:", x$m, "\n")
  cat(" - Outcomes:", x$K, "\n")
  cat(" - Mean effect sizes:\n")
  print(x$mean_effects)
  cat(" - Average SE across studies:", round(x$mean_se, 3), "\n")
  invisible(x)
}

#' @rdname makepooldata-methods
#' @method plot makepooldata
#' @importFrom graphics axis boxplot segments
#' @export
plot.makepooldata <- function(x, ...) {
  boxplot(x$Q_mat,
          main = "Effect sizes by outcome",
          names = paste0("eff", 1:x$K),
          ylab = "Estimate", ...)
  invisible(x)
}
