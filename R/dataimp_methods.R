#' Methods for \code{dataimp} objects
#'
#' These methods provide printing, summarizing, and plotting facilities for
#' objects of class \code{dataimp}, created by \code{\link{genimp}}.
#'
#' @param x,object An object of class \code{dataimp}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return
#' - \code{print.dataimp}: prints a short description of the imputation object.
#' - \code{summary.dataimp}: returns an object of class \code{summary.dataimp}.
#' - \code{print.summary.dataimp}: prints the summary.
#' - \code{plot.dataimp}: creates a basic plot (e.g., distribution of imputed effects).
#'
#'#' @details
#' When \code{genimp()} is called repeatedly (e.g. inside \code{lapply} or \code{mapply}),
#' the result will be a list of \code{dataimp} objects. In that case, use
#' \code{lapply(out, summary)} or \code{purrr::map(out, summary)} to obtain summaries
#' for each element.
#'
#' @examples
#' dmnar <- data.frame(
#'   EstCR = c(3.7, 3.5, 4.2, NA),
#'   EstSR = c(3.2, 3.2, NA, 5.6),
#'   SECR = c(1.5, 1.3, 2.1, NA),
#'   SESR = c(0.7, 1.8, NA, 2.2),
#'   Cor.ws = c(0.75, 0.5, NA, NA),
#'   N = c(100, 150, 120, 95)
#' )
#' imp1 <- function(n) rnorm(n, mean = 3, sd = 2)
#' imp2 <- function(n) runif(n, min = 4, max = 7)
#' out <- genimp(df = dmnar, iter = 2,
#'               imp1 = imp1, imp2 = imp2,
#'               eff1 = "EstCR", eff2 = "EstSR",
#'               se1 = "SECR", se2 = "SESR",
#'               cor = "Cor.ws", N = "N")
#'
#' out              # calls print.dataimp
#' summary(out)     # calls summary.dataimp
#' plot(out)        # calls plot.dataimp
#'
#' @name dataimp-methods
NULL

#' @rdname dataimp-methods
#' @method print dataimp
#' @export
print.dataimp <- function(x, ...) {
  cat("DataImputation object\n")
  cat(" - Number of imputations:", x$iter, "\n")
  cat(" - Number of studies:", nrow(x$imputations[[1]]), "\n")
  cat(" - Use summary() for details\n")
}

#' @rdname dataimp-methods
#' @method summary dataimp
#' @export
summary.dataimp <- function(object, ...) {
  res <- list(
    iter = object$iter,
    n_studies = nrow(object$imputations[[1]]),
    imp_counts = object$missing_counts,
    call = object$call
  )
  class(res) <- "summary.dataimp"
  res
}

#' @rdname dataimp-methods
#' @export
print.summary.dataimp <- function(x, ...) {
  cat("Summary of DataImputation object\n")
  cat(" - Number of imputations:", x$iter, "\n")
  cat(" - Number of studies:", x$n_studies, "\n")
  cat(" - Imputed values per variable:\n")
  print(x$imp_counts)
}

#' @rdname dataimp-methods
#' @method plot dataimp
#' @export
plot.dataimp <- function(x, ...) {
  df0 <- x$imputations[[1]]
  boxplot(df0[[x$vars$eff1]], df0[[x$vars$eff2]],
          names = c(x$vars$eff1, x$vars$eff2),
          main = "Distribution of imputed effects")
}
