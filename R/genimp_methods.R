#' Methods for \code{genimp} objects
#'
#' These methods provide printing, summarizing, and plotting facilities for
#' objects of class \code{genimp}, created by \code{\link{genimp}}.
#'
#' @param x,object An object of class \code{genimp}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return
#' - \code{print.genimp}: prints a short description of the imputation object.
#' - \code{summary.genimp}: returns an object of class \code{summary.dataimp}.
#' - \code{print.summary.genimp}: prints the summary.
#' - \code{plot.genimp}: creates a basic plot (e.g., distribution of imputed effects).
#'
#'#' @details
#' When \code{genimp()} is called repeatedly (e.g. inside \code{lapply} or \code{mapply}),
#' the result will be a list of \code{genimp} objects. In that case, use
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
#'
#' imp1 <- function(n) rnorm(n, mean = 3, sd = 2)
#' imp2 <- function(n) runif(n, min = 4, max = 7)
#' out <- genimp(df = dmnar, iter = 2,
#'               imp1 = imp1, imp2 = imp2,
#'               eff1 = "EstCR", eff2 = "EstSR",
#'               se1 = "SECR", se2 = "SESR",
#'               cor = "Cor.ws", N = "N")
#'
#' out              # calls print.genimp
#' summary(out)     # calls summary.genimp
#' plot(out)        # calls plot.genimp
#'
#' @name genimp-methods
NULL

#' @rdname genimp-methods
#' @method print genimp
#' @export
print.genimp <- function(x, ...) {
  cat("DataImputation object\n")
  cat(" - Number of imputations:", x$iter, "\n")
  cat(" - Number of studies:", nrow(x$imputations[[1]]), "\n")
  cat(" - Use summary() for details\n")
}

#' @rdname genimp-methods
#' @method summary genimp
#' @export
summary.genimp <- function(object, ...) {
  res <- list(
    iter = object$iter,
    n_studies = nrow(object$imputations[[1]]),
    imp_counts = object$missing_counts,
    call = object$call
  )
  class(res) <- "summary.genimp"
  res
}

#' @rdname genimp-methods
#' @export
print.summary.genimp <- function(x, ...) {
  cat("Summary of DataImputation object\n")
  cat(" - Number of imputations:", x$iter, "\n")
  cat(" - Number of studies:", x$n_studies, "\n")
  cat(" - Imputed values per variable:\n")
  print(x$imp_counts)
}

#' @rdname genimp-methods
#' @method plot genimp
#' @export
plot.genimp <- function(x, ...) {

  if (length(x$imputations) == 0) {
    warning("No imputations available to plot.")
    return(invisible(NULL))
  }

  df0 <- x$imputations[[1]]

  v1 <- x$vars$eff1
  v2 <- x$vars$eff2

  if (!(v1 %in% names(df0)) | !(v2 %in% names(df0))) {
    stop("Variables ", v1, " and/or ", v2, " are not present in the imputations.")
  }

  y1 <- as.numeric(df0[[v1]])
  y2 <- as.numeric(df0[[v2]])

  boxplot(
    y1, y2,
    names = c(v1, v2),
    main = "Distribution of imputed effects (first imputation)",
    ylab = "Values",
    ...
  )
}
