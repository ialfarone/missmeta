#' Methods for \code{genimp_multi} objects
#'
#' Printing and summarizing methods for objects returned by \code{\link{genimp_multi}}.
#'
#' @param x,object An object of class \code{genimp_multi}.
#' @param ... Further arguments passed to or from other methods (currently ignored).
#'
#' @return
#' - \code{print.genimp_multi}: prints a short description of the object.
#' - \code{summary.genimp_multi}: returns an object of class \code{summary.genimp_multi}.
#' - \code{print.summary.genimp_multi}: prints the summary.
#'
#' @examples
#' # Define dummy imputation functions
#' imp1 <- function(n) rnorm(n, 0, 1)
#' imp2 <- function(n) rnorm(n, 1, 1)
#' imp3 <- function(n) rnorm(n, 2, 1)
#'
#' imps <- list(imp1, imp2, imp3)
#'
#' # Create example dataset with missing values
#' set.seed(123)
#' df <- data.frame(
#'   eff1 = c(0.5, NA, 0.3, 0.7, 0.5, 0.2),
#'   eff2 = c(NA, 0.2, 0.3, 0.4, 0.2, 0.5),
#'   eff3 = c(0.1, 0.2, NA, 0.4, 0.9, 0.7),
#'   se1  = c(0.1, NA, 0.2, 0.15, 0.1, 0.3),
#'   se2  = c(NA, 0.25, 0.2, 0.2, 0.4, 0.2),
#'   se3  = c(0.05, 0.1, NA, 0.15, 0.4, 0.6),
#'   cor12 = c(NA, NA, 0.5, 0.7, 0.6, 0.6),
#'   cor13 = c(0.3, NA, NA, 0.6, 0.6, 0.5),
#'   cor23 = c(NA, 0.3, NA, 0.5, 0.6, 0.7),
#'   N = c(50, 60, 55, 45, 100, 62)
#' )
#'
#' # Run the imputation
#' out <- genimp_multi(
#'   df = df,
#'   iter = 10, # increase number of imputations
#'   imps = imps,
#'   effs = c("eff1", "eff2", "eff3"),
#'   ses = c("se1", "se2", "se3"),
#'   cors = c("cor12", "cor13", "cor23"),
#'   Ns = "N",
#'   imprho = c(0.7, 0.5, 0.6)
#' )
#'
#' summary(out)
#'
#' @name genimp-multi-methods
#' @title Methods for genimp_multi objects
NULL

#' @rdname genimp-multi-methods
#' @method print genimp_multi
#' @export
print.genimp_multi <- function(x, ...) {
  cat("Multivariate genimputation object\n")
  cat(" - Number of imputations:", x$iter, "\n")
  cat(" - Number of studies:", nrow(x$imputations[[1]]), "\n")
  cat(" - Number of outcomes:", length(x$vars$effs), "\n")
  cat(" - Use summary() for details\n")
  invisible(x)
}

#' @rdname genimp-multi-methods
#' @method summary genimp_multi
#' @export
summary.genimp_multi <- function(object, ...) {
  res <- list(
    iter = object$iter,
    n_studies = nrow(object$imputations[[1]]),
    n_outcomes = length(object$vars$effs),
    imp_counts = object$missing_counts,
    call = object$call
  )
  class(res) <- "summary.genimp_multi"
  res
}

#' @rdname genimp-multi-methods
#' @export
print.summary.genimp_multi <- function(x, ...) {
  cat("Summary of Multivariate genimputation object\n")
  cat(" - Number of imputations:", x$iter, "\n")
  cat(" - Number of studies:", x$n_studies, "\n")
  cat(" - Outcomes:", x$n_outcomes, "\n")
  cat(" - Imputed values per variable:\n")
  print(x$imp_counts)
  invisible(x)
}

#' @rdname genimp-multi-methods
#' @method plot genimp_multi
#' @export
plot.genimp_multi <- function(x, ...) {

  if (length(x$imputations) == 0) {
    warning("No imputations available to plot.")
    return(invisible(NULL))
  }

  df0 <- x$imputations[[1]]

  effs <- x$vars$effs

  missing_cols <- effs[!(effs %in% names(df0))]
  if (length(missing_cols) > 0) {
    stop("The following effect size columns are missing in imputations: ",
         paste(missing_cols, collapse = ", "))
  }

  y <- df0[effs]
  y <- as.data.frame(lapply(y, as.numeric))

  boxplot(
    y,
    main = "Distribution of imputed effect sizes (first imputation)",
    ylab = "Effect size values",
    ...
  )
}

