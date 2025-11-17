#' Flexible Imputation for Multivariate Meta-Analysis
#'
#' Generates multiple imputed datasets for multivariate meta-analysis with missing effect sizes,
#' standard errors, and within-study correlations. The function allows user-defined imputation models
#' for multiple outcome variables (e.g., three different effect sizes) and imputes missing standard
#' errors using a multivariate log-normal model. Missing correlations are imputed with user-specified values.
#'
#' @param df Data frame for meta-analysis with missing values (data.frame).
#' @param iter Number of imputed datasets to generate (numeric).
#' @param imps List of functions, each returning a numeric vector of length \code{n}, used to impute missing values
#'   for each effect size (e.g., \code{list(imp1, imp2, imp3)}).
#' @param effs Character vector. Column names in \code{df} representing the effect sizes to be imputed (e.g., \code{c("eff1", "eff2", "eff3")}).
#' @param ses Character vector. Column names in \code{df} representing the standard errors associated with the effect sizes.
#' @param cors Optional character vector. Column names in \code{df} for within-study correlations between the effect sizes (e.g., \code{c("cor12", "cor13", "cor23")}).
#' @param Ns Character string. Column name in \code{df} representing the total sample size per study (e.g., \code{"N"}).
#' @param imprho Numeric scalar or vector. Imputed value(s) for within-study correlations if \code{cors} contain missing values.
#'   If a scalar, the same value is used for all correlations. If a vector, must match the length of \code{cors}.
#' @param seed Optional numeric value. If provided, the function generates distinct
#' and reproducible imputed datasets by assigning a different seed to each iteration.
#' If \code{NULL} (default), no seed is set.
#'
#' @return A list of \code{iter} imputed data frames, with missing values replaced using user-specified models and multivariate draws.
#' @export
#'
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats var complete.cases
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
#' imp_data <- genimp_multi(
#'   df = df,
#'   iter = 10, # increase number of imputations
#'   imps = imps,
#'   effs = c("eff1", "eff2", "eff3"),
#'   ses = c("se1", "se2", "se3"),
#'   cors = c("cor12", "cor13", "cor23"),
#'   Ns = "N",
#'   imprho = c(0.7, 0.5, 0.6),
#'   seed = 123
#' )
#'
#' # View the first imputed dataset
#' head(imp_data[[1]])
#'
genimp_multi <- function(df, iter = 100,
                         imps,
                         effs,
                         ses,
                         cors = NULL,
                         Ns = "N",
                         imprho,
                         seed = NULL) {

  if (!is.null(seed)) {
    base_seed <- seed
    seeds_iter <- base_seed + seq_len(iter)
  } else {
    seeds_iter <- rep(NA, iter)
  }

  K <- length(effs)
  results <- vector("list", iter)

  for (i in seq_len(iter)) {

    if (!is.na(seeds_iter[i])) {
      set.seed(seeds_iter[i])
    }

    dfi <- df

    for (k in seq_along(effs)) {
      eff <- effs[k]
      Nmiss <- sum(is.na(dfi[[eff]]))
      if (Nmiss > 0) {
        dfi[[eff]][is.na(dfi[[eff]])] <- imps[[k]](Nmiss)
      }
    }

    if (!is.null(cors)) {
      if (length(imprho) == 1) {
        imprho_vec <- rep(imprho, length(cors))
      } else if (length(imprho) == length(cors)) {
        imprho_vec <- imprho
      } else {
        stop("imprho must be a scalar or a vector with length equal to cors.")
      }

      for (idx in seq_along(cors)) {
        cor <- cors[idx]
        cor_val <- imprho_vec[idx]
        dfi[[cor]][is.na(dfi[[cor]])] <- cor_val
      }
    }

    comp <- complete.cases(dfi[, ses])
    if (sum(comp) < 2) {
      stop("Not enough complete cases to estimate covariance for standard errors.")
    }

    log_sig <- sapply(ses, function(se) {
      log(dfi[[se]][comp] * sqrt(dfi[[Ns]][comp]))
    })

    if (is.vector(log_sig)) {
      log_sig <- matrix(log_sig, ncol = length(ses))
    }

    mu_hat <- colMeans(log_sig)
    Sigma_hat <- var(log_sig)

    miss <- !complete.cases(dfi[, ses])
    nmiss <- sum(miss)

    if (nmiss > 0) {
      log_draw <- mvtnorm::rmvnorm(nmiss, mean = mu_hat, sigma = Sigma_hat)
      sigma_imp <- exp(log_draw)

      idx_miss <- which(miss)
      for (k in seq_along(ses)) {
        idx_se <- idx_miss[is.na(dfi[[ses[k]]][idx_miss])]
        if (length(idx_se) > 0) {
          dfi[[ses[k]]][idx_se] <- sigma_imp[seq_along(idx_se), k] / sqrt(dfi[[Ns]][idx_se])
        }
      }
    }

    results[[i]] <- dfi
  }


  out <- list(
    imputations = results,
    iter = iter,
    seed = seed,
    seeds_used = seeds_iter,
    call = match.call(),
    vars = list(effs = effs, ses = ses, cors = cors, N = Ns),
    missing_counts = sapply(c(effs, ses, cors),
                            function(v) sum(is.na(df[[v]])))
  )

  class(out) <- "genimp_multi"
  out
}

