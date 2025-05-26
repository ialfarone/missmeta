#' Generate flexible imputation for bivariate meta-analysis
#'
#' Generates multiple imputed datasets by filling in missing effect sizes using user-defined
#' distributions. The function allows separate imputation models for two outcome variables
#' (e.g., effect sizes from different measures), and uses a log-normal model to impute missing
#' standard errors. Within-study correlations can also be imputed.

#'
#' @param df Dataframe for meta-analysis with missing values at study-level (dataframe).
#' @param iter Number of imputed datasets (numeric).
#'
#' @param imp1 Function that returns a numeric vector of length n for imputed values of eff1.
#' @param imp2 Function that returns a numeric vector of length n for imputed values of eff2.
#'
#' @param eff1 Character string. Column name in `df` for the first effect size to impute (e.g., "EstCR").
#' @param eff2 Character string. Column name in `df` for the second effect size to impute (e.g., "EstSR").
#'
#' @param se1 Character string. Column name in `df` for the standard error associated with `eff1`.
#' @param se2 Character string. Column name in `df` for the standard error associated with `eff2`.
#'
#' @param cor Character string. Column name in `df` for the within-study correlation between effect sizes.
#'
#' @param N Character string. Column name in `df` for the sample size (or total number of observations) per study.
#'
#' @param imprho Numeric. Assumed within-study correlation to impute for studies where `cor` is missing.
#'
#'
#' @return A list of imputed data frames (one per iteration), with missing values replaced using user-specified models.
#' @export
#'
#' @importFrom stats var
#' @importFrom mvtnorm rmvnorm
#'
#' @examples
#'
#'  dmnar <- data.frame(
#'   EstCR = c(3.7, 3.5, 4.2, NA),
#'   EstSR = c(3.2, 3.2, NA, 5.6),
#'   SECR = c(1.5, 1.3, 2.1, NA),
#'   SESR = c(0.7, 1.8, NA, 2.2),
#'   Cor.ws = c(0.75, 0.5, NA, NA),
#'   N = c(100, 150, 120, 95)
#' )
#'
#' imp1 = function(n) rnorm(n, mean = 3, sd = 2)
#' imp2 = function(n) runif(n, min = 4, max = 7)
#'
#' out = genimp(df = dmnar,
#'               iter = 2, # set 1000
#'               imp1 = imp1,
#'               imp2 = imp2,
#'               eff1 = "EstCR",
#'               eff2 = "EstSR",
#'               se1 = "SECR",
#'               se2 = "SESR",
#'               cor = "Cor.ws",
#'               N = "N",
#'               imprho = 0.3)
#' out

genimp = function(df, iter = 100,
                  imp1, imp2,
                  eff1 = "Eff1", eff2 = "Eff2",
                  se1 = "SE1", se2 = "SE2",
                  cor = "Cor.ws", N = "N",
                  imprho = 0.7) {
  results = vector("list", iter)

  for (i in seq_len(iter)) {
    dfi = df
    Nmiss1 = sum(is.na(dfi[[eff1]]))
    Nmiss2 = sum(is.na(dfi[[eff2]]))

    dfi[[eff1]][is.na(dfi[[eff1]])] = imp1(Nmiss1)
    dfi[[eff2]][is.na(dfi[[eff2]])] = imp2(Nmiss2)
    dfi[[cor]][is.na(dfi[[cor]])] = imprho

    comp = !is.na(dfi[[se1]]) & !is.na(dfi[[se2]])
    log_sig = cbind(
      log(dfi[[se1]][comp] * sqrt(dfi[[N]][comp])),
      log(dfi[[se2]][comp] * sqrt(dfi[[N]][comp]))
    )


    mu_hat    = colMeans(log_sig)
    Sigma_hat = var(log_sig)

    miss = is.na(dfi[[se1]]) | is.na(dfi[[se2]])
    log_draw  = mvtnorm::rmvnorm(sum(miss), mean = mu_hat, sigma = Sigma_hat)
    sigma_imp = exp(log_draw)

    idx_se1 = which(miss & is.na(dfi[[se1]]))
    idx_se2 = which(miss & is.na(dfi[[se2]]))

    dfi[[se1]][idx_se1] = sigma_imp[seq_along(idx_se1), 1] / sqrt(dfi[[N]][idx_se1])
    dfi[[se2]][idx_se2] = sigma_imp[seq_along(idx_se2), 2] / sqrt(dfi[[N]][idx_se2])

    results[[i]] <- dfi
  }

  results
}
