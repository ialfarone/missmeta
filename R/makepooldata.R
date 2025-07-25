#' Helper to create Q_mat and U_list for sumeth_multi
#'
#' @param data Dataframe with effect sizes (named eff1, eff2, ...), standard errors (named se1, se2, ...),
#' covariances (named cov12, cov13,...).
#' @param effs Prefix for effect size columns (e.g., "eff")
#' @param ses Prefix for standard error columns (e.g., "se")
#' @param covs Prefix for covariance columns (e.g., "cov")
#'
#' @return A list with Q_mat and U_list
#' @export
#'
makepooldata <- function(data, effs = "eff", ses = "se", covs = "cov") {
  K <- sum(grepl(paste0("^", effs), names(data)))
  m <- nrow(data)

  effs_col <- paste0(effs, 1:K)
  Q_mat <- as.matrix(data[, effs_col])

  U_list <- lapply(1:m, function(i) {
    se_vals <- unlist(data[i, paste0(ses, 1:K)])
    cov_mat <- matrix(0, K, K)
    diag(cov_mat) <- se_vals^2

    if ("cov" %in% names(data) && K == 2) {
      cov_val <- data$cov[i]
      cov_mat[1, 2] <- cov_val
      cov_mat[2, 1] <- cov_val
    } else {

      for (j in 1:(K-1)) {
        for (k in (j+1):K) {
          cov_name <- paste0(covs, j, k)
          if (cov_name %in% names(data)) {
            cov_val <- data[[cov_name]][i]
            cov_mat[j, k] <- cov_val
            cov_mat[k, j] <- cov_val
          }
        }
      }
    }
    cov_mat
  })
  list(Q_mat = Q_mat, U_list = U_list)
}
