#' Helper to create Q_mat and U_list for poolsums
#'
#' @param data Dataframe with effect sizes, standard errors, covariances.
#' @param effs Prefix for effect size columns (e.g., "eff")
#' @param ses Prefix for standard error columns (e.g., "se")
#' @param covs Prefix for covariance columns (e.g., "cov")
#'
#' @return A list with Q_mat and U_list
#' @export
#'
#'
makepooldata <- function(data, effs = "eff", ses = "se", covs = "cov") {
  K <- sum(grepl(paste0("^", effs), names(data)))
  m <- nrow(data)

  # Build Q_mat
  effs_col <- paste0(effs, 1:K)
  Q_mat <- as.matrix(data[, effs_col])

  # Build U_list
  U_list <- lapply(1:m, function(i) {
    se_vals <- unlist(data[i, paste0(ses, 1:K)])
    cov_mat <- matrix(0, K, K)
    diag(cov_mat) <- se_vals^2

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
    cov_mat
  })

  list(Q_mat = Q_mat, U_list = U_list)
}
