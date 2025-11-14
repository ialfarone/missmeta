test_that("genimp_multi imputes missing values as expected", {
  set.seed(123)

  # Define dummy imputation functions
  imp1 <- function(n) rep(1, n)
  imp2 <- function(n) rep(2, n)
  imp3 <- function(n) rep(3, n)
  imps <- list(imp1, imp2, imp3)

  # Example dataset with missing values
  df <- data.frame(
    eff1 = c(0.5, NA, 0.3, 0.7, 0.5, 0.2),
    eff2 = c(NA, 0.2, 0.3, 0.4, 0.2, 0.5),
    eff3 = c(0.1, 0.2, NA, 0.4, 0.9, 0.7),
    se1  = c(0.1, NA, 0.2, 0.15, 0.1, 0.3),
    se2  = c(NA, 0.25, 0.2, 0.2, 0.4, 0.2),
    se3  = c(0.05, 0.1, NA, 0.15, 0.4, 0.6),
    cor12 = c(NA, NA, 0.5, 0.7, 0.6, 0.6),
    cor13 = c(0.3, NA, NA, 0.6, 0.6, 0.5),
    cor23 = c(NA, 0.3, NA, 0.5, 0.6, 0.7),
    N = c(50, 60, 55, 45, 100, 62)
  )

  # Run the imputation
  out <- genimp_multi(
    df = df,
    iter = 1,
    imps = imps,
    effs = c("eff1", "eff2", "eff3"),
    ses = c("se1", "se2", "se3"),
    cors = c("cor12", "cor13", "cor23"),
    Ns = "N",
    imprho = c(0.7, 0.5, 0.6)
  )

  # --- Basic structure ---
  expect_s3_class(out, "genimp_multi")
  expect_type(out$imputations, "list")
  expect_equal(length(out$imputations), 1)
  expect_true(all(c("eff1", "eff2", "eff3") %in% names(out$imputations[[1]])))

  # --- Check that all missing values are filled ---
  df_imp <- out$imputations[[1]]
  expect_true(all(complete.cases(df_imp[, c("eff1", "eff2", "eff3",
                                            "se1", "se2", "se3",
                                            "cor12", "cor13", "cor23")])))

  # --- Check correct imputed constants for effects ---
  expect_equal(df_imp$eff1[is.na(df$eff1)], rep(1, sum(is.na(df$eff1))))
  expect_equal(df_imp$eff2[is.na(df$eff2)], rep(2, sum(is.na(df$eff2))))
  expect_equal(df_imp$eff3[is.na(df$eff3)], rep(3, sum(is.na(df$eff3))))

  # --- Check correlations filled with correct values ---
  expect_true(all(df_imp$cor12[is.na(df$cor12)] == 0.7))
  expect_true(all(df_imp$cor13[is.na(df$cor13)] == 0.5))
  expect_true(all(df_imp$cor23[is.na(df$cor23)] == 0.6))

  # --- Missing counts correctly reported ---
  expect_named(out$missing_counts)
  expect_equal(unname(out$missing_counts["eff1"]), sum(is.na(df$eff1)))
  expect_equal(unname(out$missing_counts["se1"]), sum(is.na(df$se1)))
})

test_that("genimp_multi handles scalar imprho correctly", {
  set.seed(42)

  imp1 <- function(n) rep(9, n)
  imp2 <- function(n) rep(8, n)
  imp3 <- function(n) rep(7, n)
  imps <- list(imp1, imp2, imp3)

  df <- data.frame(
    eff1 = c(0.5, NA, 0.3, 0.7, 0.5, 0.2),
    eff2 = c(NA, 0.2, 0.3, 0.4, 0.2, 0.5),
    eff3 = c(0.1, 0.2, NA, 0.4, 0.9, 0.7),
    se1  = c(0.1, NA, 0.2, 0.15, 0.1, 0.3),
    se2  = c(NA, 0.25, 0.2, 0.2, 0.4, 0.2),
    se3  = c(0.05, 0.1, NA, 0.15, 0.4, 0.6),
    cor12 = c(NA, NA, NA, NA, NA, NA),
    cor13 = c(NA, NA, NA, NA, NA, NA),
    cor23 = c(NA, NA, NA, NA, NA, NA),
    N = c(50, 60, 55, 45, 100, 62)
  )

  # Run the imputation
  out <- genimp_multi(
    df = df,
    iter = 1,
    imps = imps,
    effs = c("eff1", "eff2", "eff3"),
    ses = c("se1", "se2", "se3"),
    cors = c("cor12", "cor13", "cor23"),
    Ns = "N",
    imprho = 0.8
  )

  df_imp <- out$imputations[[1]]
  expect_true(all(df_imp$cor12 == 0.8))
  expect_true(all(df_imp$cor13 == 0.8))
  expect_true(all(df_imp$cor23 == 0.8))
})

test_that("genimp_multi throws informative error for imprho length mismatch", {
  imp1 <- function(n) rnorm(n)
  imp2 <- function(n) rnorm(n)
  imp3 <- function(n) rnorm(n)
  imps <- list(imp1, imp2, imp3)

  df <- data.frame(
    eff1 = c(NA, 1),
    eff2 = c(2, 3),
    eff3 = c(4, 5),
    se1 = c(0.1, 0.2),
    se2 = c(0.1, 0.2),
    se3 = c(0.1, 0.2),
    cor12 = c(NA, NA),
    cor13 = c(NA, NA),
    cor23 = c(NA, NA),
    N = c(50, 60)
  )

  expect_error(
    genimp_multi(df, 1, imps,
                 effs = c("eff1", "eff2", "eff3"),
                 ses = c("se1", "se2", "se3"),
                 cors = c("cor12", "cor13", "cor23"),
                 Ns = "N",
                 imprho = c(0.5, 0.6) # wrong length
    ),
    "imprho must be a scalar or a vector with length equal to cors"
  )
})
