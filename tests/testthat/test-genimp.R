test_that("genimp returns a list of imputed data frames", {
  df_test <- data.frame(
    EstCR = c(3.7, 1.3, 4.2, NA),
    EstSR = c(3.2, 3.2, NA, 5.6),
    SECR = c(1.5, 1.2, 2.1, NA),
    SESR = c(0.7, 1.8, NA, 2.2),
    Cor.ws = c(0.75, 0.6, NA, 0.8),
    N = c(100, 150, 120, 95)
  )

  imp1 <- function(n) rep(3, n)
  imp2 <- function(n) rep(4, n)

  out <- genimp(
    df = df_test,
    iter = 3,
    imp1 = imp1,
    imp2 = imp2,
    eff1 = "EstCR",
    eff2 = "EstSR",
    se1 = "SECR",
    se2 = "SESR",
    cor = "Cor.ws",
    N = "N",
    imprho = 0.5
  )

  expect_s3_class(out, "dataimp")
  expect_length(out$imputations, 3)

  lapply(out$imputations, function(df_out) {
    expect_s3_class(df_out, "data.frame")
    expect_equal(nrow(df_out), nrow(df_test))

    expect_false(any(is.na(df_out$EstCR)))
    expect_false(any(is.na(df_out$EstSR)))
    expect_false(any(is.na(df_out$SECR)))
    expect_false(any(is.na(df_out$SESR)))
    expect_false(any(is.na(df_out$Cor.ws)))
  })

})

test_that("genimp output structure is consistent", {
  df_test <- data.frame(
    EstCR = c(1, NA, 2),
    EstSR = c(NA, 4, 3),
    SECR = c(0.2, NA, 0.3),
    SESR = c(0.5, NA, 0.4),
    Cor.ws = c(NA, NA, 0.9),
    N = c(50, 60, 70)
  )

  imp1 <- function(n) rep(10, n)
  imp2 <- function(n) rep(20, n)

  out <- genimp(df_test, iter = 2,
                imp1 = imp1, imp2 = imp2,
                eff1 = "EstCR", eff2 = "EstSR",
                se1 = "SECR", se2 = "SESR",
                cor = "Cor.ws", N = "N")

  expect_s3_class(out, "dataimp")
  expect_true(is.list(out))
  expect_named(out, c("imputations", "iter", "call", "vars", "missing_counts"))

  expect_type(out$iter, "double")
  expect_equal(length(out$imputations), 2)
  expect_true(all(c("eff1","eff2","se1","se2","cor","N") %in% names(out$vars)))
})

test_that("genimp correctly counts missing values in input", {
  df_test <- data.frame(
    EstCR = c(3.7, 1.3, 4.2, NA),
    EstSR = c(3.2, 3.2, NA, 5.6),
    SECR = c(1.5, 1.2, 2.1, NA),
    SESR = c(0.7, 1.8, NA, 2.2),
    Cor.ws = c(0.75, 0.6, NA, 0.8),
    N = c(100, 150, 120, 95)
  )

  imp1 <- function(n) rnorm(n)
  imp2 <- function(n) rnorm(n)

  out <- genimp(
    df = df_test,
    iter = 3,
    imp1 = imp1,
    imp2 = imp2,
    eff1 = "EstCR",
    eff2 = "EstSR",
    se1 = "SECR",
    se2 = "SESR",
    cor = "Cor.ws",
    N = "N",
    imprho = 0.5
  )

  expect_equal(out$missing_counts$eff1, 1)
  expect_equal(out$missing_counts$eff2, 1)
  expect_equal(out$missing_counts$se1, 1)
  expect_equal(out$missing_counts$se2, 1)
  expect_equal(out$missing_counts$cor, 1)
})


test_that("genimp produces reproducible imputations with deterministic imputers", {

   df_test <- data.frame(
    EstCR = c(3.7, 1.3, 4.2, NA),
    EstSR = c(3.2, 3.2, NA, 5.6),
    SECR = c(1.5, 1.2, 2.1, NA),
    SESR = c(0.7, 1.8, NA, 2.2),
    Cor.ws = c(0.75, 0.6, NA, 0.8),
    N = c(100, 150, 120, 95)
  )

  imp1 <- function(n) rep(9, n)
  imp2 <- function(n) rep(8, n)

  out <- genimp(
    df = df_test,
    iter = 2,
    imp1 = imp1,
    imp2 = imp2,
    eff1 = "EstCR",
    eff2 = "EstSR",
    se1 = "SECR",
    se2 = "SESR",
    cor = "Cor.ws",
    N = "N",
    imprho = 0.5
  )


  expect_identical(out$imputations[[1]]$EstCR, out$imputations[[2]]$EstCR)
})

test_that("genimp imputes positive standard errors", {
  df_test <- data.frame(
    EstCR = c(3.7, 1.3, 4.2, NA),
    EstSR = c(3.2, 3.2, NA, 5.6),
    SECR = c(1.5, 1.2, 2.1, NA),
    SESR = c(0.7, 1.8, NA, 2.2),
    Cor.ws = c(0.75, 0.6, NA, 0.8),
    N = c(100, 150, 120, 95)
  )

  imp1 <- function(n) rnorm(n, 2)
  imp2 <- function(n) rnorm(n, 3)

  out <- genimp(
    df = df_test,
    iter = 2,
    imp1 = imp1,
    imp2 = imp2,
    eff1 = "EstCR",
    eff2 = "EstSR",
    se1 = "SECR",
    se2 = "SESR",
    cor = "Cor.ws",
    N = "N",
    imprho = 0.5
  )

  lapply(out$imputations, function(df_out) {
    expect_true(all(df_out$SECR > 0))
    expect_true(all(df_out$SESR > 0))
  })
})


