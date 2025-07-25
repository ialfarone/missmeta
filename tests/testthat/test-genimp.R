test_that("genimp returns a list of imputed data frames", {
  df_test <- data.frame(
    EstCR = c(3.7, NA, 4.2, NA),
    EstSR = c(3.2, 3.2, NA, 5.6),
    SECR = c(1.5, 1.3, 2.1, NA),
    SESR = c(0.7, 1.8, NA, 2.2),
    Cor.ws = c(0.75, NA, NA, 0.8),
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

  expect_type(out, "list")
  expect_length(out, 3)
  lapply(out, function(df_out) {
    expect_s3_class(df_out, "data.frame")
    expect_equal(nrow(df_out), nrow(df_test))

    expect_false(any(is.na(df_out$EstCR)))
    expect_false(any(is.na(df_out$EstSR)))
    expect_false(any(is.na(df_out$SECR)))
    expect_false(any(is.na(df_out$SESR)))
    expect_false(any(is.na(df_out$Cor.ws)))
  })
})
