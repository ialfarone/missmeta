test_that("sumeth_multi produces expected results for MVMA", {

  Q_mat <- matrix(
    c(1, 2,
      1.5, 2.5,
      2, 3),
    ncol = 2,
    byrow = TRUE
  )

  U_list <- list(
    matrix(c(0.1^2, 0.01,
             0.01, 0.2^2), 2, 2),
    matrix(c(0.15^2, 0.02,
             0.02, 0.25^2), 2, 2),
    matrix(c(0.2^2, 0.03,
             0.03, 0.3^2), 2, 2)
  )

  res <- sumeth_multi(Q_mat, U_list, method = "test")

  expect_s3_class(res, "sumeth_multi")

  sumres <- summary(res)
  expect_s3_class(sumres, "summary.sumeth_multi")
  expect_true(all(c("outcome", "estimate", "se", "ci_lb", "ci_ub", "df")
                  %in% colnames(sumres$table)))
})
