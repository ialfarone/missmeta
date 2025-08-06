test_that("makepooldata works as expected with MVMA results", {

  df <- data.frame(
    eff1 = 1, eff2 = 2, eff3 = 3,
    se1 = 0.1, se2 = 0.2, se3 = 0.3,
    cov12 = 0.01, cov13 = 0.02, cov23 = 0.03
  )

  result <- makepooldata(df)

  expect_equal(dim(result$Q_mat), c(1, 3))
  expect_equal(length(result$U_list), 1)

  U1 <- result$U_list[[1]]
  expect_equal(dim(U1), c(3, 3))
  expect_equal(diag(U1), c(0.1^2, 0.2^2, 0.3^2))
  expect_equal(U1[1, 2], 0.01)
  expect_equal(U1[1, 3], 0.02)
  expect_equal(U1[2, 3], 0.03)
})
