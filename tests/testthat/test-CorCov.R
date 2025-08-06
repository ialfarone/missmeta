test_that("CorCov returns the expected covariance",{
  cov <- CorCov(2, 3, 0.5)
  expect_equal(cov, 3)
          })
