test_that("sumeth works as expected", {

  res <- data.frame(
    eff1 = c(3.7, 3.5),
    eff2 = c(3.2, 3.2),
    se1 = c(1.5, 1.3),
    se2 = c(2.7, 1.8),
    cov12 = c(0.02, 0.5)
  )

  result <- sumeth(
    eff1 = res$eff1,
    eff2 = res$eff2,
    se1 = res$se1,
    se2 = res$se2,
    cov12 = res$cov12,
    method = "Normal (1, 6)"
  )

  expect_s3_class(result, "sumeth")

  sumres <- summary(result)

  expect_s3_class(sumres, "summary.sumeth")
  expect_true(all(c("estimates", "se", "ci", "df") %in% names(sumres)))


})
