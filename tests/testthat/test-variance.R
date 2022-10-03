k <- 5
rho <- 0.5
phi <- 2
sigma <- matrix(phi^2 * rho, k, k)
diag(sigma) <- phi^2
x <- NULL

test_that("same result for `parallel = TRUE` and `FALSE` under parallel", {
  expect_equal(
    var_ell(sigma, kurt = 1, parallel = FALSE),
    var_ell(sigma, kurt = 1, parallel = TRUE)
  )
  expect_equal(
    var_ell(sigma, kurt = 2, parallel = FALSE),
    var_ell(sigma, kurt = 2, parallel = TRUE)
  )
})

test_that("different results for different kurtosis when elliptic", {
  expect_lt(
    var_ell(sigma, kurt = 2, parallel = TRUE),
    var_ell(sigma, kurt = 3, parallel = TRUE)
  )
})

test_that("avar equivalent to var_ell under parallell normality", {
  expect_equal(
    avar(x, sigma, type = "normal", parallel = TRUE),
    var_ell(sigma, kurt = 0, parallel = TRUE)
  )
})
