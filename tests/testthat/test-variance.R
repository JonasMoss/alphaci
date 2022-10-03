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


lambda <- runif(5)
sigma <- runif(3)
x <- simulate_congeneric(10, k = 7, lambda = lambda, sigma = sigma)
sigma <- cov(x)

test_that("avar yields different results.", {
  results <- c(
    avar(x, cov(x), type = "adf", parallel = FALSE),
    avar(x, cov(x), type = "elliptical", parallel = FALSE),
    avar(x, cov(x), type = "elliptical", parallel = TRUE),
    avar(x, cov(x), type = "normal", parallel = FALSE),
    avar(x, cov(x), type = "normal", parallel = TRUE)
  )
  for (i in seq(length(results) - 1)) {
    expect_false(isTRUE(all.equal(results[i], results[i + 1])))
  }
})

test_that("avar_std yields different results.", {
  results <- c(
    avar_std(x, cov(x), type = "adf", parallel = FALSE),
    avar_std(x, cov(x), type = "elliptical", parallel = FALSE),
    avar_std(x, cov(x), type = "elliptical", parallel = TRUE),
    avar_std(x, cov(x), type = "normal", parallel = FALSE),
    avar_std(x, cov(x), type = "normal", parallel = TRUE)
  )
  for (i in seq(length(results) - 1)) {
    expect_false(isTRUE(all.equal(results[i], results[i + 1])))
  }
})
