library("psychTools")
x <- bfi[, 1:5]
x[, 1] <- 7 - x[, 1] # Reverse-coded item.
x <- na.omit(x)
avar_std(x, cov(x), type = "adf", parallel = FALSE)
avar(x, cov(x), type = "adf", parallel = FALSE)




library("future.apply")
plan(multisession)

#set.seed(313)
sigma <- runif(5)
lambda <- runif(5)
n <- 1000
results <- future_replicate(10000, {
  x <- simulate_congeneric(n, 5, sigma, lambda)
  alphaci:::alpha_std(cov(x))
})

x <- simulate_congeneric(10000, 5, sigma, lambda)
avar_std(x, cov(x), type = "normal", parallel = FALSE)
var(results) * n


#set.seed(313)
sigma <- runif(5)
lambda <- runif(5)
n <- 1000
k <- 5
sigma <- cov(mtcars[, sample(11, 5, replace = TRUE)])
sigma <- matrix(runif(1), k, k)
diag(sigma) <- 1

results <- future_replicate(10000, {
  x <- LaplacesDemon::rmvl(n, mu = rep(0, k), Sigma = sigma)
  alphaci:::alpha_std(cov(x))
})

x <- LaplacesDemon::rmvl(50000, mu = rep(0, k), Sigma = sigma)
avar_std(x, cov(x), type = "normal", parallel = FALSE)
var(results) * n

x <- LaplacesDemon::rmvl(50, mu = rep(0, k), Sigma = sigma)
alphaci_std(x, bootstrap = TRUE)
