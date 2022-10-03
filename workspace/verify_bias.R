lambda = runif(5)
sigma = runif(5)

alpha_bias(lambda, sigma)
omega(sigma, lambda) - alpha(sigma, lambda)
var(lambda) * (k - 1) / k
var(lambda) / (k * mean(lambda)^2 + mean(sigma^2))

1 / (k + mean(sigma^2) / mean(lambda)^2) / 2



var(lambda) / (var(lambda) + mean(sigma^2))
