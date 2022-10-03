alphaci_std <- function(x,
                    type = c("adf", "elliptical", "normal"),
                    transform = "none",
                    parallel = FALSE,
                    conf_level = 0.95,
                    alternative = c("two.sided", "greater", "less"),
                    bootstrap = FALSE,
                    n_reps = 1000) {
  call <- match.call()

  type <- match.arg(type)
  alternative <- match.arg(alternative)

  transformer <- get_transformer(transform)
  quants <- limits(alternative, conf_level)
  x <- stats::na.omit(as.matrix(x))

  sigma <- stats::cov(x)
  est <- alpha(sigma)
  sd <- sqrt(avar(x, sigma, type, parallel))

  ci <- if (!bootstrap) {
    ci_asymptotic(est, sd, nrow(x), transformer, quants)
  } else {
    ci_boot(x, est, sd, type, transformer, parallel, quants, n_reps)
  }

  names(ci) <- quants
  attr(ci, "conf.level") <- conf_level
  attr(ci, "alternative") <- alternative
  attr(ci, "type") <- type
  attr(ci, "n") <- nrow(x)
  attr(ci, "parallel") <- parallel
  attr(ci, "transform") <- transform
  attr(ci, "bootstrap") <- bootstrap
  attr(ci, "n_reps") <- n_reps
  attr(ci, "estimate") <- est
  attr(ci, "sd") <- sd
  attr(ci, "call") <- call
  class(ci) <- "alphaci"
  ci[2] <- min(ci[2], 1)
  ci
}
