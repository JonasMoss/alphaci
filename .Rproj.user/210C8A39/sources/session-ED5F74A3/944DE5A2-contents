#' Estimate coefficient alpha with a confidence interval.
#'
#' This function makes use of `future.apply` when bootstrapping.
#'
#' @export
#' @param x Data to estimate alpha on.
#' @param type Type of confidence interval. Either `adf`, `elliptical`, or
#'   `normal`.
#' @param transform One of `"none"`, `"log"`, `"fisher"`, and `"arcsin`.
#'   Defaults to `"none"`.
#' @param parallel If `TRUE`, makes calculations under the assumption of a
#'   parallel model. Default to `FALSE`.
#' @param alternative Direction of the confidence interval.
#' @param conf_level Confidence level.
#' @param bootstrap If `TRUE`, performs a studentized bootstrap with `n_reps`
#'   repetitions. Defaults to `FALSE`.
#' @param n_reps Number of bootstrap samples if `bootstrap = TRUE`. Ignored if
#'   `bootstrap = FALSE`.
#' @return An appropriate object.
alphaci <- function(x,
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

#' @export
print.alphaci <- function(x, digits = getOption("digits"), ...) {
  at <- \(y) attr(x, y)
  cat("Call: ", paste(deparse(at("call")), sep = "\n",
                         collapse = "\n"), "\n\n", sep = "")

  if (!is.null(x)) {
    cat(format(100 * at("conf.level")),
        "% confidence interval (n = ", at("n"), ").\n", sep = "")
    print(x[1:2], digits = digits)
    cat("\n")
  }

  if (!is.null(at("estimate"))) {
    cat("Sample estimates.\n")
    print(c(
      alpha = at("estimate"),
      sd = at("sd")),
      digits = digits)
  }
  invisible(x)
}
