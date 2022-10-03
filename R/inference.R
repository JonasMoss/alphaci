#' Estimate coefficient alpha with a confidence interval.
#'
#' This function makes use of `future.apply` when bootstrapping. Supports
#'    both ordinary coefficient alpha (`alphaci`) and standardized coefficient
#'    alpha (`alphaci_std`).
#'
#' None of the methods accept missing data.
#'
#'
#'
#' @export
#' @param x Input data data can be converted to a matrix using `as.matrix`.
#'   Missing values will be ignored.
#' @param type Type of confidence interval. Either `adf`, `elliptical`, or
#'   `normal`.
#' @param transform One of `"none"`, `"log"`, `"fisher"`, and `"arcsin`.
#'   Defaults to `"none"`.
#' @param parallel If `TRUE`, makes calculations under the assumption of a
#'   parallel model. Defaults to `FALSE`.
#' @param alternative A character string specifying the alternative hypothesis,
#'   must be one of `"two.sided"` (default), `"greater"` or `"less"`.
#' @param conf_level Confidence level.
#' @param bootstrap If `TRUE`, performs a studentized bootstrap with `n_reps`
#'   repetitions. Defaults to `FALSE`.
#' @param n_reps Number of bootstrap samples if `bootstrap = TRUE`. Ignored if
#'   `bootstrap = FALSE`. Defaults to `1000`.
#' @return A vector of class `alphaci` containing the confidence end points.
#' @name alphaci
#' @examples
#' library("alphaci")
#' library("psychTools")
#' x <- bfi[, 1:5]
#' x[, 1] <- 7 - x[, 1] # Reverse-coded item.
#' alphaci(x)
#' alphaci_std(x)
#'
alphaci <- function(x,
                    type = c("adf", "elliptical", "normal"),
                    transform = "none",
                    parallel = FALSE,
                    conf_level = 0.95,
                    alternative = c("two.sided", "greater", "less"),
                    bootstrap = FALSE,
                    n_reps = 1000) {
  call <- match.call()
  alphaci_(x,
    type,
    transform,
    parallel,
    conf_level,
    alternative,
    bootstrap,
    n_reps,
    standardized = FALSE,
    call
  )
}

#' @export
#' @rdname alphaci
alphaci_std <- function(x,
                        type = c("adf", "elliptical", "normal"),
                        transform = "none",
                        parallel = FALSE,
                        conf_level = 0.95,
                        alternative = c("two.sided", "greater", "less"),
                        bootstrap = FALSE,
                        n_reps = 1000) {
  call <- match.call()
  alphaci_(x,
    type,
    transform,
    parallel,
    conf_level,
    alternative,
    bootstrap,
    n_reps,
    standardized = TRUE,
    call
  )
}

alphaci_ <- function(x,
                     type = c("adf", "elliptical", "normal"),
                     transform,
                     parallel,
                     conf_level,
                     alternative = c("two.sided", "greater", "less"),
                     bootstrap,
                     n_reps,
                     standardized,
                     call) {
  type <- match.arg(type)
  alternative <- match.arg(alternative)
  transformer <- get_transformer(transform)

  quants <- limits(alternative, conf_level)
  x <- stats::na.omit(as.matrix(x))

  sigma <- stats::cov(x)
  if (!standardized) {
    est <- alpha(sigma)
    sd <- sqrt(avar(x, sigma, type, parallel))
  } else {
    est <- alpha_std(sigma)
    sd <- sqrt(avar_std(x, sigma, type, parallel))
  }

  ci <- if (!bootstrap) {
    ci_asymptotic(est, sd, nrow(x), transformer, quants)
  } else {
    ci_boot(
      x,
      est,
      sd,
      type,
      transformer,
      parallel,
      quants,
      n_reps,
      standardized = standardized
    )
  }

  names(ci) <- quants
  attr(ci, "conf_level") <- conf_level
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
  cat("Call: ", paste(deparse(at("call")),
    sep = "\n",
    collapse = "\n"
  ), "\n\n", sep = "")

  if (!is.null(x)) {
    cat(format(100 * at("conf_level")),
      "% confidence interval (n = ", at("n"), ").\n",
      sep = ""
    )
    print(x[1:2], digits = digits)
    cat("\n")
  }

  if (!is.null(at("estimate"))) {
    cat("Sample estimates.\n")
    print(c(
      alpha = at("estimate"),
      sd = at("sd")
    ),
    digits = digits
    )
  }
  invisible(x)
}
