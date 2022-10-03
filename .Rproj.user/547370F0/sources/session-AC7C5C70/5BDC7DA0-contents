#' Calculate the asymptotic variance.
#' @param x Data to estimate alpha on.
#' @param sigma Covariance matrix of x.
#' @param type Type of confidence interval. Either `adf`, `elliptical`, or
#'   `normal`.
#' @param transform Choose between `"fisher"` and `"none"`.
#' @param parallel If `TRUE`, makes calculations under the assumption of a
#'   parallel model.
#' @keywords internal
#' @return Asymptotic variance.
avar <- function(x, sigma, type, parallel) {
  n <- nrow(x)
  k <- ncol(sigma)
  if (type == "elliptical" | type == "normal") {
    g2 <- \(x) mean((x - mean(x))^4) / stats::var(x)^2
    kurtosis <- \(x) (n - 1) / ((n - 2) * (n - 3)) *((n + 1) * g2(x) + 6)
    kurt <- if (type == "normal") 0 else mean(apply(x, 2, kurtosis)) - 3
    var_ell(sigma, kurt, parallel)
  } else {
    gamma_mat_ <- gamma_mat(x)
    d_mat <- matrixcalc::D.matrix(k)
    vec <- t(d_mat) %*% (tr(sigma) * rep(1, k^2) - sum(sigma) * c(diag(k)))
    c(t(vec) %*% gamma_mat_ %*% vec) / sum(sigma)^4 * (k / (k - 1))^2
  }
}

#' Theoretical variance for the elliptical model.
#' @param sigma Covariance matrix.
#' @param kurt Common kurtosis parameter.
#' @param parallel If `TRUE`, calculates the variance under assumption of
#'    a parallel model.
#' @return Theoretical variance.
#' @keywords internal
var_ell <- function(sigma, kurt = 0, parallel = FALSE) {
  k <- ncol(sigma)
  corr <- (1 + kurt / 3)
  if (parallel) {
    2 * k / (k - 1) * corr * (1 - alpha(sigma))^2
  } else {
    sigma2 <- sigma %*% sigma
    sum_s <- sum(sigma)
    tr_s <- tr(sigma)
    sum_s2 <- sum(sigma2)
    tr_s2 <- tr(sigma2)
    q_diff <- sum_s * (tr_s2 + tr_s^2) - 2 * tr_s * sum_s2
    q_mult <- 2 * corr * (k^2 / (k - 1)^2) / sum_s^3
    q_diff * q_mult
  }
}

#' Gamma matrix
#'
#' Calculate the gamma matrix from a matrix of observations.
#' @param x A numeric matrix of observations.
#' @return The sample estimate of the gamma matrix.
#' @keywords internal
gamma_mat <- function(x) {
  i_row <- \(n) unlist(lapply(seq_len(n), seq.int, n))
  i_col <- \(n) rep.int(seq_len(n), times = rev(seq_len(n)))
  y <- t(x) - colMeans(x, na.rm = TRUE)
  z <- y[i_col(ncol(x)), , drop = FALSE] * y[i_row(ncol(x)), , drop = FALSE]
  base::tcrossprod(z - rowMeans(z, na.rm = TRUE)) / nrow(x)
}
