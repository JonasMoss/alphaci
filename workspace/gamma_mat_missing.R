set.seed(313)
x = simulate_tau(10, k, 1, 3)
true = alpha(rep(3, k), rep(1, k))





gamma_mat <- function(x) {
  i_row <- \(n) unlist(lapply(seq_len(n), seq.int, n))
  i_col <- \(n) rep.int(seq_len(n), times = rev(seq_len(n)))
  y <- t(x) - colMeans(x, na.rm = TRUE)
  z <- y[i_col(ncol(x)), , drop = FALSE] * y[i_row(ncol(x)), , drop = FALSE]
  base::tcrossprod(z - rowMeans(z, na.rm = TRUE)) / nrow(x)
}
