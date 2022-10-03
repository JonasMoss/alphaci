get_transformer <- function(transform) {
  if (transform == "none") {
    return(transformer_none)
  }
  if (transform == "log") {
    return(transformer_log)
  }
  if (transform == "fisher") {
    return(transformer_fisher)
  }
  if (transform == "arcsin") {
    return(transformer_arcsin)
  }
  stop(paste0("`transformer = ", transform, "` not supported."))
}

transformer_fisher <- c(
  est = \(est) atanh(est),
  sd = \(est, sd) sd / (1 - est^2),
  inv = tanh
)

transformer_log <- c(
  est = \(est) log(1 - est),
  sd = \(est, sd) sd / abs(1 - est),
  inv = \(x) 1 - exp(x)
)

transformer_none <- c(
  est = \(est) est,
  sd = \(est, sd) sd,
  inv = \(x) x
)

transformer_arcsin <- c(
  est = asin,
  sd = \(est, sd) sd / sqrt(1 - est^2),
  inv = sin
)
