f <- function(x,
                        type = c("adf", "elliptical", "normal"),
                        transform = "none",
                        parallel = FALSE,
                        conf_level = 0.95,
                        alternative = c("two.sided", "greater", "less"),
                        bootstrap = FALSE,
                        n_reps = 1000) {

  eval(as.call(c(quote(alphaci_),
          sapply(names(formals()), str2lang),
          standardized = FALSE,
          call = call)))
}

f()
