---
title: "Simple simulation"
author: "Jonas Moss"
format: html
editor: visual
---

## Simple simulation

We run some simple simulations

```{r}
set.seed(313)
x = simulate_tau(100, k, 1, 3)
true = alpha(rep(3, k), rep(1, k))

alphaci(x, bootstrap = TRUE, transform = "fisher")
alphaci(x, bootstrap = TRUE, transform = "none")
```

You can add options to executable code like this

```{r}
#| echo: false
2 * 2
```

The `echo: false` option disables the printing of code (only output is displayed).
