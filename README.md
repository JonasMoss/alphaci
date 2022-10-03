
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alphaci <img src="man/figures/logo.png" align="right" width="160" height="135"/>

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/alphaci.png)](https://cran.r-project.org/package=alphaci)
[![R-CMD-check](https://github.com/JonasMoss/alphaci/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JonasMoss/alphaci/actions/workflows/R-CMD-check.yaml)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/JonasMoss/alphaci/branch/main/graph/badge.svg)](https://app.codecov.io/gh/JonasMoss/alphaci?branch=main)

An `R` package for doing inference with coefficient alpha and
standardized alpha. Many methods are supported, with special emphasis on
small samples and non-normality. ***Note:*** This package is under
active development along with a companion paper.

## Installation

The package is not available on `CRAN` yet, so use the following command
from inside `R`:

``` r
# install.packages("remotes")
remotes::install_github("JonasMoss/alphaci")
```

## Usage

Call the `library` function and load some data:

``` r
library("alphaci")
library("psychTools")
x <- bfi[, 1:5]
x[, 1] <- 7 - x[, 1] # Reverse-coded item.
head(x)
#>       A1 A2 A3 A4 A5
#> 61617  5  4  3  4  4
#> 61618  5  4  5  2  5
#> 61620  2  4  5  4  4
#> 61621  3  4  6  5  5
#> 61622  5  3  3  4  5
#> 61623  1  6  5  6  5
```

Then calculate an asymptotically distribution-free confidence interval
for ![\alpha](https://latex.codecogs.com/svg.latex?%5Calpha "\alpha"),

``` r
alphaci(x)
#> Call: alphaci(x = x)
#> 
#> 95% confidence interval (n = 2709).
#>     0.025     0.975 
#> 0.6828923 0.7246195 
#> 
#> Sample estimates.
#>     alpha        sd 
#> 0.7037559 0.5536964
```

You can also calculate confidence intervals for standardized alpha

``` r
alphaci_std(x)
#> Call: alphaci_std(x = x)
#> 
#> 95% confidence interval (n = 2709).
#>     0.025     0.975 
#> 0.6938373 0.7331658 
#> 
#> Sample estimates.
#>     alpha        sd 
#> 0.7135016 0.5218675
```

## Supported techniques

`alphaci` supports three basic asymptotic confidence interval
constructios. The asymptotically distribution-free interval of
@Maydeu-Olivares2007-zh, the pseudo-elliptical construction of
@Yuan2002-oy, and the normal method of @Van_Zyl2000-si.

| Method       | Description                                                                                                                                                                                                                                                                                                            |
|--------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `adf`        | The asymptotic distribution free method \[@Maydeu-Olivares2007-zh\]. The method is asymptotically correct, but has poor small-sample performance.                                                                                                                                                                      |
| `elliptical` | The elliptical or pseudo-elliptical kurtosis correction \[@Yuan2002-oy\]. Uses the unbiased sample estimator of the common kurtosis \[@Joanes1998-jo\]. Has better small-sample performance than `adf` and `normal` if the kurtosis is large and ![n](https://latex.codecogs.com/svg.latex?n "n") is small.            |
| `normal`     | Assumes normality of ![X](https://latex.codecogs.com/svg.latex?X "X") \[@Van_Zyl2000-si\]. This method is not recommended since it yields too short confidence intervals when the excess kurtosis of ![X](https://latex.codecogs.com/svg.latex?X "X") is larger than ![0](https://latex.codecogs.com/svg.latex?0 "0"). |

In addition, you may transform the intervals using one of four
transforms:

1.  The [Fisher
    transform](https://en.wikipedia.org/wiki/Fisher_transformation), or
    ![\alpha\mapsto \operatorname{artanh}(\alpha)](https://latex.codecogs.com/svg.latex?%5Calpha%5Cmapsto%20%5Coperatorname%7Bartanh%7D%28%5Calpha%29 "\alpha\mapsto \operatorname{artanh}(\alpha)").
    Famously used in inference for the correlation coefficient.
2.  The ![\log](https://latex.codecogs.com/svg.latex?%5Clog "\log")
    transform, where
    ![\alpha \mapsto \log(1-\alpha)](https://latex.codecogs.com/svg.latex?%5Calpha%20%5Cmapsto%20%5Clog%281-%5Calpha%29 "\alpha \mapsto \log(1-\alpha)").
    This is an asymptotic pivot under the elliptical model with parallel
    items.
3.  The identity transform. The default option.
4.  The
    [![\arcsin](https://latex.codecogs.com/svg.latex?%5Carcsin "\arcsin")
    transform](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions).
    This transform might fail when
    ![n](https://latex.codecogs.com/svg.latex?n "n") is small, as
    negative values for
    ![\hat{\alpha}](https://latex.codecogs.com/svg.latex?%5Chat%7B%5Calpha%7D "\hat{\alpha}")
    is possible, but
    ![\arcsin](https://latex.codecogs.com/svg.latex?%5Carcsin "\arcsin")
    do not accept them,

The option `bootstrap` does studentized bootstrapping \[@Efron1987-ov\]
with `n_reps` repetitions. If `bootstrap = FALSE`, an ordinary normal
approximation will be used. The studentized bootstrap intervals are is a
second-order correct, so its confidence intervals will be better than
the normal approximation when
![n](https://latex.codecogs.com/svg.latex?n "n") is sufficiently large.

Finally, the option `parallel = TRUE` can be used, which is suitable if
covariance matrix
![\operatorname{Cov}(X)](https://latex.codecogs.com/svg.latex?%5Coperatorname%7BCov%7D%28X%29 "\operatorname{Cov}(X)")
is compound symmetric. If the distribution is normal or
(pseudo-)elliptic, it can be used to simplify the expression for the
asymptotic variance of alpha and standardized alpha to

![\sigma^{2}=\frac{2k(1+\kappa/3)}{k-1}(1-\alpha)^{2},](https://latex.codecogs.com/svg.latex?%5Csigma%5E%7B2%7D%3D%5Cfrac%7B2k%281%2B%5Ckappa%2F3%29%7D%7Bk-1%7D%281-%5Calpha%29%5E%7B2%7D%2C "\sigma^{2}=\frac{2k(1+\kappa/3)}{k-1}(1-\alpha)^{2},")

where ![\kappa](https://latex.codecogs.com/svg.latex?%5Ckappa "\kappa")
is the common kurtosis parameter.

## How to Contribute or Get Help

If you encounter a bug, have a feature request or need some help, open a
[Github issue](https://github.com/JonasMoss/alphaci/issues). Create a
pull requests to contribute. This project follows a [Contributor Code of
Conduct](https://www.contributor-covenant.org/version/1/4/code-of-conduct.md).

## References
