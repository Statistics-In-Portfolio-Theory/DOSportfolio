
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DOSPorfolio

<!-- badges: start -->
<!-- badges: end -->

The goal of DOSPorfolio is to provide a simple interface for computing
portfolio weights according to the dynamic weighting scheme from *link
when published on arxiv?*.

## Installation

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Statistics-In-Portfolio-Theory/DOSportfolio")
```

## Example

This is a basic example which shows you how use the package:

``` r
library(DOSPorfolio)
n <- 200*2
p <- 80
c <- p/n

# first change point in the portfolio is at the 199th observation
break_points <- c(199)
# Simulate data
data <- matrix(rt(n*p, df=5), ncol=p, nrow=n)
target_w <- ones_vec(p)/p
S <- var(data)
S_inv <- solve(S)
# Use all data to estimate r0/R0, the relative loss to the GMV portfolio
r <- r0_strategy(S_inv, S, target_w, p/n)
# estimate the GMV portfolio sequentially using non-overlapping subsamples 
w_non_overlapping(data, break_points, target_w, r)
# do the same with overlapping subsamples
w_overlapping(data, break_points, target_w, r)
```

To read the documentation you can simply use `?w_overlapping`.

## TODO:

-   Create S3 class (and/or iterator) which enclose the
    overlapping/nonoverlapping portfolios.
-   Setup so that the portfolio estimators can work with the `tsibble`
    package?
-   Extend theory to mean-variance portfolio (…)
