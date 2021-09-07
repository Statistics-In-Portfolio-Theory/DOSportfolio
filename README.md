
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DOSPorfolio

<!-- badges: start -->
<!-- badges: end -->

The goal of DOSPorfolio is to provide a simple interface for computing
portfolio weights according to the dynamic weighting scheme from
(Bodnar, Parolya, and Thorsén 2021).

## Installation

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Statistics-In-Portfolio-Theory/DOSportfolio")
```

## Example

This is a very simple example which shows you how use the package:

``` r
library(DOSPortfolio)
n <- 25*2
p <- 15
# Simulate data
data <- sqrt(5/3) * matrix(rt(n*p, df=5), ncol=p, nrow=n)
# specify the allocation points. The DOSPortfolio class will validate what is 
# "ok" allocation periods.
reallocation_points <- c(25, 42)
(portfolios <- DOSPortfolio(data, reallocation_points))
```

The variable `portfolios` is a “DOSPortfolio” class whose documentation
can be viewed by `?DOSPortfolio`. The constructor `DOSPortfolio()` looks
for violations against the assumptions made in the reference. Here is an
example when things does not work

``` r
reallocation_points <- c(37, 42)
# observe that there is little data between the first and second allcoation 
# point. Its actually to little since p > n_2, e.g. 15 > 42 - 37. 
portfolios <- DOSPortfolio(data, reallocation_points)
# Error
```

## Possible further scope:

-   Create summary function so that one can construct summaries of the
    portfolios as done in (Bodnar, Parolya, and Thorsén 2021).
-   Include test on finite fourth moment, summary function to generate
    table from the article?
-   Setup so that the portfolio estimators can work with the `tsibble`
    package?
-   Extend theory to mean-variance portfolio and Shrinkage estimators.

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-BODNAR21dynshrink" class="csl-entry">

Bodnar, Taras, Nestor Parolya, and Erik Thorsén. 2021. “Dynamic
Shrinkage Estimation of the High-Dimensional Minimum-Variance
Portfolio.” <https://arxiv.org/abs/2106.02131/>.

</div>

</div>
