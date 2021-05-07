
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
library(DOSPorfolio)
n <- 25*2
p <- 15
# Simulate data
data <- matrix(rt(n*p, df=5), ncol=p, nrow=n)
change_points <- c(25, 42)
# use the first subsample to estimate the relative loss
(portfolios <- DOSPortfolio(data, change_points))
```

To read the documentation you can simply use `?DOSPortfolio`.

## TODO:

-   Finish tests.
-   Include test on finite fourth moment, summary function to generate
    table from the article?
-   Setup so that the portfolio estimators can work with the `tsibble`
    package?
-   Extend theory to mean-variance portfolio (…)

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-BODNAR21dynshrink" class="csl-entry">

Bodnar, Taras, Nestor Parolya, and Erik Thorsén. 2021. “Dynamic
Shrinkage Estimation of the High-Dimensional Minimum-Variance
Portfolio.” *Test Journal* test vol.: x–1.

</div>

</div>
