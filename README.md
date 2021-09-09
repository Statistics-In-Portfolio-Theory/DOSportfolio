
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
#> $weights
#>            [,1]       [,2]        [,3]      [,4]       [,5]       [,6]
#> [1,] 0.08697641 0.07240871 0.018210965 0.0800573 0.06954698 0.07120553
#> [2,] 0.09185841 0.07378896 0.006563325 0.0832761 0.07023934 0.07229657
#>            [,7]         [,8]       [,9]      [,10]      [,11]      [,12]
#> [1,] 0.05088562  0.006348246 0.06908115 0.08272137 0.08722852 0.06232573
#> [2,] 0.04709222 -0.008150919 0.06966153 0.08658055 0.09217112 0.06128227
#>           [,13]      [,14]      [,15]
#> [1,] 0.08156509 0.08208267 0.07935573
#> [2,] 0.08514633 0.08578832 0.08240588
#> 
#> $shrinkage_type
#> [1] "non-overlapping"
#> 
#> attr(,"class")
#> [1] "DOSPortfolio"
```

The variable `portfolios` is a “DOSPortfolio” class whose documentation
can be viewed by `?DOSPortfolio`. The constructor `DOSPortfolio()` looks
for violations against the assumptions made in the reference. Here is an
example when things does not work

``` r
reallocation_points <- c(37, 42)
# observe that there is little data between the first and second allcoation 
# point. Its actually to little since p > n_2, e.g. 15 > 42 - 37. 

# This will cause an error
DOSPortfolio(data, reallocation_points)
#> Error: Non-overlapping estimator can not handle concentration ratios above one.
#>             Consider excluding one (or more) break point(s) or provide more data.
```

However, by using the the argument `shrinkage_type="overlapping"` we can
make it work!

``` r
DOSPortfolio(data, reallocation_points, shrinkage_type = "overlapping")
#> $weights
#>            [,1]       [,2]        [,3]       [,4]       [,5]       [,6]
#> [1,] 0.08409633 0.07159444 0.025082350 0.07815841 0.06913853 0.07056188
#> [2,] 0.09300762 0.07411387 0.003821481 0.08403381 0.07040232 0.07255340
#>            [,7]        [,8]       [,9]      [,10]      [,11]      [,12]
#> [1,] 0.05312349  0.01490185 0.06873876 0.08044469 0.08431269 0.06294131
#> [2,] 0.04619926 -0.01156401 0.06979815 0.08748900 0.09333460 0.06103664
#>           [,13]      [,14]      [,15]
#> [1,] 0.07945238 0.07989656 0.07755632
#> [2,] 0.08598935 0.08666063 0.08312389
#> 
#> $shrinkage_type
#> [1] "overlapping"
#> 
#> attr(,"class")
#> [1] "DOSPortfolio"
```

If you want to learn to more why this is then you can have a look at the
*“introduction”* vignette or read the paper!

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
Portfolio.” *arXiv Preprint arXiv:2106.02131*.
<https://arxiv.org/abs/2106.02131>.

</div>

</div>
