# Bootstrap Confidence Intervals for Linear Model Coefficients

Computes bootstrap confidence intervals for regression coefficients from
a linear model using a fast parallel implementation.

## Usage

``` r
coefCI(
  fit,
  conf.level = 0.95,
  sides = c("two.sided", "left", "right"),
  R = 2000,
  ...
)
```

## Arguments

- fit:

  An object of class `"lm"`.

- conf.level:

  Confidence level. Default is `0.95`.

- sides:

  Type of interval: `"two.sided"`, `"left"`, or `"right"`.

- R:

  Number of bootstrap samples.

- ...:

  Further arguments (unused).

## Value

A matrix with rows corresponding to coefficients and columns: `est`,
`lci`, `uci`.

## Details

Uses a nonparametric bootstrap (resampling observations). For each
sample, the linear model is refitted and coefficients are stored.

Confidence intervals are based on empirical quantiles.

## See also

Other regression.utils:
[`pseudoRSq()`](https://andrisignorell.github.io/alloy/reference/pseudoRSq.md),
[`rSq()`](https://andrisignorell.github.io/alloy/reference/rSq.md),
[`refLevel()`](https://andrisignorell.github.io/alloy/reference/refLevel.md),
[`response()`](https://andrisignorell.github.io/alloy/reference/response.md),
[`varImp()`](https://andrisignorell.github.io/alloy/reference/varImp.md),
[`vif()`](https://andrisignorell.github.io/alloy/reference/vif.md)

## Examples

``` r
fit <- lm(mpg ~ wt + hp, data = mtcars)
coefCI(fit)
#>                     est         lci         uci
#> (Intercept) 37.22727012 33.32622297 41.48213555
#> wt          -3.87783074 -5.39708085 -2.54888574
#> hp          -0.03177295 -0.04914129 -0.01999668
```
