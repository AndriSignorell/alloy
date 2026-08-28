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
  seed = NULL,
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

- seed:

  Optional random seed.

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

Other regression.utils: [`pseudoR2()`](pseudoR2.md), [`rSq()`](rSq.md),
[`refLevel()`](refLevel.md), [`response()`](response.md),
[`varImp()`](varImp.md), [`vif()`](vif.md)

## Examples

``` r
fit <- lm(mpg ~ wt + hp, data = mtcars)
coefCI(fit)
#>                     est         lci         uci
#> (Intercept) 37.22727012 33.26612345 41.38766346
#> wt          -3.87783074 -5.42334250 -2.57094227
#> hp          -0.03177295 -0.04887914 -0.02005618
```
