# Confidence Interval for the Difference of Two Regression Coefficients

Computes the Wald confidence interval for the difference \\b_2 - b_1\\
of two coefficients of a fitted linear model, using \\Var(b_2 - b_1) =
Var(b_1) + Var(b_2) - 2\\Cov(b_1, b_2)\\ and the t-distribution with the
model's residual degrees of freedom.

## Usage

``` r
coeffDiffCI(
  x,
  coeff,
  conf.level = 0.95,
  sides = c("two.sided", "left", "right"),
  vcov. = vcov
)
```

## Arguments

- x:

  a fitted model object of class `"lm"`.

- coeff:

  two coefficients, given as names or as (integer) positions in
  `coef(x)`. The difference is taken as second minus first,
  `coeff[2] - coeff[1]`.

- conf.level:

  confidence level of the interval, default `0.95`.

- sides:

  a character string specifying the side of the confidence interval, one
  of `"two.sided"` (default), `"left"`, or `"right"`.

- vcov.:

  covariance matrix of the coefficient estimates, or a function to
  compute it from `x` (default
  [`vcov`](https://rdrr.io/r/stats/vcov.html)). Supply e.g.
  [`sandwich::vcovHC`](https://zeileis.codeberg.page/sandwich/reference/vcovHC.html)
  for heteroskedasticity-robust intervals.

## Value

a named numeric vector with components `diff`, `lci`, and `uci`.

## Examples

``` r
fit <- lm(mpg ~ cyl + disp + hp, data = mtcars)
coeffDiffCI(fit, c("cyl", "hp"))
#>       diff        lci        uci 
#>  1.2127406 -0.4341433  2.8596245 
```
