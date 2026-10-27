# Penalized Likelihood Inference for Firth Logistic Regression

Term-wise penalized likelihood ratio tests and profile penalized
likelihood confidence intervals for logistic models fitted with
`fitMod(..., engine = "logitFirth")`.

## Usage

``` r
# S3 method for class 'logitFirth'
drop1(object, scope, test = c("Chisq", "none"), ...)

# S3 method for class 'logitFirth'
confint(object, parm, level = 0.95, ...)
```

## Arguments

- object:

  a model fitted with `fitMod(..., engine = "logitFirth")`

- scope:

  the terms to be tested, by default all terms that can be dropped, see
  [`drop1()`](https://rdrr.io/r/stats/add1.html)

- test:

  `"Chisq"` (default) for the penalized likelihood ratio test, `"none"`
  for the statistics only

- ...:

  further arguments, ignored

- parm:

  the coefficients to compute intervals for, as names or indices.
  Default all.

- level:

  the confidence level. Default 0.95.

## Value

[`drop1()`](https://rdrr.io/r/stats/add1.html) returns an object of
class `"anova"` with the columns `Df`, `PLR` (the penalized likelihood
ratio statistic) and, for `test = "Chisq"`, `Pr(>Chi)`.
[`confint()`](https://rdrr.io/r/stats/confint.html) returns a matrix
with one row per coefficient and the lower and upper bound in its
columns.

## Details

Firth's method maximizes the penalized log-likelihood \\l^\*(\beta) =
l(\beta) + \frac{1}{2}\log\|I(\beta)\|\\. Inference follows the
penalized likelihood as well (Heinze and Schemper, 2002): the test
statistic of a term is \\2\\(l^\*(\hat\beta) - l^\*(\hat\beta_0))\\,
where \\\hat\beta_0\\ maximizes \\l^\*\\ with the coefficients of the
term fixed at 0, and is referred to a chi-square distribution with as
many degrees of freedom as the term has coefficients. A confidence
interval collects all values of a coefficient that such a test would not
reject.

The restricted fits keep the penalty of the full model. A refit without
the columns of the term, as in `brglm2:::drop1.brglmFit`, changes the
penalty and compares unpenalized deviances instead; the Wald intervals
of `brglm2:::confint.brglmFit` are unreliable near separation, the
situation Firth's method is used for.

The methods apply to the Jeffreys penalty with the logit link
(`type = "MPL_Jeffreys"`, the default of the engine, or the equivalent
`"AS_mean"` and `"AS_mixed"` in
[`brglm2::brglmFit`](https://rdrr.io/pkg/brglm2/man/brglmFit.html)). For
other types the methods of brglm2 are used.

A bound that cannot be found within a wide search range (the profile
does not reach the critical value) is returned as `-Inf` or `Inf` with a
warning.

## References

Firth, D. (1993) Bias reduction of maximum likelihood estimates.
*Biometrika* 80, 27-38.

Heinze, G. and Schemper, M. (2002) A solution to the problem of
separation in logistic regression. *Statistics in Medicine* 21,
2409-2419.

## See also

Other modelling:
[`classification-metrics`](https://andrisignorell.github.io/alloy/reference/classification-metrics.md),
[`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md),
[`plot.FitMod()`](https://andrisignorell.github.io/alloy/reference/plot.FitMod.md),
[`predict.FitMod()`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md),
[`predictors()`](https://andrisignorell.github.io/alloy/reference/predictors.md),
[`print.FitMod()`](https://andrisignorell.github.io/alloy/reference/print.FitMod.md),
[`regression-metrics`](https://andrisignorell.github.io/alloy/reference/regression-metrics.md)

## Examples

``` r
if (requireNamespace("brglm2", quietly = TRUE)) {
  fit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logitFirth")
  drop1(fit)
  confint(fit)
}
#>                     2.5 %       97.5 %
#> (Intercept) -6.1654650411 -1.733021721
#> gre          0.0001124749  0.004365933
#> gpa          0.1504196393  1.440833495
#> rank2       -1.2847644285 -0.053262453
#> rank3       -1.9975621774 -0.655065307
#> rank4       -2.3448074019 -0.725328933
```
