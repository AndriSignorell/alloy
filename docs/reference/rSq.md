# R-squared of a Linear Model

Computes the (adjusted) R-squared of a linear model fitted via
[`lm`](https://rdrr.io/r/stats/lm.html), optionally together with a
bootstrap confidence interval based on a parallel RcppParallel
implementation.

## Usage

``` r
rSq(
  fit,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  adjusted = TRUE,
  R = 2000,
  seed = NULL,
  ...
)
```

## Arguments

- fit:

  an object of class `"lm"`.

- conf.level:

  confidence level of the interval. Set to `NA` (default) to return the
  point estimate only, without bootstrapping.

- sides:

  a character string naming the side on which the finite bound lies,
  must be one of `"two.sided"` (default), `"left"` or `"right"`.

- adjusted:

  logical; if `TRUE` (default) the adjusted R-squared is reported,
  otherwise the ordinary one.

- R:

  number of bootstrap replicates, defaults to 2000.

- seed:

  integer seed for the bootstrap. If `NULL` (default) the seed is drawn
  from R's random number stream, so that
  [`set.seed()`](https://rdrr.io/r/base/Random.html) governs the result.

- ...:

  further arguments, currently unused.

## Value

If `conf.level` is `NA`, a single numeric value. Otherwise a named
numeric vector with the elements

- est:

  the (adjusted) R-squared of the fitted model.

- lci:

  the lower confidence limit.

- uci:

  the upper confidence limit.

## Details

If `conf.level` is `NA` (the default), the coefficient is taken directly
from the fitted model and a single number is returned. Only when a
confidence level is supplied is the bootstrap started.

The bootstrap resamples observations (pairs bootstrap): for each
replicate the model is refitted on the resampled rows of the model
matrix and the R-squared is recomputed. The interval is formed from the
empirical quantiles of the resulting distribution. Degenerate bootstrap
samples (singular design matrices) are discarded.

The point estimate follows the definition used by
[`summary.lm`](https://rdrr.io/r/stats/summary.lm.html), which for
models without intercept is based on the uncentred sum of squares and
therefore not comparable to the value of a model with intercept. For
adjusted R-squared \$\$R^2\_{adj} = 1 - (1 - R^2)\frac{n - i}{n - p}\$\$
is used, where \\p\\ is the rank of the model and \\i\\ is 1 for a model
with intercept and 0 otherwise.

`sides` names the side on which the finite bound lies, so `"left"`
yields \\\[lci, \infty)\\ and `"right"` yields \\(-\infty, uci\]\\, with
the full \\\alpha\\ on that single side. This reverses the meaning the
argument has in DescTools, where it names the direction of the
alternative hypothesis. Since R-squared cannot exceed 1, the open side
of a `"left"` interval is reported as 1 rather than as `Inf`; for
`"right"` the open side is 0, or `-Inf` for the adjusted coefficient,
which is not bounded below.

The bootstrap branch supports neither weighted models nor models without
intercept, as the C++ routine centres unconditionally and ignores
weights. Both cases are rejected with an error; the point estimate
remains available via `conf.level = NA`.

## See also

[`lm`](https://rdrr.io/r/stats/lm.html),
[`summary.lm`](https://rdrr.io/r/stats/summary.lm.html)

Other regression.utils: [`coefCI()`](coefCI.md),
[`pseudoR2()`](pseudoR2.md), [`refLevel()`](refLevel.md),
[`response()`](response.md), [`varImp()`](varImp.md), [`vif()`](vif.md)

## Examples

``` r
fit <- lm(mpg ~ wt + hp, data = mtcars)

rSq(fit)
#> [1] 0.8148396
## [1] 0.8148

rSq(fit, adjusted = FALSE)
#> [1] 0.8267855
## [1] 0.8268

rSq(fit, conf.level = 0.95, seed = 123)
#>       est       lci       uci 
#> 0.8148396 0.7309312 0.9064123 

rSq(fit, conf.level = 0.95, sides = "left", R = 1000, seed = 123)
#>       est       lci       uci 
#> 0.8148396 0.7471338 1.0000000 
```
