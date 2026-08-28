# Confidence intervals for ROC curve coordinates

Computes confidence intervals for sensitivity, specificity, or other
coordinates of an ROC curve via
[`ci.coords`](https://rdrr.io/pkg/pROC/man/ci.coords.html).

## Usage

``` r
# S3 method for class 'roc'
confint(object, parm, level = 0.95, x = "best", ...)
```

## Arguments

- object:

  An object of class `"roc"` as returned by
  [`roc`](https://rdrr.io/pkg/pROC/man/roc.html) or
  [`roc`](https://andrisignorell.github.io/alloy/reference/roc.md).

- parm:

  Currently unused; present for compatibility with the generic
  [`confint`](https://rdrr.io/r/stats/confint.html).

- level:

  Confidence level. Default is `0.95`.

- x:

  Coordinate at which to evaluate the confidence interval. Either a
  numeric value (e.g. a threshold or specificity value) or one of the
  special strings accepted by
  [`ci.coords`](https://rdrr.io/pkg/pROC/man/ci.coords.html): `"best"`
  (default), `"all"`, or `"local maximas"`.

- ...:

  Further arguments passed to
  [`ci.coords`](https://rdrr.io/pkg/pROC/man/ci.coords.html).

## Value

A `"ci.coords"` object as returned by
[`ci.coords`](https://rdrr.io/pkg/pROC/man/ci.coords.html).

## See also

[`ci.coords`](https://rdrr.io/pkg/pROC/man/ci.coords.html),
[`roc`](https://andrisignorell.github.io/alloy/reference/roc.md)

## Examples

``` r
fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
r <- roc(fitLogit)
#> Error in roc.default(fitLogit): No valid data provided.
confint(r)                    # CI at best cut-point
#> Error: object 'r' not found
confint(r, x = 0.5)          # CI at specificity = 0.5
#> Error: object 'r' not found
```
