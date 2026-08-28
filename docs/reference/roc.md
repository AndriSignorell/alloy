# ROC curve for a fitted model or predictor vector

Convenience wrapper around
[`roc`](https://rdrr.io/pkg/pROC/man/roc.html) that accepts either a
fitted `"FitMod"` binary classification model or a numeric predictor
vector directly.

## Usage

``` r
roc(x, resp = NULL, ...)
```

## Arguments

- x:

  Either a fitted binary classification model of class `"FitMod"` (in
  which case predicted probabilities and the response are extracted
  automatically), or a numeric vector of predicted probabilities /
  scores when `resp` is supplied.

- resp:

  Optional factor or binary vector of true class labels. If `NULL`
  (default), `x` must be a `"FitMod"` object and the response is
  extracted via [`response`](response.md).

- ...:

  Further arguments passed to
  [`roc`](https://rdrr.io/pkg/pROC/man/roc.html).

## Value

An object of class `"roc"` as returned by
[`roc`](https://rdrr.io/pkg/pROC/man/roc.html).

## Details

When `x` is a `"FitMod"` object, the second column of
`predict(x, type = "prob")` is used as the predictor (i.e. the
probability of the second factor level). For models with non-standard
probability output, supply the predictor vector explicitly via `x` and
`resp`.

## See also

[`roc`](https://rdrr.io/pkg/pROC/man/roc.html), [`bestCut`](bestCut.md),
[`response`](response.md)

Other roc: [`bestCut()`](bestCut.md), [`lift()`](lift.md)

## Examples

``` r
fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
r <- roc(fitLogit)
#> Error in roc.default(fitLogit): No valid data provided.
plot(r)
#> Error: object 'r' not found

# Supply predictor and response directly
p <- predict(fitLogit)[, 2]
r2 <- roc(p, resp = Admit$admit)
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
```
