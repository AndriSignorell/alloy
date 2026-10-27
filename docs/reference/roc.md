# ROC curve for a fitted model, a cross-validation or a predictor vector

Convenience wrapper around
[`roc`](https://rdrr.io/pkg/pROC/man/roc.html) that accepts a fitted
`"FitMod"` binary classification model, the result of
[`crossVal()`](https://andrisignorell.github.io/alloy/reference/crossVal.md)
for such a model, or a numeric predictor vector directly.

## Usage

``` r
roc(x, ref = NULL, ...)
```

## Arguments

- x:

  Either a fitted binary classification model of class `"FitMod"` (in
  which case predicted probabilities and the response are extracted
  automatically), an object of class `"CrossVal"` from
  [`crossVal()`](https://andrisignorell.github.io/alloy/reference/crossVal.md)
  (out-of-fold predictions), or a numeric vector of predicted
  probabilities / scores when `ref` is supplied.

- ref:

  Optional factor or binary vector of true class labels. If `NULL`
  (default), `x` must be a `"FitMod"` or `"CrossVal"` object and the
  response is taken from it.

- ...:

  Further arguments passed to
  [`roc`](https://rdrr.io/pkg/pROC/man/roc.html); they override the
  defaults set here, e.g. `direction`.

## Value

An object of class `"roc"` as returned by
[`roc`](https://rdrr.io/pkg/pROC/man/roc.html).

## Details

For a `"FitMod"` object the predicted probability of the second class
(the second column of `predict(x)`) is the predictor. The class order is
known then, so `levels` is set to the classes and `direction` to `"<"`
(higher predictions for the second class): pROC would otherwise choose
the direction from the data and turn a model worse than chance into one
better than chance.

For a `"CrossVal"` object the curve is drawn from the out-of-fold
predictions, each made by a model that had not seen the observation,
with the same `levels` and `direction`. With repeated cross-validation
each observation enters once, with the mean of its out-of-fold
predictions; the AUC of this curve can therefore differ slightly from
the pooled `auc` in the summary of
[`crossVal()`](https://andrisignorell.github.io/alloy/reference/crossVal.md),
which is computed per repetition and then averaged.

For a predictor vector with `ref`, pROC determines levels and direction
itself unless they are given in `...`. For models with non-standard
probability output, supply the predictor vector explicitly via `x` and
`ref`.

## See also

[`roc`](https://rdrr.io/pkg/pROC/man/roc.html),
[`rocTest()`](https://andrisignorell.github.io/alloy/reference/rocTest.md),
[`bestCut`](https://andrisignorell.github.io/alloy/reference/bestCut.md),
[`response`](https://andrisignorell.github.io/alloy/reference/response.md)

Other roc:
[`bestCut()`](https://andrisignorell.github.io/alloy/reference/bestCut.md),
[`lift()`](https://andrisignorell.github.io/alloy/reference/lift.md),
[`rocTest()`](https://andrisignorell.github.io/alloy/reference/rocTest.md)

## Examples

``` r
fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")
r <- roc(fitLogit)
#> Error in roc.default(fitLogit): No valid data provided.
plot(r)
#> Error: object 'r' not found

# Supply predictor and response directly
p <- predict(fitLogit)[, 2]
r2 <- roc(p, ref = Admit$admit)
#> Error in roc.default(p, ref = Admit$admit): No valid data provided.

# cross-validated ROC curve
set.seed(1)
rCV <- roc(crossVal(fitLogit, k = 10))
#> Error in roc.default(crossVal(fitLogit, k = 10)): No valid data provided.
rCV$auc
#> Error: object 'rCV' not found
```
