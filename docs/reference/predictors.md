# Predictors of a Fitted Model

Returns the names of the terms on the right hand side of a fitted model.
The information is in `attr(terms(x), "term.labels")`; this is the same
thing under a name one can guess.

## Usage

``` r
predictors(x, numeric = FALSE)
```

## Arguments

- x:

  a fitted model - anything with a
  [`terms`](https://rdrr.io/r/stats/terms.html) method, including
  `"FitMod"`, `"glm"` and `"lm"`.

- numeric:

  logical; keep only terms that are numeric in the model frame. Default
  `FALSE`.

## Value

a character vector of term labels, empty for an intercept-only model.

## Details

What comes back are *terms*, not variables: a transformed term appears
as it was written (`"log(insulin)"`), an interaction as `"a:b"`, and a
variable used twice appears once per term. That is the right granularity
for looping over diagnostics, because a diagnostic is per term. For the
underlying variable names - what a data frame would have to contain -
use `all.vars(formula(x))[-1]`.

`numeric = TRUE` keeps the terms that are numeric in the model frame.
Diagnostics of functional form only apply to those: linearity in the
logit is not a question one can ask of a factor.

## See also

[`binnedResid`](https://andrisignorell.github.io/alloy/reference/binnedResid.md)
and
[`plotPartialResid`](https://andrisignorell.github.io/alloy/reference/plotPartialResid.md),
the two diagnostics that are computed per term;
[model-diagnostics-overview](https://andrisignorell.github.io/alloy/reference/model-diagnostics-overview.md)
for an overview.

Other modelling:
[`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md),
[`plot.FitMod()`](https://andrisignorell.github.io/alloy/reference/plot.FitMod.md),
[`predict.FitMod()`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md),
[`print.FitMod()`](https://andrisignorell.github.io/alloy/reference/print.FitMod.md)

## Examples

``` r
fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")

predictors(fitLogit)
#> [1] "gre"  "gpa"  "rank"
predictors(fitLogit, numeric = TRUE)     # rank drops out
#> [1] "gre" "gpa"

# the loop this exists for
bins <- binnedResid(fitLogit, var = predictors(fitLogit))
#> Warning: only 16 distinct bins could be formed instead of the requested 20
#> Warning: only 19 distinct bins could be formed instead of the requested 20
plotFacet(bins, dim = c(1, 3), panelFun = panelBinnedResid,
          xlim = "free", ylab = "mean residual")
#> Error in plotFacet(bins, dim = c(1, 3), panelFun = panelBinnedResid, xlim = "free",     ylab = "mean residual"): could not find function "plotFacet"
```
