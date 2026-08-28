# Separation Plot

Draws one thin vertical line per observation, ordered by predicted
probability and coloured by the observed outcome. A model that separates
the classes shows the events collected on the right; one that does not
shows them evenly mixed. The whole sample fits in a single strip, which
makes it a compact companion to a numeric summary such as the c
statistic.

## Usage

``` r
plotSeparation(
  x,
  main = NULL,
  xlab = "observations ordered by predicted probability",
  ylab = "",
  col = .useTheme,
  box = .useTheme,
  line = TRUE,
  expected = TRUE,
  legend = TRUE,
  stamp = .useTheme,
  ...
)
```

## Arguments

- x:

  a fitted logistic model of class `"FitMod"` (fitted with
  `fitfn = "logit"`) or a binomial
  [`glm`](https://rdrr.io/r/stats/glm.html).

- main:

  main title. `NULL` (default) derives one from the model formula; `""`,
  `NA` or `FALSE` suppress it.

- xlab, ylab:

  axis labels.

- col:

  colours for non-events and events, in that order. `.useTheme`
  (default) resolves against the active theme.

- box:

  plot box, following the flexible
  `TRUE`/`FALSE`/`NA`/[`list()`](https://rdrr.io/r/base/list.html)
  pattern.

- line:

  the predicted probabilities drawn across the strip. `TRUE` (default),
  `FALSE`, or a named list passed to `lines`.

- expected:

  marker for the expected number of events. `TRUE` (default), `FALSE`,
  or a named list passed to `points`.

- legend:

  legend for the two outcome colours. `TRUE` (default), `FALSE`, or a
  named list passed to `legend`.

- stamp:

  corner stamp, passed to the graphics framework.

- ...:

  further graphical parameters passed to
  [`par()`](https://rdrr.io/r/graphics/par.html) via the internal
  framework.

## Value

invisibly, a data frame with the columns `p` and `y`, sorted by `p`.

## Details

The plot answers a different question from
[`plotCalibration`](https://andrisignorell.github.io/alloy/reference/plotCalibration.md):
separation is about the *ordering* of the predictions, calibration about
their *level*. A model can rank perfectly and still predict risks twice
too high, and the ROC curve - being invariant to any monotone
transformation of the predictions - cannot see the difference either.
Read the two plots together.

The marker on the axis sits at the expected number of events, \\\sum
\hat p_i\\, counted from the right. If it lands where the observed
events start, the model gets the overall event rate right.

## References

Greenhill, B., Ward, M. D. and Sacks, A. (2011) The separation plot: a
new visual method for evaluating the fit of binary models. *American
Journal of Political Science*, **55**(4), 991–1002.

## See also

[model-diagnostics-overview](https://andrisignorell.github.io/alloy/reference/model-diagnostics-overview.md)
for an overview of the diagnostics for logistic models in alloy.

## Examples

``` r
fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")

plotSeparation(fitLogit)

```
