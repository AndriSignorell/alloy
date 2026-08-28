# Calibration Curve

Plots observed event rates against predicted probabilities, with a
smoother and its confidence band, the diagonal of perfect calibration,
and the calibration intercept and slope. Where a goodness-of-fit test
returns a single p-value, this shows *where* and *by how much* the
predictions are off.

## Usage

``` r
plotCalibration(
  x,
  newdata = NULL,
  main = NULL,
  xlab = "predicted probability",
  ylab = "observed proportion",
  xlim = NULL,
  ylim = NULL,
  nBins = NULL,
  conf.level = 0.95,
  col = .useTheme,
  bg = .useTheme,
  pch = .useTheme,
  cex = .useTheme,
  grid = .useTheme,
  box = .useTheme,
  smooth = TRUE,
  rug = TRUE,
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

- newdata:

  optional data frame for evaluating calibration out of sample. `NULL`
  (default) uses the training data.

- main:

  main title. `NULL` (default) derives one from the model formula; `""`,
  `NA` or `FALSE` suppress it.

- xlab, ylab:

  axis labels.

- xlim, ylim:

  axis limits. `NULL` (default) uses \\\[0, 1\]\\ clipped to the range
  of the predictions.

- nBins:

  number of bins for the observed proportions drawn on top of the
  smoother. `NULL` (default) uses 10; `0` or `FALSE` suppresses them.

- conf.level:

  level of the confidence band. Default `0.95`.

- col, bg, pch, cex:

  colour, fill, symbol and size of the binned points. `.useTheme`
  (default) resolves against the active theme.

- grid, box:

  background grid and plot box, following the flexible
  `TRUE`/`FALSE`/`NA`/[`list()`](https://rdrr.io/r/base/list.html)
  pattern.

- smooth:

  the loess smoother and its band. `TRUE` (default) draws it with
  defaults, `FALSE`/`NA` suppresses it, a named list is passed to
  `lines.loess` (e.g. `list(bandArgs = FALSE)`).

- rug:

  marks for the individual predictions, events above and non-events
  below the panel. `TRUE` (default), `FALSE`, or a named list passed to
  `rug`.

- legend:

  annotation with intercept, slope and Brier score. `TRUE` (default),
  `FALSE`, or a named list passed to `boxedText`. Its `x` element may be
  one of the named positions of
  [`abcCoords`](https://andrisignorell.github.io/pharos/reference/abcCoords.html)
  (`"topleft"` by default, `"bottomright"` where the curve runs through
  the upper corner).

- stamp:

  corner stamp, passed to the graphics framework.

- ...:

  further graphical parameters passed to
  [`par()`](https://rdrr.io/r/graphics/par.html) via the internal
  framework.

## Value

invisibly, a list with the components `intercept`, `slope`, `brier`
(scaled and unscaled), `bins` (the binned observed proportions) and
`inSample`.

## Details

Two numbers summarise the curve, both from a logistic regression of the
response on the linear predictor \\\hat\eta = \mathrm{logit}(\hat p)\\:

- intercept:

  \\\alpha\\ from \\y \sim \mathrm{offset} (\hat\eta)\\ -
  calibration-in-the-large. Zero when the predicted risks are right on
  average; negative when the model predicts too much risk overall.

- slope:

  \\\beta\\ from \\y \sim \hat\eta\\. One when the predictions are
  neither too extreme nor too flat; below one is the signature of
  overfitting, the usual finding when a model is evaluated on the data
  it was fitted to.

On the development sample both are one and zero *by construction* for a
logistic model fitted by maximum likelihood - the curve then only shows
departures from linearity in the logit, not overfitting. The numbers
earn their meaning on new data (`newdata`) or a resampled estimate;
`plotCalibration` reports them either way, and says which case it is in
the annotation.

The smoother is a loess of the binary response on the fitted
probability. Its band is pointwise and rests on constant variance, which
a binary response does not have; read it as an indication of where the
data are thin, not as a simultaneous confidence region.

## References

Steyerberg, E. W. et al. (2010) Assessing the performance of prediction
models: a framework for traditional and novel measures. *Epidemiology*,
**21**(1), 128–138.

Van Calster, B. et al. (2019) Calibration: the Achilles heel of
predictive analytics. *BMC Medicine*, **17**, 230.

## See also

[model-diagnostics-overview](https://andrisignorell.github.io/alloy/reference/model-diagnostics-overview.md)
for an overview of the diagnostics for logistic models in alloy.

## Examples

``` r
fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")

plotCalibration(fitLogit)


# out of sample, where intercept and slope carry information
idx <- sample(nrow(Admit), nrow(Admit) * 0.7)
fitTrain <- fitMod(admit ~ gre + gpa + rank, Admit[idx, ], fitfn = "logit")
plotCalibration(fitTrain, newdata = Admit[-idx, ])

```
