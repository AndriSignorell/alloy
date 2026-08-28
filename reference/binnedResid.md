# Binned Residuals

Computes the mean response residual within bins of the fitted
probabilities or of one or more predictors, together with the pointwise
band implied by the model. This is the computation behind
[`plotBinnedResid`](https://andrisignorell.github.io/alloy/reference/plotBinnedResid.md),
separated out so that the numbers can be had without a device and
several predictors can be prepared in one call.

## Usage

``` r
binnedResid(
  x,
  var = NULL,
  nBins = NULL,
  conf.level = 0.95,
  method = c("model", "empirical")
)
```

## Arguments

- x:

  a fitted logistic model of class `"FitMod"` (fitted with
  `fitfn = "logit"`) or a binomial
  [`glm`](https://rdrr.io/r/stats/glm.html).

- var:

  the binning variable. `NULL` (default) bins by the fitted
  probabilities; a character *vector* names variables in the model frame
  (or, for a transformed term, in the data the model was fitted from); a
  numeric or factor vector of length \\n\\ is used directly.

- nBins:

  number of bins. `NULL` (default) uses \\\lfloor\sqrt{n}\rfloor\\ for
  \\n \geq 100\\, 10 for \\10 \< n \< 100\\ and \\n/2\\ below that.
  Ignored for a factor.

- conf.level:

  level of the pointwise band. Default `0.95`.

- method:

  standard error of the band, `"model"` (default) or `"empirical"`. See
  Details.

## Value

for a single binning variable, a data frame with one row per bin and the
columns

- `bin`:

  the bin label - the interval, or the factor level

- `x`:

  mean of the binning variable in the bin

- `y`:

  mean residual in the bin. Named `y` rather than `resid` so the result
  can go straight into
  [`plotFacet`](https://andrisignorell.github.io/pharos/reference/plotFacet.html),
  whose samples are `x`/`y`

- `n`:

  number of observations in the bin

- `se`:

  standard error of the mean residual

- `lci`, `uci`:

  the band around zero

with attributes `label` (the name of the binning variable),
`categorical`, `method`, `conf.level` and `outside` (the number of bins
outside the band).

For several binning variables, a named list of such data frames.

## Details

Observations are grouped into bins of roughly equal size (quantiles of
the binning variable) and each bin contributes one row: the mean of the
binning variable, the mean residual \\y - \hat p\\, the bin size and the
band. Under a correct model the mean residual in a bin is centred at
zero with standard error \\\sqrt{\overline{p(1-p)/m}/n_b}\\.

A factor is grouped by its levels rather than by quantiles, giving one
row per level. A count variable with few distinct values is the awkward
case in between: quantile breaks collapse, fewer bins than requested
come back, and the warning saying so is worth heeding - passing the
variable as a factor is usually the more honest treatment.

`method` controls the band only:

- `"model"`:

  binomial standard error implied by the fitted probabilities. Exact
  under the model and usable in small bins.

- `"empirical"`:

  standard error from the spread within the bin. Assumes nothing about
  the model, but needs bins large enough to estimate a variance.

## References

Gelman, A. and Hill, J. (2007) *Data Analysis Using Regression and
Multilevel/Hierarchical Models*. Cambridge University Press, ch. 5.

## See also

[model-diagnostics-overview](https://andrisignorell.github.io/alloy/reference/model-diagnostics-overview.md)
for an overview of the diagnostics for logistic models in alloy.

## Examples

``` r
fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")

binnedResid(fitLogit, var = "gre")
#> Warning: only 16 distinct bins could be formed instead of the requested 20
#>          bin        x            y  n         se        lci       uci
#> 1  [220,399] 348.0000 -0.046545606 20 0.07746230 -0.1518233 0.1518233
#> 2  (399,440] 419.2857 -0.057590515 28 0.07384978 -0.1447429 0.1447429
#> 3  (440,460] 460.0000  0.045280982 14 0.11114160 -0.2178335 0.2178335
#> 4  (460,500] 491.3514 -0.004344753 37 0.06638298 -0.1301082 0.1301082
#> 5  (500,520] 520.0000  0.160531706 24 0.08463872 -0.1658889 0.1658889
#> 6  (520,540] 540.0000  0.021967880 27 0.08578528 -0.1681361 0.1681361
#> 7  (540,560] 560.0000 -0.013352211 24 0.08721798 -0.1709441 0.1709441
#> 8  (560,580] 580.0000 -0.118272699 29 0.08394377 -0.1645268 0.1645268
#> 9  (580,600] 600.0000  0.063386623 23 0.09408003 -0.1843935 0.1843935
#> 10 (600,620] 620.0000  0.042721711 30 0.08459114 -0.1657956 0.1657956
#> 11 (620,640] 640.0000 -0.104709448 21 0.10082925 -0.1976217 0.1976217
#> 12 (640,660] 660.0000  0.134927965 24 0.09480058 -0.1858057 0.1858057
#> 13 (660,680] 680.0000  0.096773987 20 0.10353499 -0.2029248 0.2029248
#> 14 (680,700] 700.0000 -0.221289991 22 0.10157408 -0.1990815 0.1990815
#> 15 (700,740] 730.0000 -0.065805220 22 0.10017477 -0.1963389 0.1963389
#> 16 (740,800] 791.4286  0.056469566 35 0.08068543 -0.1581405 0.1581405

# all predictors at once, ranked by how badly they fit
bins <- binnedResid(fitLogit, var = predictors(fitLogit))
#> Warning: only 16 distinct bins could be formed instead of the requested 20
#> Warning: only 19 distinct bins could be formed instead of the requested 20
sort(sapply(bins, attr, "outside"), decreasing = TRUE)
#>  gre  gpa rank 
#>    1    1    0 
```
