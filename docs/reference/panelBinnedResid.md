# Facet Panel for Binned Residuals

A panel function for
[`plotFacet`](https://andrisignorell.github.io/pharos/reference/plotFacet.html)
that draws one binned residual panel: the band, the zero line and the
bin means. Pass the output of
[`binnedResid`](https://andrisignorell.github.io/alloy/reference/binnedResid.md)
as the samples.

## Usage

``` r
panelBinnedResid(x, y, lci, uci, col, pch = 16, bandCol = NULL, ...)
```

## Arguments

- x, y:

  the bin midpoints and mean residuals, supplied by `plotFacet` from the
  sample's `x` and `y` components.

- lci, uci:

  the band, supplied from the sample's components of the same name.

- col, pch:

  colour and symbol of the bin means, supplied by `plotFacet`.

- bandCol:

  fill colour of the band. `NULL` (default) takes it from the active
  theme.

- ...:

  further arguments passed to
  [`points`](https://rdrr.io/r/graphics/points.html).

## Value

called for its side effect; returns `NULL` invisibly.

## See also

[model-diagnostics-overview](https://andrisignorell.github.io/alloy/reference/model-diagnostics-overview.md)
for an overview of the diagnostics for logistic models in alloy.

## Examples

``` r
fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
bins <- binnedResid(fitLogit, var = predictors(fitLogit))
#> Warning: only 16 distinct bins could be formed instead of the requested 20
#> Warning: only 19 distinct bins could be formed instead of the requested 20

pharos::plotFacet(bins, dim = c(1, 3), panelFun = panelBinnedResid,
                  xlim = lapply(bins, function(b) range(b$x)),
                  ylim = range(unlist(lapply(bins, function(b) c(b$lci, b$uci)))),
                  stripLabels = vars, ylab = "mean residual")
#> Error: object 'vars' not found
```
