# Best cut-point of an ROC curve

A thin convenience wrapper around
[`coords`](https://rdrr.io/pkg/pROC/man/coords.html) that returns the
optimal cut-point of an ROC curve together with the corresponding
sensitivity and specificity.

## Usage

``` r
bestCut(x, method = c("youden", "closest.topleft"))
```

## Arguments

- x:

  An object of class `"roc"` as returned by
  [`roc`](https://rdrr.io/pkg/pROC/man/roc.html).

- method:

  Character string specifying the optimality criterion. `"youden"`
  (default) maximises the Youden index \\J = \text{sensitivity} +
  \text{specificity} - 1\\; `"closest.topleft"` minimises the Euclidean
  distance to the top-left corner \\(0, 1)\\ of the ROC space.

## Value

A named numeric vector with elements `threshold`, `specificity`, and
`sensitivity` at the optimal cut-point. When ties exist,
[`coords`](https://rdrr.io/pkg/pROC/man/coords.html) may return multiple
columns; the result is then a matrix (one column per tied optimum).

## See also

[`coords`](https://rdrr.io/pkg/pROC/man/coords.html),
[`roc`](https://rdrr.io/pkg/pROC/man/roc.html)

Other roc:
[`lift()`](https://andrisignorell.github.io/alloy/reference/lift.md),
[`roc()`](https://andrisignorell.github.io/alloy/reference/roc.md)

## Examples

``` r
library(pROC)
#> Type 'citation("pROC")' for a citation.
#> 
#> Attaching package: ‘pROC’
#> The following object is masked from ‘package:alloy’:
#> 
#>     roc
#> The following objects are masked from ‘package:stats’:
#> 
#>     cov, smooth, var
data(aSAH)
r <- roc(aSAH$outcome, aSAH$s100b)
#> Setting levels: control = Good, case = Poor
#> Setting direction: controls < cases

bestCut(r)
#> Warning: 'transpose=TRUE' is deprecated. Only 'transpose=FALSE' will be allowed in a future version.
#>   threshold specificity sensitivity 
#>   0.2050000   0.8055556   0.6341463 
bestCut(r, method = "closest.topleft")
#> Warning: 'transpose=TRUE' is deprecated. Only 'transpose=FALSE' will be allowed in a future version.
#>   threshold specificity sensitivity 
#>   0.2050000   0.8055556   0.6341463 
```
