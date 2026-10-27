# Test for the Difference of Two ROC Curves

Compares the areas under two ROC curves (or their partial areas,
sensitivities or specificities), with
[`roc.test`](https://rdrr.io/pkg/pROC/man/roc.test.html). The curves can
be given as `"roc"` objects or as anything
[`roc()`](https://andrisignorell.github.io/alloy/reference/roc.md)
accepts without a reference: a fitted binary `"FitMod"` model or a
cross-validation from
[`crossVal()`](https://andrisignorell.github.io/alloy/reference/crossVal.md).

## Usage

``` r
rocTest(roc1, roc2, ...)
```

## Arguments

- roc1, roc2:

  the two ROC curves: objects of class `"roc"` (from
  [`roc()`](https://andrisignorell.github.io/alloy/reference/roc.md) or
  [`roc`](https://rdrr.io/pkg/pROC/man/roc.html)), binary `"FitMod"`
  models or `"CrossVal"` objects, which are passed to
  [`roc()`](https://andrisignorell.github.io/alloy/reference/roc.md)
  first.

- ...:

  further arguments passed to
  [`roc.test`](https://rdrr.io/pkg/pROC/man/roc.test.html), e.g.
  `method` (`"delong"`, `"bootstrap"`, `"venkatraman"`, ...),
  `alternative` and `paired`.

## Value

An object of class `"htest"` as returned by
[`roc.test`](https://rdrr.io/pkg/pROC/man/roc.test.html), with
`statistic`, `p.value`, `estimate` and further components.

## Details

Whether the curves are paired is decided by pROC from the data: paired
if both rest on the same observations with the same response, as for two
models fitted to, or cross-validated on, the same sample; unpaired
otherwise, e.g. for a development and an external validation sample. The
default DeLong test is available in both cases; set `paired` explicitly
where the automatic decision is not wanted.

Two cross-validated curves are paired only if they cover the same
observations; a fold that failed in one of them breaks the pairing.

The name follows the naming rules of the package suite; the function is
a thin wrapper, all statistics come from pROC.

## References

DeLong, E. R., DeLong, D. M. and Clarke-Pearson, D. L. (1988) Comparing
the areas under two or more correlated receiver operating characteristic
curves: a nonparametric approach. *Biometrics*, **44**(3), 837–845.

Robin, X. et al. (2011) pROC: an open-source package for R and S+ to
analyze and compare ROC curves. *BMC Bioinformatics*, **12**, 77.

## See also

[`roc()`](https://andrisignorell.github.io/alloy/reference/roc.md),
[`roc.test`](https://rdrr.io/pkg/pROC/man/roc.test.html),
[`crossVal()`](https://andrisignorell.github.io/alloy/reference/crossVal.md)

Other roc:
[`bestCut()`](https://andrisignorell.github.io/alloy/reference/bestCut.md),
[`lift()`](https://andrisignorell.github.io/alloy/reference/lift.md),
[`roc()`](https://andrisignorell.github.io/alloy/reference/roc.md)

## Examples

``` r
idx <- seq(1, nrow(Admit), by = 2)
dev <- Admit[idx, ]
ext <- Admit[-idx, ]

fit <- fitMod(admit ~ gre + gpa + rank, dev, engine = "logit")

# development against external validation: unpaired DeLong test
rDev <- roc(fit)
#> Error in roc.default(fit): No valid data provided.
rExt <- roc(predict(fit, newdata = ext)[, 2], ref = ext$admit)
#> Error in roc.default(predict(fit, newdata = ext)[, 2], ref = ext$admit): No valid data provided.
rocTest(rDev, rExt)
#> Error: object 'rDev' not found

# two models, cross-validated on the same sample: paired DeLong test
set.seed(1)
cvFull <- crossVal(fitMod(admit ~ gre + gpa + rank, Admit,
                          engine = "logit"))
set.seed(1)
cvSmall <- crossVal(fitMod(admit ~ gpa, Admit, engine = "logit"))
rocTest(cvFull, cvSmall)
#> 
#>  DeLong's test for two correlated ROC curves
#> 
#> data:  cvFull and cvSmall
#> Z = 1.8766, p-value = 0.06057
#> alternative hypothesis: true difference in AUC is not equal to 0
#> 95 percent confidence interval:
#>  -0.002459669  0.113243898
#> sample estimates:
#> AUC of roc1 AUC of roc2 
#>   0.6663350   0.6109429 
#> 
```
