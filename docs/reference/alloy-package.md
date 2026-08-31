# alloy: A Unified Modeling Framework for Regression and Classification

`alloy` provides a single, consistent interface for fitting, displaying
and comparing a wide range of statistical and machine-learning models.
The modeler specifies *what* model they want (e.g. `"logit"`,
`"weibull"`, `"randomForest"`) rather than *how* to call the underlying
function – the package handles the rest.

## Main function

The entry point is
[`fitMod`](https://andrisignorell.github.io/alloy/reference/fitMod.md),
which accepts a standard R formula and a `fitfn` argument naming the
model type. When `fitfn` is omitted the appropriate method is chosen
automatically from the response variable type.

Supported model families:

- Continuous outcome:

  `lm`, `lmrob`, `gamma`, `tobit`

- Binary outcome:

  `logit`

- Count outcome:

  `poisson`, `quasipoisson`, `negbin`, `zeroinfl`

- Ordered / nominal outcome:

  `polr`, `multinom`

- Survival outcome:

  `coxph`, `weibull`, `exponential`, `lognormal`, `loglogistic`

- Mixed models:

  `lmMixed`, `logitMixed`, `poissonMixed`, `negbinMixed`, `gammaMixed`

- Machine learning:

  `randomForest`, `nnet`, `rpart`, `C5.0`, `svm`, `naiveBayes`, `lda`,
  `qda`, `glmnet`, `xgboost`

## Unified output

[`print.FitMod`](https://andrisignorell.github.io/alloy/reference/print.FitMod.md)
produces Stata-style output with confidence intervals, p-values, and
reference category headers, consistently across all model types. The
`output` argument controls the scale: `"coef"`, `"or"`, `"irr"`, `"hr"`,
`"tr"`, or `"genuine"` (original model output).

[`predict.FitMod`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md)
returns a numeric vector for regression and survival models, and a tidy
`data.frame` for classifiers (`output = "prob"`, `"class"`, or
`"both"`).

## Additional functions

- [`varImp`](https://andrisignorell.github.io/alloy/reference/varImp.md):

  Variable importance for ML models (Cleveland dot plot via
  [`plot.varImp`](https://andrisignorell.github.io/alloy/reference/plot.varImp.md)).

- [`tMod`](https://andrisignorell.github.io/alloy/reference/tMod.md):

  Side-by-side comparison of multiple models.

- [`pseudoR2`](https://andrisignorell.github.io/alloy/reference/pseudoR2.md):

  Pseudo-R\\^2\\ measures for GLMs.

- [`vif`](https://andrisignorell.github.io/alloy/reference/vif.md):

  Variance inflation factors (VIF / GVIF).

- [`coefCI`](https://andrisignorell.github.io/alloy/reference/coefCI.md),
  [`rSq`](https://andrisignorell.github.io/alloy/reference/rSq.md):

  Bootstrap CIs for coefficients and R\\^2\\.

- [`conf`](https://andrisignorell.github.io/DescToolsX/reference/conf.html):

  Confusion matrix and classification metrics.

- [`roc`](https://andrisignorell.github.io/alloy/reference/roc.md),
  [`bestCut`](https://andrisignorell.github.io/alloy/reference/bestCut.md):

  ROC analysis.

- [`refLevel`](https://andrisignorell.github.io/alloy/reference/refLevel.md):

  Reference levels of factor predictors.

- [`rules`](https://andrisignorell.github.io/alloy/reference/rules.md),
  [`node`](https://andrisignorell.github.io/alloy/reference/node.md),
  [`cParam`](https://andrisignorell.github.io/alloy/reference/cParam.md),
  [`bestTree`](https://andrisignorell.github.io/alloy/reference/bestTree.md),
  [`leafRates`](https://andrisignorell.github.io/alloy/reference/leafRates.md),
  [`splits`](https://andrisignorell.github.io/alloy/reference/splits.md):

  Utilities for `rpart` trees.

## Datasets

The package ships with several benchmark datasets used in the vignette:
[`Admit`](https://andrisignorell.github.io/alloy/reference/Admit.md),
[`Apt`](https://andrisignorell.github.io/alloy/reference/Apt.md),
[`BioChemists`](https://andrisignorell.github.io/alloy/reference/BioChemists.md),
[`Contraception`](https://andrisignorell.github.io/alloy/reference/Contraception.md),
[`Fish`](https://andrisignorell.github.io/alloy/reference/Fish.md),
[`IceCream`](https://andrisignorell.github.io/alloy/reference/IceCream.md),
[`Lahigh`](https://andrisignorell.github.io/alloy/reference/Lahigh.md),
[`Ologit`](https://andrisignorell.github.io/alloy/reference/Ologit.md),
[`Pima`](https://andrisignorell.github.io/alloy/reference/Pima.md),
[`Whas100`](https://andrisignorell.github.io/alloy/reference/Whas100.md).

## Design philosophy

Modelers think in models, not in function calls. `alloy` follows the
principle that `fitMod(y ~ x, data, fitfn = "logit")` is preferable to
`glm(y ~ x, data, family = "binomial")` – the intent is stated directly,
and the package handles package-specific quirks, defaults, and
post-processing transparently. Output follows Stata conventions where
applicable.

## References

UCLA Statistical Methods and Data Analytics:
<https://stats.oarc.ucla.edu>

## See also

[`fitMod`](https://andrisignorell.github.io/alloy/reference/fitMod.md),
[`print.FitMod`](https://andrisignorell.github.io/alloy/reference/print.FitMod.md),
[`predict.FitMod`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md)

## Author

**Maintainer**: Andri Signorell <andri@signorell.net>

Authors:

- Andri Signorell <andri@signorell.net>
