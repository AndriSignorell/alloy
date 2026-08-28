# alloy: A Unified Modeling Framework for Regression and Classification

`alloy` provides a single, consistent interface for fitting, displaying
and comparing a wide range of statistical and machine-learning models.
The modeler specifies *what* model they want (e.g. `"logit"`,
`"weibull"`, `"randomForest"`) rather than *how* to call the underlying
function – the package handles the rest.

## Main function

The entry point is [`fitMod`](fitMod.md), which accepts a standard R
formula and a `fitfn` argument naming the model type. When `fitfn` is
omitted the appropriate method is chosen automatically from the response
variable type.

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

[`print.FitMod`](print.FitMod.md) produces Stata-style output with
confidence intervals, p-values, and reference category headers,
consistently across all model types. The `output` argument controls the
scale: `"coef"`, `"or"`, `"irr"`, `"hr"`, `"tr"`, or `"genuine"`
(original model output).

[`predict.FitMod`](predict.FitMod.md) returns a numeric vector for
regression and survival models, and a tidy `data.frame` for classifiers
(`output = "prob"`, `"class"`, or `"both"`).

## Additional functions

- [`varImp`](varImp.md):

  Variable importance for ML models (Cleveland dot plot via
  [`plot.varImp`](plot.varImp.md)).

- [`tMod`](tMod.md):

  Side-by-side comparison of multiple models.

- [`pseudoR2`](pseudoR2.md):

  Pseudo-R\\^2\\ measures for GLMs.

- [`vif`](vif.md):

  Variance inflation factors (VIF / GVIF).

- [`coefCI`](coefCI.md), [`rSq`](rSq.md):

  Bootstrap CIs for coefficients and R\\^2\\.

- [`conf`](https://andrisignorell.github.io/DescToolsX/reference/conf.html):

  Confusion matrix and classification metrics.

- [`roc`](roc.md), [`bestCut`](bestCut.md):

  ROC analysis.

- [`refLevel`](refLevel.md):

  Reference levels of factor predictors.

- [`rules`](rules.md), [`node`](node.md), [`cParam`](cParam.md),
  [`bestTree`](bestTree.md), [`leafRates`](leafRates.md),
  [`splits`](splits.md):

  Utilities for `rpart` trees.

## Datasets

The package ships with several benchmark datasets used in the vignette:
[`Admit`](Admit.md), [`Apt`](Apt.md), [`BioChemists`](BioChemists.md),
[`Contraception`](Contraception.md), [`Fish`](Fish.md),
[`IceCream`](IceCream.md), [`Lahigh`](Lahigh.md), [`Ologit`](Ologit.md),
[`Pima`](Pima.md), [`Whas100`](Whas100.md).

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

[`fitMod`](fitMod.md), [`print.FitMod`](print.FitMod.md),
[`predict.FitMod`](predict.FitMod.md)

## Author

**Maintainer**: Andri Signorell <andri@signorell.net>

Authors:

- Andri Signorell <andri@signorell.net>
