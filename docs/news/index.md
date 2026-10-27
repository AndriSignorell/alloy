# Changelog

## alloy 0.0.0.918

### New features

- Modelling layer of the DescToolsX package suite.
  [`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md)
  fits more than thirty regression and classification methods through
  one interface and selects a method automatically from the type of the
  response.
- Fitted objects carry the class `"FitMod"` on top of the original model
  object, so [`print()`](https://rdrr.io/r/base/print.html),
  [`predict()`](https://rdrr.io/r/stats/predict.html),
  [`coef()`](https://rdrr.io/r/stats/coef.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) behave
  consistently while all methods of the underlying model remain
  available.
- [`print.FitMod()`](https://andrisignorell.github.io/alloy/reference/print.FitMod.md)
  reports coefficient tables on the scale appropriate to the model —
  coefficients, odds ratios, incidence rate ratios, hazard ratios or
  time ratios — with reference-category headers for factors and optional
  robust standard errors.
- [`predict.FitMod()`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md)
  returns probabilities, classes or both, with the column order always
  aligned to the factor levels of the response.
- Diagnostics for binary responses: binned residuals, calibration
  curves, randomised quantile residuals, influence and separation plots,
  and partial residual plots for linearity in the logit. See
  `?model-diagnostics-overview`.
- [`tMod()`](https://andrisignorell.github.io/alloy/reference/tMod.md)
  compares several fitted models in a table or a plot.
- [`crossVal()`](https://andrisignorell.github.io/alloy/reference/crossVal.md)
  estimates the out-of-sample performance of a fitted model by
  (repeated, stratified or grouped) k-fold cross-validation and reports
  apparent, cross-validated and pooled measures with the optimism of the
  apparent ones.
- `fitMod(engine = "logitFirth")` fits Firth’s bias-reduced logistic
  regression (via brglm2), with finite estimates under separation;
  [`drop1()`](https://rdrr.io/r/stats/add1.html) and
  [`confint()`](https://rdrr.io/r/stats/confint.html) give penalized
  likelihood ratio tests and profile penalized likelihood intervals. A
  Firth fit that does not converge from the default start values of
  brglm2 (the diverging ML estimates under complete separation) is
  repeated from 0.
- [`update()`](https://rdrr.io/r/stats/update.html) on a
  [`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md)
  result refits through
  [`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md),
  so the `"FitMod"` class and its methods are kept; a different `engine`
  may be given.
- The metrics of DescToolsX get their methods for fitted models from
  alloy, see
  [`?"classification-metrics"`](https://andrisignorell.github.io/alloy/reference/classification-metrics.md)
  and
  [`?"regression-metrics"`](https://andrisignorell.github.io/alloy/reference/regression-metrics.md):
  [`confusion()`](https://andrisignorell.github.io/DescToolsX/reference/confusion.html),
  [`cStat()`](https://andrisignorell.github.io/DescToolsX/reference/cStat.html),
  [`brierScore()`](https://andrisignorell.github.io/DescToolsX/reference/brierScore.html),
  [`averagePrecision()`](https://andrisignorell.github.io/DescToolsX/reference/averagePrecision.html)
  and
  [`logLoss()`](https://andrisignorell.github.io/DescToolsX/reference/logLoss.html)
  for
  [`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md)
  classification models (on the training data or on `newdata`) and for
  `glm` objects,
  [`confusion()`](https://andrisignorell.github.io/DescToolsX/reference/confusion.html)
  also for `multinom`, `rpart`, `randomForest`, `svm`, `lda` and `qda`;
  [`mae()`](https://andrisignorell.github.io/DescToolsX/reference/mae.html),
  [`mse()`](https://andrisignorell.github.io/DescToolsX/reference/mse.html),
  [`rmse()`](https://andrisignorell.github.io/DescToolsX/reference/rmse.html),
  [`mape()`](https://andrisignorell.github.io/DescToolsX/reference/mape.html)
  and
  [`smape()`](https://andrisignorell.github.io/DescToolsX/reference/smape.html)
  for
  [`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md)
  regression models and for `lm` objects. These methods used to live in
  DescToolsX, which now works on vectors and tables only. The ten
  metrics are re-exported.
- [`tModSummary()`](https://andrisignorell.github.io/alloy/reference/tModSummary.md)
  reported the MAPE relative to the predicted instead of the observed
  values (prediction and observation were passed in swapped order); MAE,
  MSE and RMSE were not affected.
- [`predict()`](https://rdrr.io/r/stats/predict.html) on a logistic
  [`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md)
  model (`"logit"`, `"logitFirth"`) passes an explicit `type = "link"`,
  `"response"` or `"terms"` to
  [`predict.glm()`](https://rdrr.io/r/stats/predict.glm.html), so
  functions written for `glm` objects, such as
  [`DescToolsX::brierScore()`](https://andrisignorell.github.io/DescToolsX/reference/brierScore.html),
  work with these models.
  [`roc()`](https://andrisignorell.github.io/alloy/reference/roc.md) and
  [`lift()`](https://andrisignorell.github.io/alloy/reference/lift.md)
  request the probabilities with `output = "prob"`.
- Bootstrap routines
  ([`coefCI()`](https://andrisignorell.github.io/alloy/reference/coefCI.md),
  [`rSq()`](https://andrisignorell.github.io/alloy/reference/rSq.md))
  are implemented in C++ via Rcpp, RcppArmadillo and RcppParallel.

### Acknowledgements

Parts of the code and documentation were reviewed with the help of large
language models (OpenAI Codex, Anthropic Claude). Every suggestion was
assessed, edited and verified by the maintainer, who remains solely
responsible for the content of this package.
