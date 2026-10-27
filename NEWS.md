# alloy 0.0.0.918

## New features

- Modelling layer of the DescToolsX package suite. `fitMod()` fits more
  than thirty regression and classification methods through one
  interface and selects a method automatically from the type of the
  response.
- Fitted objects carry the class `"FitMod"` on top of the original model
  object, so `print()`, `predict()`, `coef()` and `plot()` behave
  consistently while all methods of the underlying model remain
  available.
- `print.FitMod()` reports coefficient tables on the scale appropriate
  to the model — coefficients, odds ratios, incidence rate ratios,
  hazard ratios or time ratios — with reference-category headers for
  factors and optional robust standard errors.
- `predict.FitMod()` returns probabilities, classes or both, with the
  column order always aligned to the factor levels of the response.
- Diagnostics for binary responses: binned residuals, calibration
  curves, randomised quantile residuals, influence and separation plots,
  and partial residual plots for linearity in the logit. See
  `?model-diagnostics-overview`.
- `tMod()` compares several fitted models in a table or a plot.
- `crossVal()` estimates the out-of-sample performance of a fitted model
  by (repeated, stratified or grouped) k-fold cross-validation and
  reports apparent, cross-validated and pooled measures with the
  optimism of the apparent ones.
- `fitMod(engine = "logitFirth")` fits Firth's bias-reduced logistic
  regression (via brglm2), with finite estimates under separation;
  `drop1()` and `confint()` give penalized likelihood ratio tests and
  profile penalized likelihood intervals.
  A Firth fit that does not converge from the default start values of
  brglm2 (the diverging ML estimates under complete separation) is
  repeated from 0.
- `update()` on a `fitMod()` result refits through `fitMod()`, so the
  `"FitMod"` class and its methods are kept; a different `engine` may be
  given.
- The metrics of DescToolsX get their methods for fitted models from
  alloy, see `?"classification-metrics"` and `?"regression-metrics"`:
  `confusion()`, `cStat()`, `brierScore()`, `averagePrecision()` and
  `logLoss()` for `fitMod()` classification models (on the training data
  or on `newdata`) and for `glm` objects, `confusion()` also for
  `multinom`, `rpart`, `randomForest`, `svm`, `lda` and `qda`; `mae()`,
  `mse()`, `rmse()`, `mape()` and `smape()` for `fitMod()` regression
  models and for `lm` objects. These methods used to live in DescToolsX,
  which now works on vectors and tables only. The ten metrics are
  re-exported.
- `tModSummary()` reported the MAPE relative to the predicted instead of
  the observed values (prediction and observation were passed in swapped
  order); MAE, MSE and RMSE were not affected.
- `predict()` on a logistic `fitMod()` model (`"logit"`, `"logitFirth"`)
  passes an explicit `type = "link"`, `"response"` or `"terms"` to
  `predict.glm()`, so functions written for `glm` objects, such as
  `DescToolsX::brierScore()`, work with these models. `roc()` and `lift()`
  request the probabilities with `output = "prob"`.
- Bootstrap routines (`coefCI()`, `rSq()`) are implemented in C++ via
  Rcpp, RcppArmadillo and RcppParallel.

## Acknowledgements

Parts of the code and documentation were reviewed with the help of large
language models (OpenAI Codex, Anthropic Claude). Every suggestion was
assessed, edited and verified by the maintainer, who remains solely
responsible for the content of this package.
