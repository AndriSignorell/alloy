# alloy (development version)

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
- Bootstrap routines (`coefCI()`, `rSq()`) are implemented in C++ via
  Rcpp, RcppArmadillo and RcppParallel.

## Acknowledgements

Parts of the code and documentation were reviewed with the help of large
language models (OpenAI Codex, Anthropic Claude). Every suggestion was
assessed, edited and verified by the maintainer, who remains solely
responsible for the content of this package.
