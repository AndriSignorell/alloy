# Predict method for FitMod objects

Unified predict interface for all models fitted via
[`fitMod`](fitMod.md). For regression and survival models the predicted
values are returned as a numeric vector. For classification models
either class probabilities, predicted classes, or both are returned as a
`data.frame` with consistent column names across all model types.

## Usage

``` r
# S3 method for class 'FitMod'
predict(
  object,
  newdata = NULL,
  output = c("prob", "class", "both", "where", "leaf"),
  s = "lambda.1se",
  type = NULL,
  ...
)
```

## Arguments

- object:

  A fitted model of class `"FitMod"`.

- newdata:

  Optional data frame of new observations. If omitted, fitted values on
  the training data are returned.

- output:

  Character string controlling the output for classification models. One
  of `"prob"` (default), `"class"`, or `"both"`. For `fitfn = "rpart"`
  additionally `"where"` (row index of the predicted leaf in the tree
  frame) or `"leaf"` (node label of the predicted leaf) are available.
  Ignored for regression and survival models.

- s:

  For `fitfn = "glmnet"` only: the value of the penalty parameter
  \\\lambda\\ at which predictions are made. Passed to
  [`predict.cv.glmnet`](https://glmnet.stanford.edu/reference/predict.cv.glmnet.html).
  Default is `"lambda.1se"`.

- type:

  For regression models (`lm`, `glm`, etc.): the `type` argument passed
  to the underlying `predict` method. If not supplied, `"response"` is
  used, so that predictions are returned on the response scale both with
  and without `newdata`. For Cox models the default is `"risk"`; for
  parametric survival models (incl. `tobit`) the default is
  `"response"`. Ignored for classification models (use `output`
  instead).

- ...:

  Further arguments passed to the underlying predict method.

## Value

- Regression models:

  A numeric vector of fitted/predicted values on the response scale.
  This includes regression variants of the machine-learning methods
  (`rpart` with `method = "anova"`, `randomForest`/`svm`/`nnet`
  regressions, `glmnet` with gaussian/poisson family, `xgboost` with a
  regression objective).

- Survival models (`coxph`):

  A numeric vector of predicted risk scores (`type = "risk"` by
  default).

- Parametric survival models (`weibull`, `exponential`, `lognormal`,
  `loglogistic`, `tobit`):

  A numeric vector of predicted survival times / expected responses
  (`type = "response"` by default).

- Classification models:

  `output = "prob"`

  :   A `data.frame` with one column per class containing predicted
      probabilities. Column names match the factor levels of the
      response variable.

  `output = "class"`

  :   A `data.frame` with a single column `class` (factor) containing
      the predicted class.

  `output = "both"`

  :   The probability columns and the `class` column combined in one
      `data.frame`.

## Details

Whether a model is treated as classification or regression is decided
from the fitted object itself (e.g. `rpart$method`, `randomForest$type`,
the `family`/`objective` stored by [`fitMod()`](fitMod.md) for
`glmnet`/`xgboost`), not from the fitting method alone.

For classification models the column order of probability outputs is
always aligned with the factor levels of the response variable,
regardless of which model type is used. This ensures that
`predict(fitLogit)` and `predict(fitRf)` return columns in the same
order.

Models that require explicit `newdata` even for training-data
predictions (e.g. `svm`, `C5.0`, `randomForest`) are handled
transparently via an internal helper. Note that for `randomForest` this
returns in-sample (not out-of-bag) predictions, consistent with the
fitted-values semantics of all other methods.

For `fitfn = "glmnet"` and `"xgboost"`, design matrices for `newdata`
are rebuilt from the `terms` and factor levels of the training data, so
new data may contain a subset of the training factor levels.

For `fitfn = "logit"`, calling `predict(object)` returns a two-column
probability `data.frame` (like all other classifiers). To obtain the
linear predictor (log-odds), use `predict(object, type = "link")`.

## See also

[`fitMod`](fitMod.md), [`print.FitMod`](print.FitMod.md)

Other modelling: [`fitMod()`](fitMod.md),
[`plot.FitMod()`](plot.FitMod.md), [`predictors()`](predictors.md),
[`print.FitMod()`](print.FitMod.md)

## Examples

``` r
# Regression
fitLm <- fitMod(Fertility ~ ., swiss)
#> fitMod: using fitfn = 'lm'
head(predict(fitLm))
#>   Courtelary     Delemont Franches-Mnt      Moutier   Neuveville   Porrentruy 
#>     74.61530     82.50994     85.91826     76.82039     64.70241     90.50011 

# Binary classification - probabilities
fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
head(predict(fitLogit))
#>           0         1
#> 1 0.8273735 0.1726265
#> 2 0.7078250 0.2921750
#> 3 0.2615918 0.7384082
#> 4 0.8216154 0.1783846
#> 5 0.8816461 0.1183539
#> 6 0.6300301 0.3699699
head(predict(fitLogit, output = "both"))
#>           0         1 class
#> 1 0.8273735 0.1726265     0
#> 2 0.7078250 0.2921750     0
#> 3 0.2615918 0.7384082     1
#> 4 0.8216154 0.1783846     0
#> 5 0.8816461 0.1183539     0
#> 6 0.6300301 0.3699699     0

# Multinomial classification
if (requireNamespace("nnet", quietly = TRUE)) {
  fitMult <- fitMod(ice_cream ~ video + puzzle + female,
                    IceCream, fitfn = "multinom")
  head(predict(fitMult, output = "both"))
}
#>     vanilla  chocolate strawberry      class
#> 1 0.5457004 0.13270412  0.3215955    vanilla
#> 2 0.4347110 0.14041387  0.4248751    vanilla
#> 3 0.5668390 0.29264026  0.1405208    vanilla
#> 4 0.5355472 0.11755137  0.3469015    vanilla
#> 5 0.5040795 0.09107643  0.4048441    vanilla
#> 6 0.4645151 0.06630794  0.4691769 strawberry

# Cox model - risk scores
if (requireNamespace("survival", quietly = TRUE)) {
  fitCox <- fitMod(Surv(foltime, folstatus) ~ gender, Whas100,
                   fitfn = "coxph")
  head(predict(fitCox))

  # Parametric survival - expected survival time
  fitWei <- fitMod(Surv(foltime, folstatus) ~ gender + age, Whas100,
                   fitfn = "weibull")
  head(predict(fitWei))
}
#> [1] 4994.958 1033.530 2558.231 1526.986 2419.490 1444.172
```
