# k-Fold Cross-Validation of a Fitted Model

Estimates the out-of-sample performance of a fitted model by k-fold
cross-validation. The data are split into `k` folds, the model is
refitted on `k - 1` of them with the specification of `fit` and
evaluated on the fold left out, until every fold has served once as test
set. The procedure can be repeated with new random splits.

## Usage

``` r
crossVal(
  fit,
  data = NULL,
  k = 10,
  repeats = 1,
  folds = NULL,
  strata = TRUE,
  cutoff = 0.5,
  keepModels = FALSE
)

# S3 method for class 'CrossVal'
print(x, digits = 3, ...)
```

## Arguments

- fit:

  a model fitted with
  [`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md),
  or a plain [`lm()`](https://rdrr.io/r/stats/lm.html) or
  [`glm()`](https://rdrr.io/r/stats/glm.html) (binomial, Poisson,
  quasi-Poisson or Gamma family), which is refitted through
  [`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md)
  with the matching engine. Binary models need a Bernoulli response (one
  0/1, logical or two-level factor value per row); aggregated binomial
  data (`cbind(successes, failures)` or proportions with trial weights)
  are not supported.

- data:

  the data frame to split. `NULL` (default) evaluates the `data`
  argument of the original call in the calling environment.

- k:

  the number of folds, between 2 and the number of observations
  (leave-one-out). Default 10.

- repeats:

  the number of times the cross-validation is repeated with a new random
  split. Default 1.

- folds:

  an optional vector of length `nrow(data)` with predefined fold
  memberships, e.g. for grouped cross-validation where all observations
  of a cluster must fall into the same fold. Overrides `k`, `repeats`
  and `strata`.

- strata:

  `TRUE` (default) stratifies the folds by the response for
  classification models and does not stratify regression models; `FALSE`
  never stratifies. A vector of length `nrow(data)`, or a list of such
  vectors, gives the strata explicitly, see
  [`splitTrainTest()`](https://andrisignorell.github.io/alloy/reference/splitTrainTest.md).

- cutoff:

  the probability threshold at or above which a binary prediction counts
  as positive, used for accuracy, sensitivity and specificity: a number
  in \\\[0, 1\]\\ (default 0.5), or `"youden"` or `"closest.topleft"`
  for the optimal cut-point of
  [`bestCut()`](https://andrisignorell.github.io/alloy/reference/bestCut.md),
  determined anew on every training fold (see Details).

- keepModels:

  logical, whether to return the models fitted on the training folds.
  Default `FALSE`.

- x:

  an object of class `"CrossVal"`

- digits:

  the number of digits to print

- ...:

  further arguments, ignored

## Value

An object of class `"CrossVal"`, a list with the components

- summary:

  a data frame with one row per measure and the columns `apparent`,
  `cv`, `sd`, `min`, `max`, `nFolds` (the number of folds with a value
  for the measure), `pooled` and `optimism`.

- perFold:

  a data frame with one row per planned fold: `rep`, `fold`, `nTrain`,
  `nTest`, `ok`, `error`, for binary models `cutoff`, and the measures
  (`NA` for failed folds).

- reps:

  a data frame with one row per repetition: `rep`, `nPredicted` and
  `coverage`, the share of observations with an out-of-fold prediction.

- pred:

  the out-of-fold predictions as a data frame with `rep`, `fold`, `row`
  (row in `data`), `obs` and `pred` (the probability of the positive
  class, the predicted value, or for multiclass models the predicted
  class), for binary models followed by the `cutoff` it was classified
  with.

- prob:

  for multiclass models a matrix of the predicted class probabilities,
  one column per class, its rows matching `pred`; else `NULL`.

- foldId:

  an integer matrix of fold memberships, one column per repetition, rows
  matching the observations used.

- coefs:

  a matrix of the coefficients per successful fold, matched by name and
  `NA` where a fold lacks a coefficient, if
  [`coef()`](https://rdrr.io/r/stats/coef.html) returns a named numeric
  vector for every fold (unnamed vectors of equal length are bound by
  position), else `NULL`.

- models:

  the models of the successful training folds if `keepModels = TRUE`,
  else `NULL`.

- warnings, errors:

  data frames of the distinct messages and their `count`: for `errors`
  the number of failed folds, for `warnings` the number of evaluations
  (folds, apparent, pooled repetitions) that raised the message.

and the settings `task`, `engine`, `formula`, `n`, `nDropped`, `k`,
`repeats`, `stratified`, `cutoff` (the number or method given),
`cutoffApparent` (the cut-point of the apparent measures), `levels` and
`call`.

## Details

**Observations.** The rows entering the cross-validation are determined
with the semantics of
[`model.frame()`](https://rdrr.io/r/stats/model.frame.html): the
`subset` of the original call is resolved with R's indexing rules
(logical, positive, negative or row-name indices), and rows are dropped
where any model term, including transformed ones such as `log(x)` or
`sqrt(x)`, the response or the weights evaluate to `NA` or `NaN`. This
is done once before the split, so that the apparent and the
cross-validated estimates rest on the same observations and the folds
are of comparable size. A subset that selects a row more than once is
rejected, as the copies could otherwise end up in training and test
fold. Variables are evaluated on the whole data only to find these rows;
every fold model recomputes its terms (e.g.
[`poly()`](https://rdrr.io/r/stats/poly.html) or
[`scale()`](https://rdrr.io/r/base/scale.html)) from its own training
data. Terms that cannot be evaluated on `data` are an error, as they
would be for the original fit.

The response must be computed row by row (`log(y)` is fine), since it is
compared with predictions of models whose response was computed on their
training folds only. A response that depends on other observations is
rejected if it calls a known data-dependent function such as
[`scale()`](https://rdrr.io/r/base/scale.html),
[`mean()`](https://rdrr.io/r/base/mean.html) or
[`rank()`](https://rdrr.io/r/base/rank.html), or if its values
recomputed on the odd rows, the even rows or the first half of the rows
differ from those computed on all rows. The second check is a heuristic
that may miss a user-defined transformation which happens to agree on
that subset; such transformations are the user's responsibility.
Computing the transformation in `data` beforehand gives all folds a
common scale, but its estimation (e.g. of the mean and SD used by
[`scale()`](https://rdrr.io/r/base/scale.html)) is then not part of the
cross-validation.

**Refitting.** Each training fold is refitted from the call stored in
`fit` with the data replaced, so the engine, family, link and tuning
parameters are carried over. Row-wise arguments are aligned with the
folds: prior weights, whether a column of `data` or a vector outside it,
are evaluated once and split together with the data. Prior weights are
used in refitting, but the performance measures are unweighted. An
`offset` argument is not supported (specify it in the formula as
[`offset()`](https://rdrr.io/r/stats/offset.html)), nor is a formula
variable of observation length that does not belong to `data`.

**Measures.** They depend on the task:

- binary classification:

  `auc` (c-statistic), `brier` and `brierScaled` (Brier score and its
  scaled version, see
  [`DescToolsX::brierScore()`](https://andrisignorell.github.io/DescToolsX/reference/brierScore.html)),
  `logLoss` (mean negative log-likelihood), `calIntercept`
  (calibration-in-the-large: the intercept of a logistic regression of
  the outcome with the logit of the prediction as offset, i.e. with the
  slope fixed at 1), `calSlope` (the slope of the logistic regression of
  the outcome on the logit of the prediction). The two fits are
  separate. The intercept is `NA` if its fit does not converge; the
  slope is `NA` if the logits of the two classes do not overlap
  (complete or quasi-complete separation), if the logits are constant,
  or if its fit does not converge. Each case is reported in the
  warnings. The remaining measures are `accuracy`, `sensitivity` and
  `specificity` at `cutoff`. The second level of the response is the
  positive class.

- multiclass classification:

  `accuracy`, `kappa`, `brier` (summed over the classes) and `logLoss`.

- regression:

  `rmse`, `mae`, `rSq` (\\1 - SSE/SST\\ on the test fold) and `calSlope`
  (slope of the observed on the predicted values).

**Data-driven cutoff.** With `cutoff = "youden"` (or
`"closest.topleft"`) the cut-point is part of the model and is
cross-validated with it: every fold model determines its cut-point with
[`bestCut()`](https://andrisignorell.github.io/alloy/reference/bestCut.md)
on its own training data and classifies the test fold with it, so the
cut-point never sees the observations it classifies. The apparent
measures use the cut-point of `fit` on all data, as `bestCut(roc(fit))`
returns it. A cut-point chosen on all data and then passed as a number
would make the cross-validated accuracy, sensitivity and specificity
optimistic. The cut-points are reported in `perFold` and `pred`; the
pooled measures classify every prediction with the cut-point of its own
fold. Ties of the criterion are resolved by the first cut-point that
pROC reports.

**Apparent, CV, pooled.** *Apparent* is the performance of `fit` on the
data it was fitted to and therefore optimistic. *CV* is the unweighted
mean of the fold results over all folds (and repeats): every fold counts
equally, whatever its size. `sd`, `min` and `max` describe the spread of
the fold results; `sd` is not a standard error of the CV estimate.
*Pooled* computes each measure once on all out-of-fold predictions of a
repetition and averages over the repetitions. This is a different
estimand from the fold mean, also for losses such as the RMSE, and the
more stable choice for small folds. With leave-one-out, measures that
need both classes or several observations (`auc`, `brierScaled`, the
calibration measures, `rSq`, `calSlope`, `kappa`) exist only in their
pooled version.

*Optimism* is the difference Apparent - CV: positive for measures where
higher is better (`auc`, `rSq`, `accuracy`), negative for losses
(`brier`, `logLoss`, `rmse`, `mae`). For the calibration measures it is
merely the difference, without a better direction. For an unpenalised
logistic model with an intercept, fitted by maximum likelihood without
weights, the apparent calibration intercept and slope are 0 and 1 by
construction; a cross-validated slope clearly below 1 is the signature
of overfitting.

Cross-validation assesses the given model specification. A variable
selection or tuning done beforehand on the full data is not part of the
refit and therefore not validated.

**Failures.** Warnings raised while fitting the folds or computing the
measures (e.g. fitted probabilities of 0 or 1) are collected and
reported once, with the number of times each was raised (counting the
folds, the apparent and the pooled evaluations). A fold whose model
cannot be fitted, cannot predict its test data (e.g. because of a factor
level absent from the training folds), returns predictions of the wrong
shape or invalid values, or whose measures cannot be computed, is kept
in `perFold` with `ok = FALSE` and its error, and is left out of the CV
estimates; the function only fails if no fold succeeds. The pooled
estimates use only repetitions in which every observation was predicted,
as an incomplete repetition would describe a different population;
without such a repetition they are `NA`.

For binary models,
[`plotCalibration()`](https://andrisignorell.github.io/alloy/reference/plotCalibration.md)
and [`roc()`](https://andrisignorell.github.io/alloy/reference/roc.md)
accept the result and show the out-of-fold predictions, see
[`rocTest()`](https://andrisignorell.github.io/alloy/reference/rocTest.md)
for comparing the cross-validated ROC curves of two models.

The split is drawn from R's global random number generator; call
[`set.seed()`](https://rdrr.io/r/base/Random.html) before the function
for reproducible results. Fit a model whose fitting draws random numbers
itself (e.g. a random forest) before setting the seed: written as a call
inside `crossVal()`, it is evaluated before the split and shifts the
random stream, so that the folds no longer match those of another model
cross-validated with the same seed. Survival models are not supported.

For mixed models, observations of clusters absent from the training
folds (always so in grouped cross-validation by cluster) are predicted
at population level, with the random effects set to zero.

The models kept with `keepModels = TRUE` support
[`predict()`](https://rdrr.io/r/stats/predict.html),
[`coef()`](https://rdrr.io/r/stats/coef.html) and inspection. Their call
refers to a temporary training set, so
[`update()`](https://rdrr.io/r/stats/update.html) needs the data
supplied again.

## See also

Other data.split:
[`splitTrainTest()`](https://andrisignorell.github.io/alloy/reference/splitTrainTest.md)

## Examples

``` r
fit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")

set.seed(1)
cv <- crossVal(fit)
cv
#> 
#> Cross-validation (10-fold, stratified)
#> 
#> Model:  admit ~ gre + gpa + rank  [logit]
#> Obs:    400, positive class "1", cutoff 0.5
#> 
#>              Apparent     CV     SD    Min    Max Pooled Optimism
#> auc             0.693  0.676  0.046  0.613  0.751  0.666    0.017
#> brier           0.195  0.200  0.013  0.183  0.216  0.200   -0.006
#> brierScaled     0.101  0.075  0.052  0.013  0.168  0.075    0.027
#> logLoss         0.573  0.588  0.030  0.546  0.626  0.588   -0.015
#> calIntercept    0.000  0.005  0.187 -0.306  0.312  0.004   -0.005
#> calSlope        1.000  0.952  0.391  0.528  1.717  0.835    0.048
#> accuracy        0.710  0.693  0.035  0.650  0.750  0.693    0.017
#> sensitivity     0.236  0.221  0.121  0.077  0.462  0.220    0.016
#> specificity     0.930  0.912  0.054  0.821  1.000  0.912    0.018
#> 

# the measures of every fold, and the variability of the coefficients
head(cv$perFold)
#>   rep fold nTrain nTest   ok error cutoff       auc     brier brierScaled
#> 1   1    1    360    40 TRUE  <NA>    0.5 0.6130952 0.2065777  0.01629690
#> 2   1    2    360    40 TRUE  <NA>    0.5 0.7321429 0.1839028  0.12427243
#> 3   1    3    360    40 TRUE  <NA>    0.5 0.7261905 0.1847553  0.12021268
#> 4   1    4    360    40 TRUE  <NA>    0.5 0.6495726 0.2113654  0.03651113
#> 5   1    5    360    40 TRUE  <NA>    0.5 0.6552707 0.2126330  0.03073291
#> 6   1    6    360    40 TRUE  <NA>    0.5 0.6581197 0.2051423  0.06487833
#>     logLoss calIntercept  calSlope accuracy sensitivity specificity
#> 1 0.6041265  -0.23602090 0.5810534    0.675  0.33333333   0.8214286
#> 2 0.5467780   0.05631846 1.7170110    0.700  0.08333333   0.9642857
#> 3 0.5499754  -0.06500459 1.0246174    0.700  0.25000000   0.8928571
#> 4 0.6192382   0.19016074 0.6309090    0.675  0.07692308   0.9629630
#> 5 0.6095431   0.05738940 0.7628065    0.650  0.07692308   0.9259259
#> 6 0.6011644   0.31168421 1.0474139    0.750  0.23076923   1.0000000
apply(cv$coefs, 2, sd, na.rm = TRUE)
#>  (Intercept)          gre          gpa        rank2        rank3        rank4 
#> 0.4495185857 0.0004797699 0.1270814117 0.0869132107 0.1092291610 0.1643939428 

# the Youden cut-point, determined on every training fold
set.seed(1)
cvY <- crossVal(fit, cutoff = "youden")
cvY$cutoffApparent
#> [1] 0.3536984
summary(cvY$perFold$cutoff)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.2966  0.3528  0.3599  0.3573  0.3692  0.3912 

# repeated cross-validation, keeping the fold models
set.seed(1)
cv <- crossVal(fit, k = 5, repeats = 10, keepModels = TRUE)
round(cv$summary, 3)
#>              apparent    cv    sd    min   max nFolds pooled optimism
#> auc             0.693 0.671 0.055  0.533 0.782     50  0.667    0.022
#> brier           0.195 0.201 0.014  0.174 0.240     50  0.201   -0.006
#> brierScaled     0.101 0.073 0.064 -0.116 0.188     50  0.074    0.028
#> logLoss         0.573 0.589 0.034  0.526 0.695     50  0.589   -0.016
#> calIntercept    0.000 0.002 0.091 -0.188 0.193     50  0.001   -0.002
#> calSlope        1.000 0.909 0.373  0.099 1.751     50  0.818    0.091
#> accuracy        0.710 0.701 0.034  0.613 0.775     50  0.701    0.009
#> sensitivity     0.236 0.233 0.078  0.080 0.462     50  0.233    0.003
#> specificity     0.930 0.919 0.040  0.833 1.000     50  0.919    0.011

# grouped cross-validation: all rows of a cluster in the same fold
d <- Admit
d$id <- rep(1:100, each = 4)
ids <- unique(d$id)
grp <- splitTrainTest(ids, p = setNames(rep(1, 5), 1:5), output = "group")
cv <- crossVal(fit, data = d, folds = grp[match(d$id, ids)])

# regression
crossVal(fitMod(Fertility ~ ., swiss, engine = "lm"), k = 5)
#> 
#> Cross-validation (5-fold)
#> 
#> Model:  Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality  [lm]
#> Obs:    47
#> 
#>          Apparent     CV     SD    Min    Max Pooled Optimism
#> rmse        6.692  7.920  1.394  6.248  9.607  8.036   -1.228
#> mae         5.321  6.487  1.610  4.204  8.278  6.518   -1.166
#> rSq         0.707  0.445  0.208  0.187  0.730  0.577    0.262
#> calSlope    1.000  0.940  0.358  0.621  1.439  0.945    0.060
#> 
```
