# Classification Metrics of Fitted Models

Methods of the classification metrics of DescToolsX for fitted models:
the confusion matrix
([`DescToolsX::confusion()`](https://andrisignorell.github.io/DescToolsX/reference/confusion.html)),
the C-statistic
([`DescToolsX::cStat()`](https://andrisignorell.github.io/DescToolsX/reference/cStat.html)),
the Brier score
([`DescToolsX::brierScore()`](https://andrisignorell.github.io/DescToolsX/reference/brierScore.html)),
the average precision
([`DescToolsX::averagePrecision()`](https://andrisignorell.github.io/DescToolsX/reference/averagePrecision.html))
and the log loss
([`DescToolsX::logLoss()`](https://andrisignorell.github.io/DescToolsX/reference/logLoss.html)).
The metrics themselves work on predictions and observed outcomes; the
methods here take both from a model.

## Usage

``` r
# S3 method for class 'FitMod'
confusion(x, newdata = NULL, cutoff = 0.5, pos = NULL, ...)

# S3 method for class 'FitMod'
cStat(x, newdata = NULL, ...)

# S3 method for class 'FitMod'
brierScore(x, newdata = NULL, ...)

# S3 method for class 'FitMod'
averagePrecision(x, newdata = NULL, ...)

# S3 method for class 'FitMod'
logLoss(x, newdata = NULL, ...)

# S3 method for class 'glm'
confusion(x, cutoff = 0.5, pos = NULL, ...)

# S3 method for class 'glm'
cStat(x, ...)

# S3 method for class 'glm'
brierScore(x, ...)

# S3 method for class 'glm'
averagePrecision(x, ...)

# S3 method for class 'glm'
logLoss(x, ...)

# S3 method for class 'multinom'
confusion(x, ...)

# S3 method for class 'rpart'
confusion(x, ...)

# S3 method for class 'randomForest'
confusion(x, ...)

# S3 method for class 'svm'
confusion(x, ...)

# S3 method for class 'lda'
confusion(x, ...)

# S3 method for class 'qda'
confusion(x, ...)
```

## Arguments

- x:

  a classification model fitted with
  [`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md),
  or a model object of one of the classes listed under Details

- newdata:

  an optional data frame to evaluate, containing the response. `NULL`
  (default) uses the training data. For models fitted with
  [`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md)
  only.

- cutoff:

  the probability of the positive class above which an observation is
  classified as positive, for binary models. Default 0.5.

- pos:

  the positive class of a binary model, by default the second level of
  the response

- ...:

  further arguments passed to the metric, e.g. `conf.level`, `sides`,
  `scaled` or `eps`

## Value

As the respective metric in DescToolsX.

## Details

**Models fitted with
[`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md).**
Predictions come from
[`predict.FitMod()`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md),
so the methods work alike for every classification engine (logistic and
Firth logistic regression, multinomial models, trees, random forests,
...), on the training data or on `newdata`. The observed classes are
aligned with the probability columns by their labels, so a 0/1 coded
response gives the classes `"0"` and `"1"`, and the second class is the
positive one. A binary model is classified by comparing the probability
of the positive class with `cutoff`, a model with more classes by the
class of highest probability. The C-statistic, the Brier score, the
average precision and the log loss need a binary model.

**Other model objects.**
[`confusion()`](https://andrisignorell.github.io/DescToolsX/reference/confusion.html)
has methods for `glm`, `multinom` (nnet), `rpart`, `randomForest`
(out-of-bag predictions), `svm` (e1071), `lda` and `qda` (MASS);
[`cStat()`](https://andrisignorell.github.io/DescToolsX/reference/cStat.html),
[`brierScore()`](https://andrisignorell.github.io/DescToolsX/reference/brierScore.html),
[`averagePrecision()`](https://andrisignorell.github.io/DescToolsX/reference/averagePrecision.html)
and
[`logLoss()`](https://andrisignorell.github.io/DescToolsX/reference/logLoss.html)
have methods for binomial `glm` objects. These use the fitted values and
the response stored in the model.
[`logLoss()`](https://andrisignorell.github.io/DescToolsX/reference/logLoss.html)
additionally requires ungrouped data (all prior weights 1) and refuses
`quasibinomial`, which has no full likelihood.

Measures on the training data are optimistic;
[`crossVal()`](https://andrisignorell.github.io/alloy/reference/crossVal.md)
reports their cross-validated values.

## See also

[`crossVal()`](https://andrisignorell.github.io/alloy/reference/crossVal.md)
for cross-validated metrics,
[regression-metrics](https://andrisignorell.github.io/alloy/reference/regression-metrics.md)

Other modelling:
[`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md),
[`logitFirth`](https://andrisignorell.github.io/alloy/reference/logitFirth.md),
[`plot.FitMod()`](https://andrisignorell.github.io/alloy/reference/plot.FitMod.md),
[`predict.FitMod()`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md),
[`predictors()`](https://andrisignorell.github.io/alloy/reference/predictors.md),
[`print.FitMod()`](https://andrisignorell.github.io/alloy/reference/print.FitMod.md),
[`regression-metrics`](https://andrisignorell.github.io/alloy/reference/regression-metrics.md)

## Examples

``` r
fit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")

confusion(fit)
#> 
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction   1   0
#>          1  30  19
#>          0  97 254
#> 
#>                 Total n : 400
#>                Accuracy : 0.7100
#>                 95% CI : (0.6637, 0.7523)
#>     No Information Rate : 0.6825
#>     P-Value [Acc > NIR] : 0.129
#>                   Kappa : 0.1994
#>  McNemar's Test P-Value : < 0.001
#> 
#>             Sensitivity : 0.2362
#>             Specificity : 0.9304
#>          Pos Pred Value : 0.6122
#>          Neg Pred Value : 0.7236
#>              Prevalence : 0.3175
#>          Detection Rate : 0.0750
#>    Detection Prevalence : 0.1225
#>       Balanced Accuracy : 0.5833
#>                 F-Value : 0.3409
#>     Matthews Cor.-Coef. : 0.2366
#> 
#>        'Positive' Class : 1
#> 
confusion(fit, cutoff = 0.3)
#> 
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction   1   0
#>          1  85 112
#>          0  42 161
#> 
#>                 Total n : 400
#>                Accuracy : 0.6150
#>                 95% CI : (0.5664, 0.6614)
#>     No Information Rate : 0.6825
#>     P-Value [Acc > NIR] : 0.998
#>                   Kappa : 0.2258
#>  McNemar's Test P-Value : < 0.001
#> 
#>             Sensitivity : 0.6693
#>             Specificity : 0.5897
#>          Pos Pred Value : 0.4315
#>          Neg Pred Value : 0.7931
#>              Prevalence : 0.3175
#>          Detection Rate : 0.2125
#>    Detection Prevalence : 0.4925
#>       Balanced Accuracy : 0.6295
#>                 F-Value : 0.5247
#>     Matthews Cor.-Coef. : 0.2412
#> 
#>        'Positive' Class : 1
#> 
cStat(fit)
#> [1] 0.6928413
brierScore(fit, scaled = TRUE)
#> [1] 0.1014325
averagePrecision(fit)
#> [1] 0.5127557
logLoss(fit)
#> [1] 0.5731469

# out of sample
set.seed(1)
d <- splitTrainTest(Admit, p = 0.3)
m <- update(fit, data = d$train)
confusion(m, newdata = d$test)
#> 
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  0
#>          1 11  9
#>          0 24 76
#> 
#>                 Total n : 120
#>                Accuracy : 0.7250
#>                 95% CI : (0.6391, 0.7970)
#>     No Information Rate : 0.7083
#>     P-Value [Acc > NIR] : 0.387
#>                   Kappa : 0.2385
#>  McNemar's Test P-Value : 0.015
#> 
#>             Sensitivity : 0.3143
#>             Specificity : 0.8941
#>          Pos Pred Value : 0.5500
#>          Neg Pred Value : 0.7600
#>              Prevalence : 0.2917
#>          Detection Rate : 0.0917
#>    Detection Prevalence : 0.1667
#>       Balanced Accuracy : 0.6042
#>                 F-Value : 0.4000
#>     Matthews Cor.-Coef. : 0.2542
#> 
#>        'Positive' Class : 1
#> 
cStat(m, newdata = d$test, conf.level = 0.95)
#>       est       lci       uci 
#> 0.6836975 0.5703704 0.7837733 

# a plain glm
g <- glm(am ~ hp + wt, data = mtcars, family = binomial)
confusion(g)
#> 
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  0
#>          1 12  1
#>          0  1 18
#> 
#>                 Total n : 32
#>                Accuracy : 0.9375
#>                 95% CI : (0.7985, 0.9827)
#>     No Information Rate : 0.5938
#>     P-Value [Acc > NIR] : < 0.001
#>                   Kappa : 0.8704
#>  McNemar's Test P-Value : 1
#> 
#>             Sensitivity : 0.9231
#>             Specificity : 0.9474
#>          Pos Pred Value : 0.9231
#>          Neg Pred Value : 0.9474
#>              Prevalence : 0.4063
#>          Detection Rate : 0.3750
#>    Detection Prevalence : 0.4063
#>       Balanced Accuracy : 0.9352
#>                 F-Value : 0.9231
#>     Matthews Cor.-Coef. : 0.8704
#> 
#>        'Positive' Class : 1
#> 
logLoss(g)
#> [1] 0.1571736
```
