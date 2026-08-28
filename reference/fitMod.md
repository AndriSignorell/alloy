# Fit a statistical or machine-learning model with automatic method selection

A unified interface for fitting a wide range of regression and
classification models. When `fitfn` is omitted the appropriate method is
chosen automatically from the type of the response variable. The return
value is always an object of class `"FitMod"` layered on top of the
original model object, so all standard methods (`predict`, `print`,
`coef`, ...) continue to work.

## Usage

``` r
fitMod(formula, data, ..., subset, na.action, fitfn = NULL)
```

## Arguments

- formula:

  A two-sided model formula.

- data:

  A data frame containing the variables in `formula`.

- ...:

  Additional arguments passed to the underlying fitting function.

- subset:

  An optional vector specifying a subset of observations. Only supported
  for fitting functions that accept a `subset` argument (and for
  `"glmnet"` and `"xgboost"`, where it is applied when the design matrix
  is built).

- na.action:

  A function for handling missing values, passed to the underlying
  fitting function (or to
  [`model.frame`](https://rdrr.io/r/stats/model.frame.html) for
  `"glmnet"` and `"xgboost"`). If not supplied, the default of the
  respective fitting function applies (usually
  [`na.omit`](https://rdrr.io/r/stats/na.fail.html)).

- fitfn:

  Character string naming the fitting method. One of `"lm"`, `"logit"`,
  `"poisson"`, `"quasipoisson"`, `"gamma"`, `"negbin"`, `"polr"`,
  `"lmrob"`, `"tobit"`, `"zeroinfl"`, `"multinom"`, `"nnet"`, `"rpart"`,
  `"C5.0"`, `"lda"`, `"qda"`, `"svm"`, `"naiveBayes"`, `"randomForest"`,
  `"glmnet"`, `"xgboost"`, `"coxph"`, `"weibull"`, `"exponential"`,
  `"lognormal"`, `"loglogistic"`, `"lmMixed"`, `"logitMixed"`,
  `"poissonMixed"`, `"negbinMixed"`, `"gammaMixed"`. If `NULL` (default)
  the method is chosen automatically.

## Value

An object of class `c("FitMod", <original class>)`. For `xgboost` and
`lme4` models, a list of class `c("FitMod", "FitMod.xgboost")` or
`c("FitMod", "FitMod.lme4")` wrapping the original model object in
`$model`. For `"glmnet"` and `"xgboost"` the result additionally stores
`terms`, `xlev` and `x_train`, so that
[`predict()`](https://rdrr.io/r/stats/predict.html) can rebuild design
matrices for new data with the factor levels of the training data.

## Details

Automatic method selection uses the following heuristic: a dichotomous
response (exactly two distinct values, factor/logical or numeric coded
as 0/1) is fitted with `"logit"`, an ordered factor with `"polr"`, an
unordered factor with `"multinom"`, a non-negative integer response with
`"poisson"`, and any other numeric response with `"lm"`. Note that
integer storage does not necessarily mean count data – data import
functions often return integer columns for metric variables. The chosen
method is always reported via
[`message()`](https://rdrr.io/r/base/message.html); supply `fitfn`
explicitly to override the heuristic.

## See also

Other modelling:
[`plot.FitMod()`](https://andrisignorell.github.io/alloy/reference/plot.FitMod.md),
[`predict.FitMod()`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md),
[`predictors()`](https://andrisignorell.github.io/alloy/reference/predictors.md),
[`print.FitMod()`](https://andrisignorell.github.io/alloy/reference/print.FitMod.md)

## Examples

``` r
# Auto-detection: numeric response -> lm
fitMod(Sepal.Length ~ ., data = iris)
#> fitMod: using fitfn = 'lm'
#> 
#> Call:
#> stats::lm(formula = Sepal.Length ~ ., data = iris)
#> 
#> Coefficients:
#>                         estimate  95%-lci     uci    p-val     
#> (Intercept)                2.171    1.623   2.720  < 0.001  ***
#> Sepal.Width                0.496    0.327   0.665  < 0.001  ***
#> Petal.Length               0.829    0.695   0.964  < 0.001  ***
#> Petal.Width               -0.315   -0.611  -0.019    0.039  *  
#> Species  (ref: setosa)         .        .       .    0.010  *  
#> Species versicolor        -0.724   -1.194  -0.253    0.003  ** 
#> Species virginica         -1.023   -1.678  -0.369    0.003  ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 150 (0)   R²/R²adj: 0.867/0.863
#> 

# factor response -> multinom
if (requireNamespace("nnet", quietly = TRUE)) {
  fitMod(Species ~ ., data = iris)
}
#> fitMod: using fitfn = 'multinom'
#> 
#> Call:
#> nnet::multinom(formula = Species ~ ., data = iris, model = TRUE, 
#>     maxit = 500, trace = FALSE)
#> 
#> Coefficients:
#> (Species == setosa is the base outcome)
#> 
#>                   estimate   95%-lci      uci   pval     
#>       versicolor                                         
#>      (Intercept)    18.408   -25.895   62.712  0.415     
#>     Sepal.Length    -6.082   -81.732   69.568  0.875     
#>      Sepal.Width    -9.397   -88.526   69.733  0.816     
#>     Petal.Length    16.170  -197.543  229.884  0.882     
#>      Petal.Width    -2.058  -120.541  116.424  0.973     
#>       virginica                                          
#>      (Intercept)   -24.230   -70.515   22.055  0.305     
#>     Sepal.Length    -8.547   -84.232   67.137  0.825     
#>      Sepal.Width   -16.077   -95.526   63.371  0.692     
#>     Petal.Length    25.600  -188.385  239.584  0.815     
#>      Petal.Width    16.227  -102.876  135.331  0.789     
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 150 (0)   Pseudo R² (McFadden): 0.964   AIC: 31.899
#> 

# Explicit method
if (requireNamespace("rpart", quietly = TRUE)) {
  fitMod(Species ~ ., data = iris, fitfn = "rpart")
}
#> 
#> Decision Tree
#> 
#> Call:
#> rpart::rpart(formula = Species ~ ., data = iris, model = TRUE, 
#>     y = TRUE)
#> 
#> Variable importance:
#>  Petal.Width Petal.Length Sepal.Length  Sepal.Width 
#>      100.000       91.430       60.803       40.478 
#> 
#> Confusion matrix (training):
#>             Reference
#> Prediction   setosa versicolor virginica
#>   setosa         50          0         0
#>   versicolor      0         49         5
#>   virginica       0          1        45
#> 
#> Accuracy: 0.960   Kappa: 0.940
#> 
#> Obs: 150
#> 

# Mixed models
if (requireNamespace("lme4", quietly = TRUE)) {
  fitMod(Reaction ~ Days + (1 | Subject), lme4::sleepstudy,
         fitfn = "lmMixed")
}
#> 
#> Linear mixed model
#> 
#> Call:
#> fitMod(formula = Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy, 
#>     fitfn = "lmMixed")
#> 
#> Fixed effects:
#>              estimate  95%-lci      uci    p-val     
#> (Intercept)   251.405  232.302  270.508  < 0.001  ***
#> Days           10.467    8.891   12.044  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Random effects:
#>  Groups   Variance Std.Dev
#>  Subject  1378.179 37.124 
#>  Residual 960.457  30.991 
#> 
#> ICC: 0.589
#> 
#> Obs: 180   Groups: Subject: 18
#> Log-lik: -893.233   AIC: 1794.465
#> 
```
