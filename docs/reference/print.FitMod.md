# Print method for FitMod objects

Displays a formatted summary of a fitted model of class `"FitMod"`, with
confidence intervals, p-values, reference category headers for factor
predictors, and model fit statistics. The output style is consistent
across all supported model types and loosely follows Stata conventions.

## Usage

``` r
# S3 method for class 'FitMod'
print(
  x,
  digits = 3,
  pdigits = 3,
  conf.level = 0.95,
  output = NULL,
  useProfile = FALSE,
  vcov = NULL,
  ...
)
```

## Arguments

- x:

  A fitted model of class `"FitMod"`.

- digits:

  Integer. Number of significant digits for estimates and confidence
  intervals. Default is `3`.

- pdigits:

  Integer. Number of digits for p-values. Default is `3`.

- conf.level:

  Numeric scalar in \\(0, 1)\\. Confidence level for intervals. Default
  is `0.95`.

- output:

  Character string controlling the scale of the coefficient table.
  Supported values depend on the model type:

  `"coef"`

  :   Raw coefficients (default for `lm`, `glm`, `lmrob`, `polr`,
      parametric survival).

  `"or"`

  :   Odds ratios - `exp(coef)` - for logistic and ordinal models.

  `"irr"`

  :   Incidence rate ratios - `exp(coef)` - for Poisson and negative
      binomial models.

  `"hr"`

  :   Hazard ratios (default for `coxph`).

  `"lhr"`

  :   Log hazard ratios for `coxph`.

  `"tr"`

  :   Time ratios - `exp(coef)` - (default for parametric survival
      models).

  `"genuine"`

  :   Passes through to the original
      [`summary()`](https://rdrr.io/r/base/summary.html) /
      [`print()`](https://rdrr.io/r/base/print.html) of the underlying
      model object.

  If `NULL` (default), an appropriate value is chosen automatically
  based on the model class.

- useProfile:

  Logical. If `TRUE` and the model is a `glm`, profile-likelihood
  confidence intervals are computed via
  [`confint`](https://rdrr.io/r/stats/confint.html). Otherwise (default)
  Wald intervals via
  [`confint.default`](https://rdrr.io/r/stats/confint.html) are used.
  Ignored for non-GLM models.

- vcov:

  Character string specifying the type of heteroscedasticity-consistent
  covariance matrix to use for standard errors, e.g. `"HC3"`
  (recommended), `"HC0"`, `"HC1"`. Passed to
  [`vcovHC`](https://zeileis.codeberg.page/sandwich/reference/vcovHC.html).
  Supported for `lm`, `glm`, and `lmrob` models; ignored with a message
  for all others. If `NULL` (default) the model's own standard errors
  are used.

- ...:

  Further arguments passed to the underlying print helper.

## Value

Invisibly returns the result of `summary(x)`.

## Details

Factor predictors are displayed with a header row showing the reference
category and an overall p-value from
[`drop1`](https://rdrr.io/r/stats/add1.html). Dummy-coded rows are
indented below the header.

For negative binomial models an additional overdispersion block is
printed showing the parameter \\\alpha = 1/\theta\\ (Stata convention)
with a one-sided likelihood-ratio test against the Poisson model.

For quasi-Poisson and quasi-binomial models, pseudo-R\\^2\\ and AIC are
not available and a note is displayed instead.

## See also

[`fitMod`](https://andrisignorell.github.io/alloy/reference/fitMod.md),
[`predict.FitMod`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md)

Other modelling:
[`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md),
[`plot.FitMod()`](https://andrisignorell.github.io/alloy/reference/plot.FitMod.md),
[`predict.FitMod()`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md),
[`predictors()`](https://andrisignorell.github.io/alloy/reference/predictors.md)

## Examples

``` r
fitLm <- fitMod(Fertility ~ ., swiss)
#> fitMod: using fitfn = 'lm'
print(fitLm)
#> 
#> Call:
#> stats::lm(formula = Fertility ~ ., data = swiss)
#> 
#> Coefficients:
#>                   estimate  95%-lci     uci    p-val     
#> (Intercept)         66.915   45.932  87.899  < 0.001  ***
#> Agriculture         -0.172   -0.310  -0.034    0.019  *  
#> Examination         -0.258   -0.756   0.240    0.315     
#> Education           -0.871   -1.230  -0.512  < 0.001  ***
#> Catholic             0.104    0.035   0.173    0.005  ** 
#> Infant.Mortality     1.077    0.329   1.825    0.007  ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 47 (0)    R²/R²adj: 0.707/0.671
#> 
print(fitLm, vcov = "HC3")
#> 
#> Call:
#> stats::lm(formula = Fertility ~ ., data = swiss)
#> 
#> Coefficients:
#>                   estimate  95%-lci     uci    p-val     
#> (Intercept)         66.915   44.341  89.489  < 0.001  ***
#> Agriculture         -0.172   -0.312  -0.032    0.021  *  
#> Examination         -0.258   -0.794   0.278    0.351     
#> Education           -0.871   -1.283  -0.459  < 0.001  ***
#> Catholic             0.104    0.040   0.168    0.003  ** 
#> Infant.Mortality     1.077    0.178   1.976    0.024  *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 47 (0)    R²/R²adj: 0.707/0.671
#> 

fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
print(fitLogit)
#> 
#> Call:
#> stats::glm(formula = admit ~ gre + gpa + rank, family = "binomial", 
#>     data = Admit)
#> 
#> Coefficients:
#>                 estimate  95%-lci     uci    p-val     
#> (Intercept)       -3.990   -6.224  -1.756  < 0.001  ***
#> gre                0.002    0.000   0.004    0.038  *  
#> gpa                0.804    0.154   1.454    0.015  *  
#> rank  (ref: 1)         .        .       .  < 0.001  ***
#> rank 2            -0.675   -1.296  -0.055    0.033  *  
#> rank 3            -1.340   -2.017  -0.663  < 0.001  ***
#> rank 4            -1.551   -2.370  -0.733  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 400 (0)   Pseudo R² (McFadden): 0.083   AIC: 470.517
#> 
print(fitLogit, output = "or")
#> 
#> Call:
#> stats::glm(formula = admit ~ gre + gpa + rank, family = "binomial", 
#>     data = Admit)
#> 
#> Odds Ratios:
#>                    OR  95%-lci    uci    p-val     
#> (Intercept)     0.019    0.002  0.173  < 0.001  ***
#> gre             1.002    1.000  1.004    0.038  *  
#> gpa             2.235    1.166  4.282    0.015  *  
#> rank  (ref: 1)      .        .      .  < 0.001  ***
#> rank 2          0.509    0.274  0.946    0.033  *  
#> rank 3          0.262    0.133  0.515  < 0.001  ***
#> rank 4          0.212    0.093  0.481  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 400 (0)   Pseudo R² (McFadden): 0.083   AIC: 470.517
#> 
print(fitLogit, output = "or", vcov = "HC3")
#> 
#> Call:
#> stats::glm(formula = admit ~ gre + gpa + rank, family = "binomial", 
#>     data = Admit)
#> 
#> Odds Ratios:
#>                    OR  95%-lci    uci    p-val     
#> (Intercept)     0.019    0.002  0.179  < 0.001  ***
#> gre             1.002    1.000  1.004    0.044  *  
#> gpa             2.235    1.122  4.450    0.022  *  
#> rank  (ref: 1)      .        .      .  < 0.001  ***
#> rank 2          0.509    0.272  0.954    0.035  *  
#> rank 3          0.262    0.132  0.521  < 0.001  ***
#> rank 4          0.212    0.092  0.487  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 400 (0)   Pseudo R² (McFadden): 0.083   AIC: 470.517
#> 

fitPois <- fitMod(daysabs ~ mathnce + langnce + gender,
                  Lahigh, fitfn = "poisson")
print(fitPois, output = "irr")
#> 
#> Call:
#> stats::glm(formula = daysabs ~ mathnce + langnce + gender, family = "poisson", 
#>     data = Lahigh)
#> 
#> Incidence Rate Ratios:
#>                           IRR  95%-lci     uci    p-val     
#> (Intercept)            14.697   12.747  16.946  < 0.001  ***
#> mathnce                 0.996    0.993   1.000    0.053  .  
#> langnce                 0.988    0.984   0.991  < 0.001  ***
#> gender  (ref: female)       .        .       .  < 0.001  ***
#> gender male             0.670    0.609   0.736  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 316 (0)   Pseudo R² (McFadden): 0.054   AIC: 3103.942
#> 

fitCox <- fitMod(Surv(foltime, folstatus) ~ gender, Whas100,
                 fitfn = "coxph")
print(fitCox)
#> 
#> Call:
#> survival::coxph(formula = Surv(foltime, folstatus) ~ gender, 
#>     data = Whas100, model = TRUE, x = TRUE)
#> 
#> Hazard Ratios:
#>            HR  95%-lci    uci  p-val     
#> gender  1.742    1.001  3.029  0.049  *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 100 (0)   Events: 51   Concordance: 0.565
#> Log-lik: -207.225   LR χ²(1): 3.746   p: 0.053
#> 
print(fitCox, output = "lhr")
#> 
#> Call:
#> survival::coxph(formula = Surv(foltime, folstatus) ~ gender, 
#>     data = Whas100, model = TRUE, x = TRUE)
#> 
#> Log Hazard Ratios:
#>       log HR  95%-lci    uci  p-val     
#> [1,]   0.555    0.001  1.108  0.049  *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 100 (0)   Events: 51   Concordance: 0.565
#> Log-lik: -207.225   LR χ²(1): 3.746   p: 0.053
#> 

fitWei <- fitMod(Surv(foltime, folstatus) ~ gender + age, Whas100,
                 fitfn = "weibull")
print(fitWei)
#> 
#> Weibull AFT model
#> 
#> Call:
#> survival::survreg(formula = Surv(foltime, folstatus) ~ gender + 
#>     age, data = Whas100, dist = "weibull")
#> 
#> Time Ratios:
#>                       TR      95%-lci          uci    p-val     
#> (Intercept)   187314.776    16083.711  2181513.001  < 0.001  ***
#> gender             0.746        0.350        1.591    0.449     
#> age                0.946        0.915        0.978    0.001  ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Scale: 1.298   Shape (1/scale): 0.771  → decreasing hazard
#> 
#> Obs (NAs): 100 (0)   Events: 51
#> Log-lik: -446.306   LR χ²(3): 17.890   p: < 0.001
#> 
print(fitWei, output = "coef")
#> 
#> Weibull AFT model
#> 
#> Call:
#> survival::survreg(formula = Surv(foltime, folstatus) ~ gender + 
#>     age, data = Whas100, dist = "weibull")
#> 
#> Time Ratios:
#>              log TR  95%-lci     uci    p-val     
#> (Intercept)  12.141    9.686  14.596  < 0.001  ***
#> gender       -0.293   -1.051   0.465    0.449     
#> age          -0.056   -0.089  -0.022    0.001  ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Scale: 1.298   Shape (1/scale): 0.771  → decreasing hazard
#> 
#> Obs (NAs): 100 (0)   Events: 51
#> Log-lik: -446.306   LR χ²(3): 17.890   p: < 0.001
#> 
print(fitWei, output = "genuine")
#> 
#> Call:
#> survival::survreg(formula = Surv(foltime, folstatus) ~ gender + 
#>     age, data = Whas100, dist = "weibull")
#>               Value Std. Error     z      p
#> (Intercept) 12.1405     1.2526  9.69 <2e-16
#> gender      -0.2930     0.3866 -0.76  0.449
#> age         -0.0558     0.0170 -3.28  0.001
#> Log(scale)   0.2606     0.1258  2.07  0.038
#> 
#> Scale= 1.3 
#> 
#> Weibull distribution
#> Loglik(model)= -446.3   Loglik(intercept only)= -455.3
#>  Chisq= 17.89 on 2 degrees of freedom, p= 0.00013 
#> Number of Newton-Raphson Iterations: 5 
#> n= 100 
#> 
```
