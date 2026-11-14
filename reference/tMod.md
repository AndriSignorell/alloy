# Compare multiple statistical models

Creates a unified comparison object for multiple fitted models by
extracting coefficients, confidence intervals, and model statistics.

## Usage

``` r
tMod(..., FUN = NULL, order = NA, verbose = FALSE)

# S3 method for class 'TMod'
print(x, digits = 3, naForm = "-", verbose = NULL, ...)

# S3 method for class 'TMod'
format(x, digits = 3, naForm = "-", verbose = NULL, ...)

# S3 method for class 'TMod'
plot(x, terms = NULL, intercept = FALSE, ...)
```

## Arguments

- ...:

  Fitted model objects

- FUN:

  Formatting function applied to the coefficients of each model. It is
  called as `FUN(est, se, tval, pval, lci, uci)` with six numeric
  vectors of equal length (one element per coefficient: estimate,
  standard error, test statistic, p-value, lower and upper confidence
  limit) and must return a vector of the same length, typically
  character. The arguments are matched by position. The default (`NULL`)
  shows the estimate followed by significance stars.

- order:

  Optional ordering of models

- verbose:

  Logical; if `TRUE`, show extended statistics

- x:

  A `"TMod"` object

- digits:

  Number of digits for printing

- naForm:

  String used for missing values

- terms:

  Optional character vector specifying which model terms should be
  displayed in the plot. If `NULL`, all terms are shown.

- intercept:

  Logical; if `FALSE` (default), intercept terms are excluded from the
  plot.

## Value

An object of class `"TMod"` with components:

- `m`: formatted coefficient table

- `mm`: model statistics

- `mall`: array of estimates and confidence intervals

- `terms`: mapping of model terms

[`format()`](https://rdrr.io/r/base/format.html): a list with the
formatted coefficient table `coef` and the model statistics `stats`,
both data.frames of strings with identical column names. Used by
[`print()`](https://rdrr.io/r/base/print.html) and by `pons::toWrd()`.

## Details

The function standardizes model output across different model classes
(e.g. `lm`, `glm`, `betareg`, `coxph`, `gam`, `lmer`) using S3 methods
implemented via
[`tModSummary()`](https://andrisignorell.github.io/alloy/reference/tmodSummary.md).
This enables direct comparison of model coefficients and fit statistics
in tabular and graphical form.

Model names are automatically derived from the call. If unavailable,
default names are assigned.

Models fitted with
[`fitMod`](https://andrisignorell.github.io/alloy/reference/fitMod.md)
are accepted and compared as the model object of the underlying fitting
function.

## See also

[`tModSummary`](https://andrisignorell.github.io/alloy/reference/tmodSummary.md)

Other model.comparison:
[`tModSummary()`](https://andrisignorell.github.io/alloy/reference/tmodSummary.md)

## Examples

``` r
# --- Linear models ---
m1 <- lm(mpg ~ wt, data = mtcars)
m2 <- lm(mpg ~ wt + hp, data = mtcars)
tMod(m1, m2)
#>             coef            m1                m2    
#> 1    (Intercept)        37.285 ***        37.227 ***
#> 2             wt        -5.344 ***        -3.878 ***
#> 3             hp          -               -0.032 ** 
#> 4            ---                                    
#> 5  adj.r.squared         0.745             0.815    
#> 6            AIC       166.029           156.652    
#> 7              N         32                32       
#> 8            NAs          0                 0       
#> 9         n vars          1                 2       
#> 10        n coef          2                 3       
#> 11           MAE         2.341             1.901    
#> 12          RMSE         2.949             2.469    

# --- Generalized linear models ---
g1 <- glm(am ~ wt, data = mtcars, family = binomial)
g2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
tMod(g1, g2)
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#>           coef           g1               g2    
#> 1  (Intercept)        12.040 **        18.866 * 
#> 2           wt        -4.024 **        -8.083 **
#> 3           hp          -               0.036 * 
#> 4          ---                                  
#> 5          AIC        23.176           16.059   
#> 6            N         32               32      
#> 7          NAs          0                0      
#> 8       n vars          1                2      
#> 9       n coef          2                3      
#> 10    McFadden         0.556            0.767   

# --- Custom cell format: estimate with confidence interval ---
# FUN receives the vectors est, se, tval, pval, lci, uci
fci <- function(est, se, tval, pval, lci, uci)
  pharos::fmCI(cbind(est, lci, uci), template = "%s (%s, %s)", digits = 3)
tMod(m1, m2, FUN = fci)
#>             coef                                      m1    
#> 1    (Intercept)                     37.285 (33.450, 41.120)
#> 2             wt                     -5.344 (-6.486, -4.203)
#> 3             hp                       -                    
#> 4            ---                                            
#> 5  adj.r.squared                      0.745                 
#> 6            AIC                    166.029                 
#> 7              N                      32                    
#> 8            NAs                       0                    
#> 9         n vars                       1                    
#> 10        n coef                       2                    
#> 11           MAE                      2.341                 
#> 12          RMSE                      2.949                 
#>                                         m2    
#> 1                      37.227 (33.957, 40.497)
#> 2                      -3.878 (-5.172, -2.584)
#> 3                      -0.032 (-0.050, -0.013)
#> 4                                             
#> 5                       0.815                 
#> 6                     156.652                 
#> 7                       32                    
#> 8                        0                    
#> 9                        2                    
#> 10                       3                    
#> 11                      1.901                 
#> 12                      2.469                 

# odds ratios with confidence interval and significance stars
forr <- function(est, se, tval, pval, lci, uci)
  paste(pharos::fmCI(exp(cbind(est, lci, uci)),
                     template = "%s (%s, %s)", digits = 2),
        pharos::fm(pval, fmt = "*"))
tMod(g1, g2, FUN = forr)
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#>           coef                                                   g1    
#> 1  (Intercept)                          169459.57 (183.79, 1.83e+10) **
#> 2           wt                               0.02 (0.00, 0.16) **      
#> 3           hp                                -                        
#> 4          ---                                                         
#> 5          AIC                              23.176                     
#> 6            N                               32                        
#> 7          NAs                                0                        
#> 8       n vars                                1                        
#> 9       n coef                                2                        
#> 10    McFadden                               0.556                     
#>                                                          g2    
#> 1                                1.56e+08 (4342.77, 4.08e+17) *
#> 2                                0.00 (3.36e-08, 0.02) **      
#> 3                                1.04 (1.01, 1.09) *           
#> 4                                                              
#> 5                               16.059                         
#> 6                                32                            
#> 7                                 0                            
#> 8                                 2                            
#> 9                                 3                            
#> 10                               0.767                         

# --- Survival models ---
if(requireNamespace("survival", quietly = TRUE)){
  library(survival)
  s1 <- coxph(Surv(time, status) ~ age, data = lung)
  s2 <- coxph(Surv(time, status) ~ age + sex, data = lung)
  tMod(s1, s2)
}
#>     coef         s1              s2    
#> 1    age        1.019 *        1.017 . 
#> 2    sex         -             0.599 **
#> 3    ---                               
#> 4    AIC     1497.579       1489.696   
#> 5      N       228            228      
#> 6    NAs         0              0      
#> 7 n vars         1              2      
#> 8 n coef         1              2      

# --- Models fitted with fitMod() ---
f1 <- fitMod(am ~ wt, data = mtcars, engine = "logit")
f2 <- fitMod(am ~ wt + hp, data = mtcars, engine = "logit")
tMod(f1, f2)
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#>           coef           f1               f2    
#> 1  (Intercept)        12.040 **        18.866 * 
#> 2           wt        -4.024 **        -8.083 **
#> 3           hp          -               0.036 * 
#> 4          ---                                  
#> 5          AIC        23.176           16.059   
#> 6            N         32               32      
#> 7          NAs          0                0      
#> 8       n vars          1                2      
#> 9       n coef          2                3      
#> 10    McFadden         0.556            0.767   
```
