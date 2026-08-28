# Compare multiple statistical models

Creates a unified comparison object for multiple fitted models by
extracting coefficients, confidence intervals, and model statistics.

## Usage

``` r
tMod(..., FUN = NULL, order = NA, verbose = FALSE)

# S3 method for class 'TMod'
print(x, digits = 3, naForm = "-", verbose = NULL, ...)

# S3 method for class 'TMod'
plot(x, terms = NULL, intercept = FALSE, ...)
```

## Arguments

- ...:

  Fitted model objects

- FUN:

  Formatting function applied to coefficients

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

## Details

The function standardizes model output across different model classes
(e.g. `lm`, `glm`, `coxph`, `gam`, `lmer`) using S3 methods implemented
via [`tmodSummary()`](tmodSummary.md). This enables direct comparison of
model coefficients and fit statistics in tabular and graphical form.

Model names are automatically derived from the call. If unavailable,
default names are assigned.

## See also

[`tmodSummary`](tmodSummary.md)

Other model.comparison: [`tmodSummary()`](tmodSummary.md)

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
```
