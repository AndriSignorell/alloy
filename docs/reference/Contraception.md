# Contraceptive use in Bangladesh

Data on contraceptive use by women in urban and rural areas of
Bangladesh, from the 1988 Bangladesh Fertility Survey. Commonly used to
illustrate mixed-effects logistic regression with a district-level
random effect.

## Usage

``` r
Contraception
```

## Format

A data frame with 1934 rows and 6 variables:

- woman:

  Factor: identifying code for each woman.

- district:

  Factor: identifying code for each district (used as grouping variable
  in mixed models).

- use:

  Factor: contraceptive use at time of survey (`"N"` = no, `"Y"` = yes).

- livch:

  Ordered factor: number of living children at time of survey. Levels
  are `0`, `1`, `2`, `3+`.

- age:

  Numeric: age of woman at time of survey (years), centred around the
  mean.

- urban:

  Factor: type of region of residence (`"urban"`, `"rural"`).

## Source

<https://www.bristol.ac.uk/cmm/learning/mmsoftware/data-rev.html>

## References

Huq, N. M., and Cleland, J. (1990). *Bangladesh Fertility Survey 1989
(Main Report)*. Dhaka: National Institute of Population Research and
Training.

## Examples

``` r
fitLogitMixed <- fitMod(use ~ age + urban + (1 | district),
                        data = Contraception, fitfn = "logitMixed")
fitLogitMixed
#> 
#> Mixed logistic regression
#> 
#> Call:
#> fitMod(formula = use ~ age + urban + (1 | district), data = Contraception, 
#>     fitfn = "logitMixed")
#> 
#> Fixed effects:
#>              estimate  95%-lci     uci    p-val     
#> (Intercept)    -0.703   -0.870  -0.536  < 0.001  ***
#> age             0.009   -0.002   0.020    0.095  .  
#> urbanY          0.653    0.427   0.880  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Random effects:
#>  Groups   Variance Std.Dev
#>  district 0.192    0.438  
#> 
#> ICC: 0.055
#> 
#> Obs: 1934   Groups: district: 60
#> Log-lik: -1250.196   AIC: 2508.392
#> 
fitLogitMixed |> print(output = "or")
#> 
#> Mixed logistic regression
#> 
#> Call:
#> fitMod(formula = use ~ age + urban + (1 | district), data = Contraception, 
#>     fitfn = "logitMixed")
#> 
#> Fixed effects (Odds Ratios):
#>                 OR  95%-lci    uci    p-val     
#> (Intercept)  0.495    0.419  0.585  < 0.001  ***
#> age          1.009    0.998  1.020    0.095  .  
#> urbanY       1.922    1.532  2.410  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Random effects:
#>  Groups   Variance Std.Dev
#>  district 0.192    0.438  
#> 
#> ICC: 0.055
#> 
#> Obs: 1934   Groups: district: 60
#> Log-lik: -1250.196   AIC: 2508.392
#> 
```
