# fitMod: A Unified Interface for Statistical Models

[`fitMod()`](../reference/fitMod.md) provides a unified interface for
fitting and displaying a wide range of statistical models. The syntax is
always the same:

``` r

fitMod(formula, data, fitfn = "...")
```

For background on the statistical methods, the [UCLA Statistical Methods
and Data Analytics](https://stats.oarc.ucla.edu) site is an excellent
reference - we link to the relevant pages throughout.

------------------------------------------------------------------------

## Datasets

The following code creates all datasets used in this vignette. Run once,
then save with
[`usethis::use_data()`](https://usethis.r-lib.org/reference/use_data.html).

``` r

library(haven)
library(foreign)

# --- Lahigh: absenteeism in Los Angeles high schools ---
Lahigh <- read.dta("https://stats.idre.ucla.edu/stat/data/lahigh.dta")
Lahigh$gender <- relevel(Lahigh$gender, ref = "male")
usethis::use_data(Lahigh)

# --- Admit: graduate school admissions ---
Admit <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
Admit$rank <- factor(Admit$rank)
usethis::use_data(Admit)

# --- Apt: tobit regression example ---
Apt <- haven::read_dta(
  "https://stats.idre.ucla.edu/stat/stata/dae/tobit.dta"
) |> toBaseR()
usethis::use_data(Apt)

# --- Ologit: ordinal logistic regression ---
Ologit <- haven::read_dta(
  "https://stats.idre.ucla.edu/stat/data/ologit.dta"
) |> toBaseR()
usethis::use_data(Ologit)

# --- IceCream: multinomial logistic regression ---
IceCream <- haven::read_sas(
  "https://stats.idre.ucla.edu/wp-content/uploads/2016/02/mlogit.sas7bdat"
)
names(IceCream) <- tolower(names(IceCream))
IceCream$ice_cream <- relevel(
  factor(IceCream$ice_cream,
         labels = c("chocolate", "vanilla", "strawberry")),
  ref = "vanilla"
)
usethis::use_data(IceCream)

# --- Whas100: Worcester Heart Attack Study ---
Whas100 <- haven::read_dta(
  "https://stats.idre.ucla.edu/stat/examples/asa2/whas100.dta"
) |> toBaseR()
Whas100$agex <- cutAge(Whas100$age, full = FALSE)
usethis::use_data(Whas100)

# --- Fish: zero-inflated count data ---
Fish <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
Fish <- within(Fish, {
  nofish   <- factor(nofish)
  livebait <- factor(livebait)
  camper   <- factor(camper)
})
usethis::use_data(Fish)

# --- BioChemists: biochemists publication data (from pscl) ---
data("bioChemists", package = "pscl")
BioChemists <- bioChemists
usethis::use_data(BioChemists)
```

------------------------------------------------------------------------

## 1. Continuous outcome

### Linear regression (`lm`, `lmrob`)

``` r

fitLm <- fitMod(Fertility ~ ., swiss)
#> fitMod: using fitfn = 'lm'
fitLm
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
```

Robust standard errors via `sandwich`:

``` r

fitLm |> print(vcov = "HC3")
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
```

Robust regression (MM-estimator):

``` r

fitLmrob <- fitMod(Fertility ~ ., swiss, fitfn = "lmrob")
fitLmrob
#> 
#> Call:
#> robustbase::lmrob(formula = Fertility ~ ., data = swiss)
#> 
#> Coefficients:
#>                   estimate  95%-lci     uci    p-val     
#> (Intercept)         65.642   46.287  84.997  < 0.001  ***
#> Agriculture         -0.192   -0.318  -0.066    0.005  ** 
#> Examination         -0.291   -0.781   0.199    0.252     
#> Education           -0.849   -1.243  -0.455  < 0.001  ***
#> Catholic             0.104    0.047   0.160  < 0.001  ***
#> Infant.Mortality     1.212    0.369   2.055    0.007  ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 47 (0)    R²/R²adj: 0.707/0.672   Scale: 6.596
```

### Gamma regression (`gamma`)

``` r

fitGamma <- fitMod(Sepal.Length ~ ., iris, fitfn = "gamma")
fitGamma
#> 
#> Call:
#> stats::glm(formula = Sepal.Length ~ ., family = Gamma(link = "log"), 
#>     data = iris)
#> 
#> Coefficients:
#>                         estimate  95%-lci     uci    p-val     
#> (Intercept)                1.112    1.019   1.206  < 0.001  ***
#> Sepal.Width                0.094    0.065   0.123  < 0.001  ***
#> Petal.Length               0.128    0.105   0.151  < 0.001  ***
#> Petal.Width               -0.049   -0.100   0.001    0.058  .  
#> Species  (ref: setosa)         .        .       .    0.063  .  
#> Species versicolor        -0.075   -0.155   0.005    0.070  .  
#> Species virginica         -0.122   -0.233  -0.011    0.033  *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 150 (0)   Pseudo R² (McFadden): 0.833   AIC: 74.954
```

### Tobit regression (`tobit`)

[UCLA reference](https://stats.oarc.ucla.edu/stata/dae/tobit-analysis/)

``` r

fitTobit <- fitMod(apt ~ read + math + prog, Apt, fitfn = "tobit")
fitTobit
#> 
#> Call:
#> AER::tobit(formula = apt ~ read + math + prog, data = Apt)
#> 
#> Coefficients:
#>                        estimate  95%-lci      uci    p-val     
#> (Intercept)             242.735  184.406  301.065  < 0.001  ***
#> read                      2.553    1.424    3.681  < 0.001  ***
#> math                      5.383    4.108    6.659  < 0.001  ***
#> prog  (ref: academic)         .        .        .  < 0.001  ***
#> prog general            -13.741  -36.469    8.988    0.236     
#> prog vocational         -48.835  -73.958  -23.712  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Scale: 61.592
#> 
#> Obs: 200  uncensored: 200
#> Log-lik: -1107.894   AIC: 2227.788
```

------------------------------------------------------------------------

## 2. Binary outcome

### Logistic regression (`logit`)

[UCLA reference](https://stats.oarc.ucla.edu/r/dae/logit-regression/)

``` r

fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
fitLogit
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
```

Odds Ratios:

``` r

fitLogit |> print(output = "or")
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
```

Robust SE:

``` r

fitLogit |> print(vcov = "HC3")
#> 
#> Call:
#> stats::glm(formula = admit ~ gre + gpa + rank, family = "binomial", 
#>     data = Admit)
#> 
#> Coefficients:
#>                 estimate  95%-lci     uci    p-val     
#> (Intercept)       -3.990   -6.260  -1.720  < 0.001  ***
#> gre                0.002    0.000   0.004    0.044  *  
#> gpa                0.804    0.115   1.493    0.022  *  
#> rank  (ref: 1)         .        .       .  < 0.001  ***
#> rank 2            -0.675   -1.303  -0.047    0.035  *  
#> rank 3            -1.340   -2.027  -0.653  < 0.001  ***
#> rank 4            -1.551   -2.384  -0.719  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 400 (0)   Pseudo R² (McFadden): 0.083   AIC: 470.517
fitLogit |> print(output = "or", vcov = "HC3")
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
```

Predicted probabilities:

``` r

head(predict(fitLogit))
#>           0         1
#> 1 0.8273735 0.1726265
#> 2 0.7078250 0.2921750
#> 3 0.2615918 0.7384082
#> 4 0.8216154 0.1783846
#> 5 0.8816461 0.1183539
#> 6 0.6300301 0.3699699
pseudoR2(fitLogit)
#>   McFadden 
#> 0.08292194
```

------------------------------------------------------------------------

## 3. Count outcome

### Poisson regression (`poisson`)

[UCLA
reference](https://stats.oarc.ucla.edu/stata/output/poisson-regression/)

``` r

fitPois <- fitMod(daysabs ~ mathnce + langnce + gender, 
                  data = Lahigh, fitfn = "poisson")
fitPois
#> 
#> Call:
#> stats::glm(formula = daysabs ~ mathnce + langnce + gender, family = "poisson", 
#>     data = Lahigh)
#> 
#> Coefficients:
#>                        estimate  95%-lci     uci    p-val     
#> (Intercept)               2.688    2.545   2.830  < 0.001  ***
#> mathnce                  -0.004   -0.007   0.000    0.053  .  
#> langnce                  -0.012   -0.016  -0.009  < 0.001  ***
#> gender  (ref: female)         .        .       .  < 0.001  ***
#> gender male              -0.401   -0.496  -0.306  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 316 (0)   Pseudo R² (McFadden): 0.054   AIC: 3103.942
fitPois |> print(output = "irr")
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
fitPois |> print(vcov = "HC0")
#> 
#> Call:
#> stats::glm(formula = daysabs ~ mathnce + langnce + gender, family = "poisson", 
#>     data = Lahigh)
#> 
#> Coefficients:
#>                        estimate  95%-lci     uci    p-val     
#> (Intercept)               2.688    2.261   3.115  < 0.001  ***
#> mathnce                  -0.004   -0.018   0.011    0.644     
#> langnce                  -0.012   -0.023  -0.002    0.022  *  
#> gender  (ref: female)         .        .       .  < 0.001  ***
#> gender male              -0.401   -0.674  -0.128    0.004  ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 316 (0)   Pseudo R² (McFadden): 0.054   AIC: 3103.942
```

### Quasi-Poisson (`quasipoisson`)

``` r

fitQpois <- fitMod(daysabs ~ mathnce + langnce + gender,
                   data = Lahigh, fitfn = "quasipoisson")
fitQpois
#> 
#> Call:
#> stats::glm(formula = daysabs ~ mathnce + langnce + gender, family = "quasipoisson", 
#>     data = Lahigh)
#> 
#> Coefficients:
#>                        estimate  95%-lci     uci    p-val     
#> (Intercept)               2.688    2.263   3.112  < 0.001  ***
#> mathnce                  -0.004   -0.014   0.007    0.517     
#> langnce                  -0.012   -0.023  -0.001    0.027  *  
#> gender  (ref: female)         .        .       .    0.005  ** 
#> gender male              -0.401   -0.684  -0.118    0.006  ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 316 (0)   Pseudo R²/AIC: not available (quasi model)
```

### Negative binomial (`negbin`)

[UCLA
reference](https://stats.oarc.ucla.edu/sas/output/negative-binomial-regression/)

``` r

fitNegbin <- fitMod(daysabs ~ mathnce + langnce + gender,
                    data = Lahigh, fitfn = "negbin")
fitNegbin
#> 
#> Call:
#> MASS::glm.nb(formula = daysabs ~ mathnce + langnce + gender, 
#>     data = Lahigh, init.theta = 0.7761669423, link = log)
#> 
#> Coefficients:
#>                        estimate  95%-lci     uci    p-val     
#> (Intercept)               2.716    2.257   3.175  < 0.001  ***
#> mathnce                  -0.002   -0.012   0.009    0.763     
#> langnce                  -0.014   -0.025  -0.004    0.008  ** 
#> gender  (ref: female)         .        .       .    0.002  ** 
#> gender male              -0.431   -0.705  -0.158    0.002  ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Overdispersion parameter (α = 1/θ):
#>   estimate  95%-lci    uci    p-val     
#>      1.288    1.047  1.530  < 0.001  ***
#> ---
#> 
#> Obs (NAs): 316 (0)   Pseudo R² (McFadden): 0.012   AIC: 1771.746
fitNegbin |> print(vcov = "HC3")
#> 
#> Call:
#> MASS::glm.nb(formula = daysabs ~ mathnce + langnce + gender, 
#>     data = Lahigh, init.theta = 0.7761669423, link = log)
#> 
#> Coefficients:
#>                        estimate  95%-lci     uci    p-val     
#> (Intercept)               2.716    2.251   3.181  < 0.001  ***
#> mathnce                  -0.002   -0.018   0.014    0.844     
#> langnce                  -0.014   -0.025  -0.004    0.009  ** 
#> gender  (ref: female)         .        .       .    0.002  ** 
#> gender male              -0.431   -0.719  -0.143    0.003  ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Overdispersion parameter (α = 1/θ):
#>   estimate  95%-lci    uci    p-val     
#>      1.288    1.047  1.530  < 0.001  ***
#> ---
#> 
#> Obs (NAs): 316 (0)   Pseudo R² (McFadden): 0.012   AIC: 1771.746
```

### Zero-inflated Poisson (`zeroinfl`)

[UCLA reference](https://stats.oarc.ucla.edu/r/dae/zip/)

``` r

fitZeroinfl <- fitMod(count ~ child + camper | persons,
                      data = Fish, fitfn = "zeroinfl")
fitZeroinfl
#> 
#> Call:
#> pscl::zeroinfl(formula = count ~ child + camper | persons, data = Fish)
#> 
#> Count model (poisson with log link):
#>              estimate  95%-lci     uci    p-val     
#> (Intercept)     1.598    1.430   1.766  < 0.001  ***
#> child          -1.043   -1.239  -0.847  < 0.001  ***
#> camper1         0.834    0.651   1.018  < 0.001  ***
#> 
#> Zero-inflation model (binomial with logit link):
#>              estimate  95%-lci     uci    p-val     
#> (Intercept)     1.297    0.565   2.030  < 0.001  ***
#> persons        -0.564   -0.884  -0.245  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 250 (0)   Log-lik: -1031.608   AIC: 2073.217
```

------------------------------------------------------------------------

## 4. Ordered / Nominal outcome

### Ordinal logistic regression (`polr`)

[UCLA reference](https://stats.oarc.ucla.edu/r/faq/ologit-coefficients/)

``` r

fitPolr <- fitMod(apply ~ pared, data = Ologit, fitfn = "polr")
fitPolr
#> 
#> Call:
#> MASS::polr(formula = apply ~ pared, data = Ologit, Hess = TRUE, 
#>     model = TRUE)
#> 
#> Coefficients:
#>          estimate  95%-lci    uci     pval     
#>   pared     1.127    0.611  1.644  < 0.001  ***
#> 
#> Thresholds:
#>                                estimate  95%-lci    uci
#>      unlikely|somewhat likely     0.377    0.161  0.593
#>   somewhat likely|very likely     2.452    2.094  2.810
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 400 (0)   Pseudo R² (McFadden): 0.025   AIC: 728.790
fitPolr |> print(output = "or")
#> 
#> Call:
#> MASS::polr(formula = apply ~ pared, data = Ologit, Hess = TRUE, 
#>     model = TRUE)
#> 
#> Odds Ratios:
#>          estimate  95%-lci    uci     pval     
#>   pared     3.088    1.843  5.175  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 400 (0)   Pseudo R² (McFadden): 0.025   AIC: 728.790
head(predict(fitPolr, output = "both"))
#>    unlikely somewhat likely very likely           class
#> 1 0.5931114       0.3275856  0.07930294        unlikely
#> 2 0.3206801       0.4692269  0.21009300 somewhat likely
#> 3 0.3206801       0.4692269  0.21009300 somewhat likely
#> 4 0.5931114       0.3275856  0.07930294        unlikely
#> 5 0.5931114       0.3275856  0.07930294        unlikely
#> 6 0.5931114       0.3275856  0.07930294        unlikely
```

### Multinomial logistic regression (`multinom`)

[UCLA
reference](https://stats.oarc.ucla.edu/stata/output/multinomial-logistic-regression/)

``` r

fitMult <- fitMod(ice_cream ~ video + puzzle + female,
                  data = IceCream, fitfn = "multinom")
fitMult
#> 
#> Call:
#> nnet::multinom(formula = ice_cream ~ video + puzzle + female, 
#>     data = IceCream, model = TRUE, maxit = 500, trace = FALSE)
#> 
#> Coefficients:
#> (ice_cream == vanilla is the base outcome)
#> 
#>                  estimate  95%-lci     uci     pval     
#>      chocolate                                          
#>     (Intercept)     1.912   -0.297   4.122    0.090  .  
#>           video    -0.024   -0.065   0.018    0.261     
#>          puzzle    -0.039   -0.077  -0.001    0.046  *  
#>          female     0.817    0.050   1.583    0.037  *  
#>      strawberry                                         
#>     (Intercept)    -4.057   -6.454  -1.660  < 0.001  ***
#>           video     0.023   -0.018   0.064    0.272     
#>          puzzle     0.043    0.004   0.082    0.031  *  
#>          female    -0.033   -0.719   0.653    0.925     
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 200 (0)   Pseudo R² (McFadden): 0.079   AIC: 404.070
head(predict(fitMult, output = "both"))
#>     vanilla  chocolate strawberry      class
#> 1 0.5457004 0.13270412  0.3215955    vanilla
#> 2 0.4347110 0.14041387  0.4248751    vanilla
#> 3 0.5668390 0.29264026  0.1405208    vanilla
#> 4 0.5355472 0.11755137  0.3469015    vanilla
#> 5 0.5040795 0.09107643  0.4048441    vanilla
#> 6 0.4645151 0.06630794  0.4691769 strawberry
```

------------------------------------------------------------------------

## 5. Survival outcome

### Cox proportional hazards (`coxph`)

[UCLA
reference](https://stats.oarc.ucla.edu/stata/examples/asa2/applied-survival-analysis-by-hosmer-lemeshow-and-maychapter-4-interpretation-of-a-fitted-proportional-hazards-regression-model/)

``` r

fitCox <- fitMod(Surv(foltime, folstatus) ~ gender + agex,
                 data = Whas100, fitfn = "coxph")
fitCox
#> 
#> Call:
#> survival::coxph(formula = Surv(foltime, folstatus) ~ gender + 
#>     agex, data = Whas100, model = TRUE, x = TRUE)
#> 
#> Hazard Ratios:
#>             HR  95%-lci     uci  p-val     
#> gender   1.238    0.679   2.257  0.486     
#> agex.L   7.243    1.725  30.407  0.007  ** 
#> agex.Q   1.748    0.465   6.572  0.409     
#> agex.C   1.002    0.305   3.295  0.997     
#> agex^4   1.453    0.501   4.215  0.491     
#> agex^5   0.746    0.308   1.808  0.516     
#> agex^6   1.985    0.923   4.269  0.079  .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 100 (0)   Events: 51   Concordance: 0.663
#> Log-lik: -199.167   LR χ²(7): 19.861   p: 0.006
head(predict(fitCox))
#> [1] 0.5980452 2.1009850 1.4904319 2.1009850 1.4904319 2.1009850
```

### Parametric survival models (`weibull`, `exponential`, `lognormal`, `loglogistic`)

Parametric survival models use the Accelerated Failure Time (AFT)
framework. Coefficients are displayed as Time Ratios (`TR = exp(coef)`):
a value greater than 1 indicates longer survival, less than 1 shorter
survival. The shape parameter determines the hazard function: shape \> 1
means increasing hazard, shape \< 1 decreasing hazard.

``` r

fitWeibull <- fitMod(Surv(foltime, folstatus) ~ gender + age + bmi,
                     data = Whas100, fitfn = "weibull")
fitWeibull
#> 
#> Weibull AFT model
#> 
#> Call:
#> survival::survreg(formula = Surv(foltime, folstatus) ~ gender + 
#>     age + bmi, data = Whas100, dist = "weibull")
#> 
#> Time Ratios:
#>                      TR     95%-lci         uci    p-val     
#> (Intercept)    7882.458     193.105  321758.777  < 0.001  ***
#> gender            0.763       0.361       1.613    0.479     
#> age               0.956       0.925       0.988    0.007  ** 
#> bmi               1.092       0.999       1.194    0.053  .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Scale: 1.264   Shape (1/scale): 0.791  → decreasing hazard
#> 
#> Obs (NAs): 100 (0)   Events: 51
#> Log-lik: -444.388   LR χ²(4): 21.726   p: < 0.001
fitWeibull |> print(output = "coef")   # log Time Ratios
#> 
#> Weibull AFT model
#> 
#> Call:
#> survival::survreg(formula = Surv(foltime, folstatus) ~ gender + 
#>     age + bmi, data = Whas100, dist = "weibull")
#> 
#> Time Ratios:
#>              log TR  95%-lci     uci    p-val     
#> (Intercept)   8.972    5.263  12.682  < 0.001  ***
#> gender       -0.270   -1.019   0.478    0.479     
#> age          -0.045   -0.077  -0.012    0.007  ** 
#> bmi           0.088   -0.001   0.177    0.053  .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Scale: 1.264   Shape (1/scale): 0.791  → decreasing hazard
#> 
#> Obs (NAs): 100 (0)   Events: 51
#> Log-lik: -444.388   LR χ²(4): 21.726   p: < 0.001
head(predict(fitWeibull))
#> [1] 6830.5301  863.6433 2932.4140 1064.7340 3597.3468 1578.2082
```

------------------------------------------------------------------------

## 6. Mixed models

Mixed models extend standard regression by adding random effects to
account for clustered or longitudinal data. The random effects structure
is specified in the formula using `lme4` notation: `(1 | group)` for a
random intercept, `(1 + x | group)` for random intercept and slope.

[UCLA reference - linear mixed
models](https://stats.oarc.ucla.edu/r/dae/linear-mixed-effects-models-using-r/)  
[UCLA reference - mixed logistic
regression](https://stats.oarc.ucla.edu/r/dae/mixed-effects-logistic-regression/)

### Linear mixed model (`lmMixed`)

``` r

fitLmm <- fitMod(Reaction ~ Days + (1 | Subject),
                 data = lme4::sleepstudy, fitfn = "lmMixed")
fitLmm
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
head(predict(fitLmm))
#>        1        2        3        4        5        6 
#> 292.1888 302.6561 313.1234 323.5907 334.0580 344.5252
```

### Mixed logistic regression (`logitMixed`)

[UCLA
reference](https://stats.oarc.ucla.edu/r/dae/mixed-effects-logistic-regression/)

``` r

fitLogitMixed <- fitMod(use ~ age + urban + (1 | district),
                        data = alloy::Contraception, fitfn = "logitMixed")
fitLogitMixed
#> 
#> Mixed logistic regression
#> 
#> Call:
#> fitMod(formula = use ~ age + urban + (1 | district), data = alloy::Contraception, 
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
fitLogitMixed |> print(output = "or")
#> 
#> Mixed logistic regression
#> 
#> Call:
#> fitMod(formula = use ~ age + urban + (1 | district), data = alloy::Contraception, 
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
head(predict(fitLogitMixed, output = "both"))
#>           N         Y class
#> 1 0.6193328 0.3806672     N
#> 2 0.6688867 0.3311133     N
#> 3 0.6547595 0.3452405     N
#> 4 0.6403537 0.3596463     N
#> 5 0.6846662 0.3153338     N
#> 6 0.6807593 0.3192407     N
```

### Mixed Poisson regression (`poissonMixed`)

``` r

fitPoisMixed <- fitMod(incidence ~ period + (1 | herd),
                       data = lme4::cbpp, fitfn = "poissonMixed")
fitPoisMixed
#> 
#> Mixed Poisson regression
#> 
#> Call:
#> fitMod(formula = incidence ~ period + (1 | herd), data = lme4::cbpp, 
#>     fitfn = "poissonMixed")
#> 
#> Fixed effects:
#>              estimate  95%-lci     uci    p-val     
#> (Intercept)     1.277    0.903   1.651  < 0.001  ***
#> period2        -1.125   -1.663  -0.586  < 0.001  ***
#> period3        -1.319   -1.900  -0.738  < 0.001  ***
#> period4        -1.945   -2.723  -1.167  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Random effects:
#>  Groups Variance Std.Dev
#>  herd   0.250    0.500  
#> 
#> ICC: 0.504
#> 
#> Obs: 56   Groups: herd: 15
#> Log-lik: -96.692   AIC: 203.384
fitPoisMixed |> print(output = "irr")
#> 
#> Mixed Poisson regression
#> 
#> Call:
#> fitMod(formula = incidence ~ period + (1 | herd), data = lme4::cbpp, 
#>     fitfn = "poissonMixed")
#> 
#> Fixed effects (Incidence Rate Ratios):
#>                IRR  95%-lci    uci    p-val     
#> (Intercept)  3.585    2.467  5.211  < 0.001  ***
#> period2      0.325    0.189  0.556  < 0.001  ***
#> period3      0.267    0.150  0.478  < 0.001  ***
#> period4      0.143    0.066  0.311  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Random effects:
#>  Groups Variance Std.Dev
#>  herd   0.250    0.500  
#> 
#> ICC: 0.504
#> 
#> Obs: 56   Groups: herd: 15
#> Log-lik: -96.692   AIC: 203.384
```

### Mixed negative binomial regression (`negbinMixed`)

``` r

fitNegbinMixed <- fitMod(incidence ~ period + (1 | herd),
                         data = lme4::cbpp, fitfn = "negbinMixed")
#> boundary (singular) fit: see help('isSingular')
fitNegbinMixed
#> Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model failed to converge with max|grad| = 0.00688859 (tol = 0.002, component 1)
#>   See ?lme4::convergence and ?lme4::troubleshooting.
#> 
#> Mixed negative binomial regression
#> 
#> Call:
#> fitMod(formula = incidence ~ period + (1 | herd), data = lme4::cbpp, 
#>     fitfn = "negbinMixed")
#> 
#> Fixed effects:
#>              estimate  95%-lci     uci    p-val     
#> (Intercept)     1.403    0.933   1.872  < 0.001  ***
#> period2        -1.209   -1.993  -0.424    0.003  ** 
#> period3        -1.403   -2.217  -0.588  < 0.001  ***
#> period4        -2.022   -2.997  -1.047  < 0.001  ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Random effects:
#>  Groups Variance  Std.Dev
#>  herd   3.090e-10 0.000  
#> 
#> Obs: 56   Groups: herd: 15
#> Log-lik: -90.731   AIC: 193.462
```

------------------------------------------------------------------------

## 7. Machine learning

All ML models use the same
[`predict()`](https://rdrr.io/r/stats/predict.html) interface with
`output = "prob"`, `"class"`, or `"both"`.

``` r

fitSvm <- fitMod(ice_cream ~ video + puzzle + female,
                 data = IceCream, fitfn = "svm")
head(predict(fitSvm, output = "both"))
#>     vanilla chocolate strawberry      class
#> 1 0.5159623 0.1536322  0.3304055    vanilla
#> 2 0.3955869 0.1506206  0.4537925 strawberry
#> 3 0.5063692 0.2883714  0.2052593    vanilla
#> 4 0.5222386 0.1536506  0.3241108    vanilla
#> 5 0.4652675 0.1399319  0.3948005    vanilla
#> 6 0.4253437 0.1467723  0.4278840 strawberry
```

``` r

fitRf <- fitMod(ice_cream ~ video + puzzle + female,
                data = IceCream, fitfn = "randomForest")
head(predict(fitRf, output = "both"))
#>   vanilla chocolate strawberry   class
#> 1   0.484     0.266      0.250 vanilla
#> 2   0.536     0.038      0.426 vanilla
#> 3   0.698     0.126      0.176 vanilla
#> 4   0.592     0.030      0.378 vanilla
#> 5   0.730     0.008      0.262 vanilla
#> 6   0.556     0.004      0.440 vanilla
```

``` r

fitNnet <- fitMod(ice_cream ~ video + puzzle + female,
                  data = IceCream, fitfn = "nnet")
head(predict(fitNnet, output = "both"))
#>     vanilla   chocolate strawberry      class
#> 1 0.7387480 0.091109964  0.1701420    vanilla
#> 2 0.4308664 0.142353723  0.4267799    vanilla
#> 3 0.5618908 0.152816825  0.2852924    vanilla
#> 4 0.4019356 0.005075169  0.5929893 strawberry
#> 5 0.6598601 0.040357504  0.2997824    vanilla
#> 6 0.5552367 0.020164170  0.4245991    vanilla
```

``` r

fitC5 <- fitMod(ice_cream ~ video + puzzle + female,
                data = IceCream, fitfn = "C5.0")
#> Registered S3 method overwritten by 'rpart':
#>   method     from 
#>   plot.rpart alloy
head(predict(fitC5, output = "both"))
#>     vanilla  chocolate strawberry   class
#> 1 0.5441532 0.26802420  0.1878226 vanilla
#> 2 0.5148438 0.06984375  0.4153125 vanilla
#> 3 0.5441532 0.26802420  0.1878226 vanilla
#> 4 0.5441532 0.26802420  0.1878226 vanilla
#> 5 0.5148438 0.06984375  0.4153125 vanilla
#> 6 0.5148438 0.06984375  0.4153125 vanilla
```

``` r

fitNbayes <- fitMod(ice_cream ~ video + puzzle + female,
                    data = IceCream, fitfn = "naiveBayes")
head(predict(fitNbayes, output = "both"))
#>     vanilla  chocolate strawberry      class
#> 1 0.5826702 0.12326804  0.2940618    vanilla
#> 2 0.3933976 0.10803560  0.4985668 strawberry
#> 3 0.5635683 0.32015682  0.1162749    vanilla
#> 4 0.5674687 0.09612110  0.3364102    vanilla
#> 5 0.5230432 0.07619147  0.4007653    vanilla
#> 6 0.4135334 0.05134811  0.5351185 strawberry
```

``` r

fitLda <- fitMod(ice_cream ~ video + puzzle + female,
                 data = IceCream, fitfn = "lda")
head(predict(fitLda, output = "both"))
#>     vanilla  chocolate strawberry      class
#> 1 0.5413134 0.13510699  0.3235796    vanilla
#> 2 0.4405425 0.13901553  0.4204419    vanilla
#> 3 0.5475604 0.30649709  0.1459425    vanilla
#> 4 0.5302854 0.12049611  0.3492185    vanilla
#> 5 0.5017598 0.09309456  0.4051456    vanilla
#> 6 0.4621317 0.06853406  0.4693342 strawberry
```

``` r

fitGlmnet <- fitMod(ice_cream ~ video + puzzle + female,
                    data = IceCream, fitfn = "glmnet")
head(predict(fitGlmnet, output = "both"))
#>     vanilla chocolate strawberry   class
#> 1 0.4858372 0.2054552  0.3087076 vanilla
#> 2 0.4538581 0.1789014  0.3672405 vanilla
#> 3 0.5148256 0.3055882  0.1795862 vanilla
#> 4 0.4863560 0.1970862  0.3165578 vanilla
#> 5 0.4727409 0.1757248  0.3515343 vanilla
#> 6 0.4660833 0.1567841  0.3771325 vanilla
```

``` r

fitXgb <- fitMod(ice_cream ~ video + puzzle + female,
                 data = IceCream, fitfn = "xgboost")
head(predict(fitXgb, output = "both"))
#>     vanilla  chocolate strawberry      class
#> 1 0.4408226 0.15315042  0.4060270    vanilla
#> 2 0.3814507 0.08022393  0.5383254 strawberry
#> 3 0.4224597 0.07979191  0.4977484 strawberry
#> 4 0.4781624 0.13712463  0.3847131    vanilla
#> 5 0.6716797 0.03283665  0.2954837    vanilla
#> 6 0.5038718 0.03706879  0.4590594    vanilla
```

``` r

fitRpart <- fitMod(ice_cream ~ video + puzzle + female,
                   data = IceCream, fitfn = "rpart")
head(predict(fitRpart, output = "both"))
#>     vanilla  chocolate strawberry      class
#> 1 0.2500000 0.16666667  0.5833333 strawberry
#> 2 0.3157895 0.05263158  0.6315789 strawberry
#> 3 0.4545455 0.27272727  0.2727273    vanilla
#> 4 0.2500000 0.16666667  0.5833333 strawberry
#> 5 0.5555556 0.14814815  0.2962963    vanilla
#> 6 0.3157895 0.05263158  0.6315789 strawberry
```

------------------------------------------------------------------------

## 8. Robust standard errors

For `lm`, `glm`, and `lmrob` models, heteroscedasticity-consistent
standard errors can be requested via the `vcov` argument (powered by the
`sandwich` package). `"HC3"` is recommended for small samples.

| Type    | Description             |
|---------|-------------------------|
| `"HC0"` | White (1980)            |
| `"HC1"` | HC0 with df correction  |
| `"HC3"` | Recommended for small n |

``` r

fitLm     |> print(vcov = "HC3")
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
fitPois   |> print(vcov = "HC0")
#> 
#> Call:
#> stats::glm(formula = daysabs ~ mathnce + langnce + gender, family = "poisson", 
#>     data = Lahigh)
#> 
#> Coefficients:
#>                        estimate  95%-lci     uci    p-val     
#> (Intercept)               2.688    2.261   3.115  < 0.001  ***
#> mathnce                  -0.004   -0.018   0.011    0.644     
#> langnce                  -0.012   -0.023  -0.002    0.022  *  
#> gender  (ref: female)         .        .       .  < 0.001  ***
#> gender male              -0.401   -0.674  -0.128    0.004  ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Obs (NAs): 316 (0)   Pseudo R² (McFadden): 0.054   AIC: 3103.942
fitNegbin |> print(vcov = "HC3")
#> 
#> Call:
#> MASS::glm.nb(formula = daysabs ~ mathnce + langnce + gender, 
#>     data = Lahigh, init.theta = 0.7761669423, link = log)
#> 
#> Coefficients:
#>                        estimate  95%-lci     uci    p-val     
#> (Intercept)               2.716    2.251   3.181  < 0.001  ***
#> mathnce                  -0.002   -0.018   0.014    0.844     
#> langnce                  -0.014   -0.025  -0.004    0.009  ** 
#> gender  (ref: female)         .        .       .    0.002  ** 
#> gender male              -0.431   -0.719  -0.143    0.003  ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Overdispersion parameter (α = 1/θ):
#>   estimate  95%-lci    uci    p-val     
#>      1.288    1.047  1.530  < 0.001  ***
#> ---
#> 
#> Obs (NAs): 316 (0)   Pseudo R² (McFadden): 0.012   AIC: 1771.746
```

------------------------------------------------------------------------

## 9. Original model output

Use `output = "genuine"` to see the original
[`summary()`](https://rdrr.io/r/base/summary.html) output of the
underlying model object:

``` r

fitLm      |> print(output = "genuine")
#> 
#> Call:
#> stats::lm(formula = Fertility ~ ., data = swiss)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -15.2743  -5.2617   0.5032   4.1198  15.3213 
#> 
#> Coefficients:
#>                  Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)      66.91518   10.70604   6.250 1.91e-07 ***
#> Agriculture      -0.17211    0.07030  -2.448  0.01873 *  
#> Examination      -0.25801    0.25388  -1.016  0.31546    
#> Education        -0.87094    0.18303  -4.758 2.43e-05 ***
#> Catholic          0.10412    0.03526   2.953  0.00519 ** 
#> Infant.Mortality  1.07705    0.38172   2.822  0.00734 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 7.165 on 41 degrees of freedom
#> Multiple R-squared:  0.7067, Adjusted R-squared:  0.671 
#> F-statistic: 19.76 on 5 and 41 DF,  p-value: 5.594e-10
fitCox     |> print(output = "genuine")
#> Call:
#> survival::coxph(formula = Surv(foltime, folstatus) ~ gender + 
#>     agex, data = Whas100, model = TRUE, x = TRUE)
#> 
#>   n= 100, number of events= 51 
#> 
#>             coef exp(coef)  se(coef)      z Pr(>|z|)   
#> gender  0.213327  1.237789  0.306486  0.696  0.48640   
#> agex.L  1.980058  7.243167  0.731965  2.705  0.00683 **
#> agex.Q  0.558226  1.747569  0.675788  0.826  0.40878   
#> agex.C  0.002147  1.002149  0.607247  0.004  0.99718   
#> agex^4  0.373971  1.453495  0.543261  0.688  0.49121   
#> agex^5 -0.293196  0.745876  0.451619 -0.649  0.51620   
#> agex^6  0.685727  1.985214  0.390592  1.756  0.07916 . 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>        exp(coef) exp(-coef) lower .95 upper .95
#> gender    1.2378     0.8079    0.6788     2.257
#> agex.L    7.2432     0.1381    1.7254    30.407
#> agex.Q    1.7476     0.5722    0.4647     6.572
#> agex.C    1.0021     0.9979    0.3048     3.295
#> agex^4    1.4535     0.6880    0.5012     4.215
#> agex^5    0.7459     1.3407    0.3078     1.808
#> agex^6    1.9852     0.5037    0.9233     4.269
#> 
#> Concordance= 0.663  (se = 0.04 )
#> Likelihood ratio test= 19.86  on 7 df,   p=0.006
#> Wald test            = 17.62  on 7 df,   p=0.01
#> Score (logrank) test = 21.14  on 7 df,   p=0.004
fitWeibull |> print(output = "genuine")
#> 
#> Call:
#> survival::survreg(formula = Surv(foltime, folstatus) ~ gender + 
#>     age + bmi, data = Whas100, dist = "weibull")
#>               Value Std. Error     z       p
#> (Intercept)  8.9724     1.8925  4.74 2.1e-06
#> gender      -0.2702     0.3819 -0.71  0.4793
#> age         -0.0447     0.0167 -2.68  0.0073
#> bmi          0.0881     0.0455  1.94  0.0527
#> Log(scale)   0.2344     0.1245  1.88  0.0598
#> 
#> Scale= 1.26 
#> 
#> Weibull distribution
#> Loglik(model)= -444.4   Loglik(intercept only)= -455.3
#>  Chisq= 21.73 on 3 degrees of freedom, p= 7.4e-05 
#> Number of Newton-Raphson Iterations: 5 
#> n= 100
```
