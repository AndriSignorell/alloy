# Pseudo R-Squared Measures for Regression Models

Computes a set of pseudo R-squared statistics for fitted regression
models where the ordinary coefficient of determination is not defined,
such as logistic, Poisson or ordinal regression.

## Usage

``` r
pseudoR2(fit, which = "McFadden")
```

## Arguments

- fit:

  a fitted model object of class `glm`, `multinom` (nnet), `polr` (MASS)
  or `vglm` (VGAM)

- which:

  character vector naming the measures to return, or `"all"` for
  everything available

## Value

a named numeric vector holding the requested measures

## Details

The following measures are available. Which of them can be computed
depends on the model class, the family and the link function; measures
that are not defined for a given fit are omitted from the result.

- `McFadden`:

  likelihood ratio index

- `McFaddenAdj`:

  adjusted likelihood ratio index, penalized for the number of estimated
  parameters

- `CoxSnell`:

  maximum likelihood R-squared

- `Nagelkerke`:

  Cox-Snell R-squared, rescaled to a maximum of 1

- `AldrichNelson`:

  based on the likelihood ratio statistic

- `VeallZimmermann`:

  correction of Aldrich-Nelson

- `McKelveyZavoina`:

  latent variable R-squared, logit and probit links only

- `Efron`:

  squared correlation between observed and predicted values

- `Tjur`:

  coefficient of discrimination, binary responses only

- `AIC`, `BIC`:

  information criteria of the fitted model

- `logLik`, `logLik0`:

  log-likelihood of the fitted and of the null model

- `G2`:

  likelihood ratio statistic

All measures are derived from the log-likelihoods of the fitted and of
the intercept-only model, not from the deviance ratio; the two coincide
only where the saturated log-likelihood vanishes.

For `glm` objects the null model is refitted through
[`glm.fit`](https://rdrr.io/r/stats/glm.html) on the model's own
response, prior weights and offset. Aggregated responses
(`cbind(success, failure)`), frequency weights and offsets are therefore
handled correctly, and the original data need not be accessible. For the
remaining classes the null model is refitted with
[`update`](https://rdrr.io/r/stats/update.html), which requires the data
of the original call to be available.

Where prior weights are present, the sample size entering Cox-Snell,
Nagelkerke, Aldrich-Nelson and Veall-Zimmermann is their sum rather than
the number of rows.

For `vglm` objects the package VGAM must be installed and the model
should have been fitted with `model = TRUE`, so that the model frame can
be extracted.

## References

McFadden, D. (1974) Conditional logit analysis of qualitative choice
behavior. In: Zarembka, P. (ed.) *Frontiers in Econometrics*, Academic
Press, New York, 105-142.

Cox, D. R., Snell, E. J. (1989) *Analysis of Binary Data*, 2nd ed.,
Chapman and Hall, London.

Nagelkerke, N. J. D. (1991) A note on a general definition of the
coefficient of determination. *Biometrika*, 78(3), 691-692.

Veall, M. R., Zimmermann, K. F. (1996) Pseudo-R2 measures for some
common limited dependent variable models. *Journal of Economic Surveys*,
10(3), 241-259.

Tjur, T. (2009) Coefficients of determination in logistic regression
models. *The American Statistician*, 63(4), 366-372.

## See also

Other regression.utils: [`coefCI()`](coefCI.md), [`rSq()`](rSq.md),
[`refLevel()`](refLevel.md), [`response()`](response.md),
[`varImp()`](varImp.md), [`vif()`](vif.md)

## Examples

``` r
fit <- glm(am ~ wt + hp, data = mtcars, family = binomial)

pseudoR2(fit)
#>  McFadden 
#> 0.7673104 
## [1] 0.7178751

pseudoR2(fit, which = c("Nagelkerke", "Tjur"))
#> Nagelkerke       Tjur 
#>  0.8708970  0.8112073 
pseudoR2(fit, which = "all")
#>        McFadden     McFaddenAdj        CoxSnell      Nagelkerke   AldrichNelson 
#>       0.7673104       0.6285170       0.6453351       0.8708970       0.5089812 
#> VeallZimmermann McKelveyZavoina           Efron            Tjur             AIC 
#>       0.8857450       0.9265406       0.8068398       0.8112073      16.0591105 
#>             BIC          logLik         logLik0              G2 
#>      20.4563182      -5.0295552     -21.6148666      33.1706228 
```
