# Randomized Quantile Residuals

Computes randomized quantile residuals (Dunn and Smyth, 1996) for a
fitted logistic model. Unlike Pearson or deviance residuals, they are
exactly standard normal when the model is correct, whatever the fitted
probabilities are - which is what makes a Q-Q plot readable for a binary
response in the first place.

## Usage

``` r
quantileResid(x, nSim = 1L)
```

## Arguments

- x:

  a fitted logistic model of class `"FitMod"` (fitted with
  `fitfn = "logit"`) or a binomial
  [`glm`](https://rdrr.io/r/stats/glm.html).

- nSim:

  number of independent randomizations. `1` (default) returns a vector,
  larger values a matrix with one column per draw.

## Value

a numeric vector of length \\n\\, or an \\n \times\\ `nSim` matrix when
`nSim > 1`. The names (or row names) are those of the model's fitted
values.

## Details

For a discrete response the probability integral transform does not
produce a uniform variable, because the distribution function jumps at
the observed values. Randomization closes the gap: with \\a_i = F(y_i -
1)\\ and \\b_i = F(y_i)\\ under the fitted model, a draw \\u_i \sim
U(a_i, b_i)\\ is uniform on \\(0,1)\\ whenever the model holds, and
\\r_i = \Phi^{-1}(u_i)\\ is standard normal.

For the binomial case both bounds are available in closed form, so no
simulation of replicate data sets is needed - one uniform draw per
observation is the entire computation.

The residuals are random: two calls on the same model return different
values, and any conclusion that changes between draws is not a
conclusion about the model. Use
[`set.seed`](https://rdrr.io/r/base/Random.html) for reproducibility,
and `nSim > 1` to see the spread across draws.

## Power for a Bernoulli response

With \\m = 1\\ the randomization interval spans the entire jump of the
distribution function, so the uniform draw contributes about as much
variation as the data do, and the residuals stay close to normal even
when the mean structure is wrong. In a simulation with a squared term
omitted (\\n = 2000\\), [`plotBinnedResid`](plotBinnedResid.md) put 35
of 44 bins outside the 95\\ residuals returned \\p = 0.55\\.

Read the Q-Q plot for what it can see - a few extreme observations, and
the distributional fit of grouped data (\\m \> 1\\), where the intervals
are narrow and the residuals do have power. For the mean structure use
the binned residuals.

## References

Dunn, P. K. and Smyth, G. K. (1996) Randomized quantile residuals.
*Journal of Computational and Graphical Statistics*, **5**(3), 236–244.

## See also

[model-diagnostics-overview](model-diagnostics-overview.md) for an
overview of the diagnostics for logistic models in alloy.

## Examples

``` r
fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")

set.seed(1)
r <- quantileResid(fitLogit)
pharos::plotQQ(r)


# spread across randomizations
apply(quantileResid(fitLogit, nSim = 20), 2, function(z) shapiro.test(z)$p.value)
#>  [1] 0.87154367 0.23085355 0.43384689 0.76980896 0.77978689 0.54881214
#>  [7] 0.75192673 0.12018519 0.55102849 0.70086520 0.39916401 0.85445491
#> [13] 0.48426356 0.36333635 0.74888588 0.79453578 0.76125994 0.09860262
#> [19] 0.48790033 0.83523604
```
