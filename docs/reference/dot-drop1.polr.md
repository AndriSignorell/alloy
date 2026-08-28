# Likelihood-ratio drop1 table for polr models

Computes a Type-II analysis-of-deviance table for a fitted
[`polr`](https://rdrr.io/pkg/MASS/man/polr.html) model by sequentially
dropping each term and comparing the resulting likelihood-ratio
statistic against the full model (or the model without higher-order
relatives for hierarchical terms).

## Usage

``` r
.drop1.polr(mod, ...)
```

## Arguments

- mod:

  A fitted `"polr"` object.

- ...:

  Currently unused.

## Value

A `data.frame` of class `c("anova", "data.frame")`.

## Details

Reduced models are fitted on the *model matrix* of the full model (with
the relevant columns removed) rather than by manipulating the original
formula. This is intentional: it avoids complex formula reconstruction
for transformed terms ([`poly()`](https://rdrr.io/r/stats/poly.html),
`ns()`, etc.) and is consistent with the Type-II approach used in car.
As a consequence, `subset` and special formula terms are already baked
into the model matrix and need no re-evaluation. Offset terms, however,
are *not* part of the model matrix and would be silently dropped in the
reduced fits, invalidating the LR comparison; models with an offset are
therefore rejected with an error.

The reduced models are refitted with the `method` of the full model
(logistic, probit, ...), so the deviances are comparable.

Fits that fail to converge are silently set to `NA`. Note that `polr()`
sometimes issues convergence *warnings* without throwing an error; such
fits are accepted as-is (consistent with the behaviour of
`drop1.default`).
