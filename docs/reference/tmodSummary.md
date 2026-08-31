# Extract model summaries for model comparison

S3 generic used internally by
[`tMod`](https://andrisignorell.github.io/alloy/reference/tMod.md) to
extract coefficients and model statistics from fitted models in a
standardized format.

## Usage

``` r
tmodSummary(x, ...)

# S3 method for class 'lm'
tmodSummary(x, conf.level = 0.95, ...)

# S3 method for class 'lmrob'
tmodSummary(x, conf.level = 0.95, ...)

# S3 method for class 'glm'
tmodSummary(x, conf.level = 0.95, useProfile = TRUE, ...)

# S3 method for class 'coxph'
tmodSummary(x, conf.level = 0.95, ...)

# S3 method for class 'gam'
tmodSummary(x, conf.level = 0.95, ...)

# S3 method for class 'lmer'
tmodSummary(x, conf.level = 0.95, ...)
```

## Arguments

- x:

  A fitted model object

- ...:

  Additional arguments passed to methods

- conf.level:

  Confidence level for intervals

- useProfile:

  Logical; use profile likelihood for CI (glm only)

## Value

A list with components:

- `coef`: data frame with columns `name, est, se, stat, p, lci, uci`

- `statsx`: named numeric vector of model statistics

## Details

Each method returns a list with components `coef` (a data frame of
coefficients and confidence intervals) and `statsx` (a named numeric
vector of model statistics). These are combined by
[`tMod()`](https://andrisignorell.github.io/alloy/reference/tMod.md) to
enable comparison across different model types.

## See also

[`tMod`](https://andrisignorell.github.io/alloy/reference/tMod.md)

Other model.comparison:
[`tMod()`](https://andrisignorell.github.io/alloy/reference/tMod.md)
