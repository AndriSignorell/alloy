# Model matrix for gls objects

Extracts the model matrix from a `gls` object by reconstructing it from
the model formula and data.

## Usage

``` r
# S3 method for class 'gls'
model.matrix(object, ...)
```

## Arguments

- object:

  A fitted `gls` model object (from nlme).

- ...:

  Additional arguments (currently ignored).

## Value

A model matrix corresponding to the fixed-effects design matrix.

## Details

This function provides an S3 method for
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) for `gls`
objects, which do not have a built-in method in base R.

The data is extracted from the original model call. This may fail if the
data is not available in the evaluation environment.

## Examples

``` r
if (FALSE) { # \dontrun{
library(nlme)
mod <- gls(distance ~ age, data = Orthodont)
model.matrix(mod)
} # }
```
