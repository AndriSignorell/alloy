# Reference level of factor predictors in a model

Extracts the reference (baseline) category for every factor predictor in
a fitted model object. The reference level is derived from the contrast
matrix that was actually used during fitting, so the result is correct
even when `base` in
[`contr.treatment`](https://rdrr.io/r/stats/contrast.html) is not 1 or
when contrasts have been set globally via
[`options`](https://rdrr.io/r/base/options.html).

## Usage

``` r
refLevel(x)
```

## Arguments

- x:

  A fitted model object with a `terms` attribute and a `model` data
  frame (e.g. objects of class `"lm"`, `"glm"`, `"lmerMod"`, or
  `"glmerMod"`).

## Value

A named character vector whose names are the factor predictor variables
and whose values are the corresponding reference levels. Returns a
zero-length named character vector when the model contains no factor
predictors.

## Details

The function inspects `attr(model.matrix(x), "contrasts")` for each
factor predictor.

- If the contrast is stored as a *character string* (e.g.
  `"contr.treatment"`), the first level of the factor is returned as the
  reference.

- If the contrast is stored as a *matrix*, the reference level is
  identified as the unique row whose entries are all zero (i.e. the row
  that does not map to any dummy column).

Contrasts other than treatment contrasts (e.g. `contr.sum`,
`contr.helmert`) are not supported and trigger an informative error.

## See also

[`contrasts`](https://rdrr.io/r/stats/contrasts.html),
[`contr.treatment`](https://rdrr.io/r/stats/contrast.html)

Other regression.utils:
[`coefCI()`](https://andrisignorell.github.io/alloy/reference/coefCI.md),
[`pseudoR2()`](https://andrisignorell.github.io/alloy/reference/pseudoR2.md),
[`rSq()`](https://andrisignorell.github.io/alloy/reference/rSq.md),
[`response()`](https://andrisignorell.github.io/alloy/reference/response.md),
[`varImp()`](https://andrisignorell.github.io/alloy/reference/varImp.md),
[`vif()`](https://andrisignorell.github.io/alloy/reference/vif.md)

## Examples

``` r
m <- lm(Sepal.Length ~ Species, data = iris)
refLevel(m)
#>  Species 
#> "setosa" 
# Species
# "setosa"

# Custom base level
iris2 <- iris
contrasts(iris2$Species) <- contr.treatment(3, base = 2)
m2 <- lm(Sepal.Length ~ Species, data = iris2)
refLevel(m2)
#>      Species 
#> "versicolor" 
# Species
# "versicolor"
```
