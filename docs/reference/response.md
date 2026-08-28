# Extract the response variable from a fitted model

Retrieves the response vector from a fitted model object, with special
handling for model classes that do not store a standard `terms`
component (`C5.0`, `rpart`, `naive_bayes`). The name of the response
variable is attached as the `"response"` attribute of the return value.

## Usage

``` r
response(x, ...)
```

## Arguments

- x:

  A fitted model object. Supported classes include `"lm"`, `"glm"`,
  `"multinom"`, `"polr"`, `"rpart"`, `"C5.0"`, and `"naive_bayes"`. Any
  other class is attempted via the default
  [`model.frame`](https://rdrr.io/r/stats/model.frame.html) path.

- ...:

  Currently unused; reserved for future methods.

## Value

The response vector as returned by
[`model.response`](https://rdrr.io/r/stats/model.extract.html), or a
factor reconstructed from internal model slots for `rpart`. The
attribute `"response"` carries the name of the response variable as a
character string, or `NA_character_` if it cannot be determined.

## See also

[`model.response`](https://rdrr.io/r/stats/model.extract.html),
[`model.frame`](https://rdrr.io/r/stats/model.frame.html)

Other regression.utils: [`coefCI()`](coefCI.md),
[`pseudoR2()`](pseudoR2.md), [`rSq()`](rSq.md),
[`refLevel()`](refLevel.md), [`varImp()`](varImp.md), [`vif()`](vif.md)
