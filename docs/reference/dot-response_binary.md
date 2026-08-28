# Extract and validate a binary response from a fitted model

Wrapper around [`response`](response.md) that additionally validates
that the response is a factor with exactly two levels, as required for
ROC analysis.

## Usage

``` r
.response_binary(x, ...)
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

A factor with exactly two levels. The attribute `"response"` carries the
name of the response variable.

## See also

[`response`](response.md),
[`roc`](https://rdrr.io/pkg/pROC/man/roc.html)
