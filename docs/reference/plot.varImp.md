# Cleveland dot plot for variable importance

Displays a Cleveland dot plot of variable importance scores as returned
by [`varImp`](varImp.md). Variables are ordered by importance (most
important at the top).

## Usage

``` r
# S3 method for class 'varImp'
plot(
  x,
  main = "Variable Importance",
  xlab = "Importance",
  pch = 16,
  col = "steelblue",
  cex = 1.2,
  ...
)
```

## Arguments

- x:

  An object of class `"varImp"` as returned by [`varImp`](varImp.md).

- main:

  Character string for the plot title. Default is
  `"Variable Importance"`.

- xlab:

  Character string for the x-axis label. Default is `"Importance"`.

- pch:

  Plotting character. Default is `16` (filled circle).

- col:

  Colour of the points and reference lines. Default is `"steelblue"`.

- cex:

  Numeric: point size. Default is `1.2`.

- ...:

  Further arguments passed to
  [`dotchart`](https://rdrr.io/r/graphics/dotchart.html).

## Value

Invisibly returns `x`.

## See also

[`varImp`](varImp.md)

## Examples

``` r
fitRf <- fitMod(ice_cream ~ video + puzzle + female,
                IceCream, fitfn = "randomForest")
plot(varImp(fitRf))

```
