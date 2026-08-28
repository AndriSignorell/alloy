# Complexity parameter table for an rpart tree

A lightweight wrapper around
[`printcp`](https://rdrr.io/pkg/rpart/man/printcp.html) and
[`plotcp`](https://rdrr.io/pkg/rpart/man/plotcp.html) that stores the CP
table and the minimum cross-validated error CP as a structured object.

## Usage

``` r
cParam(x, ...)
```

## Arguments

- x:

  A fitted `rpart` object.

- ...:

  Currently unused.

## Value

An object of class `"CP"` with components `cp` (the full CP table),
`mincp` (CP at minimum xerror), and `x` (the original rpart object).

## See also

[`bestTree`](https://andrisignorell.github.io/alloy/reference/bestTree.md),
[`printcp`](https://rdrr.io/pkg/rpart/man/printcp.html),
[`plotcp`](https://rdrr.io/pkg/rpart/man/plotcp.html)

Other tree:
[`bestTree()`](https://andrisignorell.github.io/alloy/reference/bestTree.md),
[`leafRates()`](https://andrisignorell.github.io/alloy/reference/leafRates.md),
[`node()`](https://andrisignorell.github.io/alloy/reference/node.md),
[`plot.rpart()`](https://andrisignorell.github.io/alloy/reference/plot.rpart.md),
[`rules()`](https://andrisignorell.github.io/alloy/reference/rules.md),
[`splits()`](https://andrisignorell.github.io/alloy/reference/splits.md)

## Examples

``` r
r <- rpart::rpart(Species ~ ., data = iris)
cp <- cParam(r)
cp
#> 
#> Classification tree:
#> rpart::rpart(formula = Species ~ ., data = iris)
#> 
#> Variables actually used in tree construction:
#> [1] Petal.Length Petal.Width 
#> 
#> Root node error: 100/150 = 0.66667
#> 
#> n= 150 
#> 
#>     CP nsplit rel error xerror     xstd
#> 1 0.50      0      1.00   1.14 0.052307
#> 2 0.44      1      0.50   0.61 0.060161
#> 3 0.01      2      0.06   0.07 0.025833
#> 
plot(cp)

```
