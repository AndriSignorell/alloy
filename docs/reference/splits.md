# Split labels for each node of an rpart tree

Returns a two-column character matrix with the left and right split
labels for every node of a fitted `rpart` tree. Leaf nodes are
represented by empty strings.

## Usage

``` r
splits(x)
```

## Arguments

- x:

  A fitted `rpart` object.

## Value

A character matrix with columns `"cutleft"` and `"cutright"` and one row
per node.

## See also

Other tree:
[`bestTree()`](https://andrisignorell.github.io/alloy/reference/bestTree.md),
[`cParam()`](https://andrisignorell.github.io/alloy/reference/cParam.md),
[`leafRates()`](https://andrisignorell.github.io/alloy/reference/leafRates.md),
[`node()`](https://andrisignorell.github.io/alloy/reference/node.md),
[`plot.rpart()`](https://andrisignorell.github.io/alloy/reference/plot.rpart.md),
[`rules()`](https://andrisignorell.github.io/alloy/reference/rules.md)

## Examples

``` r
r <- rpart::rpart(Species ~ ., data = iris)
splits(r)
#>      cutleft cutright
#> [1,] "<2.45" ">2.45" 
#> [2,] ""      ""      
#> [3,] "<1.75" ">1.75" 
#> [4,] ""      ""      
#> [5,] ""      ""      
```
