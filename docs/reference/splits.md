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

Other tree: [`bestTree()`](bestTree.md), [`cParam()`](cParam.md),
[`leafRates()`](leafRates.md), [`node()`](node.md),
[`plot.rpart()`](plot.rpart.md), [`rules()`](rules.md)

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
