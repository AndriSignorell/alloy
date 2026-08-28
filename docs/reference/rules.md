# Decision rules for an rpart tree

Extracts the decision path rules for selected nodes (or all nodes /
leaves only) of a fitted `rpart` tree.

## Usage

``` r
rules(x, node = NULL, leafonly = FALSE)
```

## Arguments

- x:

  A fitted `rpart` object.

- node:

  Character or numeric vector of node ids to extract. If `NULL`
  (default) all nodes are returned.

- leafonly:

  Logical. If `TRUE` only terminal (leaf) nodes are returned. Default is
  `FALSE`.

## Value

An object of class `"rules"`, a list with components `frame`, `ylevels`,
`ds.size`, and `path`. Returns `NA` if no nodes match the selection.

## See also

[`node`](node.md),
[`path.rpart`](https://rdrr.io/pkg/rpart/man/path.rpart.html)

Other tree: [`bestTree()`](bestTree.md), [`cParam()`](cParam.md),
[`leafRates()`](leafRates.md), [`node()`](node.md),
[`plot.rpart()`](plot.rpart.md), [`splits()`](splits.md)

## Examples

``` r
r <- rpart::rpart(Species ~ ., data = iris)
rules(r)
#> 
#>  Rule number: 1 
#> 
#>  Rule number: 2 [yval=setosa cover=50 (33%) prob=1.00]
#>    Petal.Length< 2.45
#> 
#>  Rule number: 3 
#>    Petal.Length>=2.45
#> 
#>  Rule number: 6 [yval=versicolor cover=54 (36%) prob=0.00]
#>    Petal.Length>=2.45
#>    Petal.Width< 1.75
#> 
#>  Rule number: 7 [yval=virginica cover=46 (31%) prob=0.00]
#>    Petal.Length>=2.45
#>    Petal.Width>=1.75
#> 
rules(r, leafonly = TRUE)
#> 
#>  Rule number: 2 [yval=setosa cover=50 (33%) prob=1.00]
#>    Petal.Length< 2.45
#> 
#>  Rule number: 6 [yval=versicolor cover=54 (36%) prob=0.00]
#>    Petal.Length>=2.45
#>    Petal.Width< 1.75
#> 
#>  Rule number: 7 [yval=virginica cover=46 (31%) prob=0.00]
#>    Petal.Length>=2.45
#>    Petal.Width>=1.75
#> 
```
