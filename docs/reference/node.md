# Structured node information for an rpart tree

Returns a structured list with detailed information about selected nodes
of a fitted `rpart` tree, including split details, class counts, and
probabilities.

## Usage

``` r
node(x, node = NULL, type = c("all", "split", "leaf"), digits = 3L)
```

## Arguments

- x:

  A fitted `rpart` object (must have been fitted with `model = TRUE` and
  `y = TRUE`).

- node:

  Character or numeric vector of node ids. If `NULL` (default) all nodes
  are returned.

- type:

  Character string. One of `"all"` (default), `"split"` (internal nodes
  only), or `"leaf"` (terminal nodes only).

- digits:

  Integer. Number of significant digits for split values. Default is
  `3`.

## Value

An object of class `"node"`, a named list with one element per selected
node. Each element contains: `id`, `vname`, `isleaf`, `nobs`, `group`,
`ycount`, `yprob`, `nodeprob`, `complexity`, `tprint`, and (for split
nodes) `sons`, `sons_n`, `primarysplits`, `surrogatesplits`.

## See also

[`rules`](https://andrisignorell.github.io/alloy/reference/rules.md)

Other tree:
[`bestTree()`](https://andrisignorell.github.io/alloy/reference/bestTree.md),
[`cParam()`](https://andrisignorell.github.io/alloy/reference/cParam.md),
[`leafRates()`](https://andrisignorell.github.io/alloy/reference/leafRates.md),
[`plot.rpart()`](https://andrisignorell.github.io/alloy/reference/plot.rpart.md),
[`rules()`](https://andrisignorell.github.io/alloy/reference/rules.md),
[`splits()`](https://andrisignorell.github.io/alloy/reference/splits.md)

## Examples

``` r
r <- rpart::rpart(Species ~ ., data = iris)
node(r)
#> 
#> node number 1: 150 observations,    complexity param = 0.5
#>   predicted class=setosa      expected loss=0.667  P(node) =1
#>     class counts:    50    50    50
#>    probabilities: 0.333 0.333 0.333 
#>   left son=2 (50 obs)  right son=3 (100 obs)
#>   Primary splits:
#>       Petal.Length         < 2.45 to the left,  improve=50, (0 missing)
#>       Petal.Width          < 0.8  to the left,  improve=50, (0 missing)
#>       Sepal.Length         < 5.45 to the left,  improve=34.2, (0 missing)
#>       Sepal.Width          < 3.35 to the right, improve=19, (0 missing)
#>   Surrogate splits:
#>       Petal.Width          < 0.8  to the left,  agree=1, adj=1, (0 split)
#>       Sepal.Length         < 5.45 to the left,  agree=0.92, adj=0.76, (0 split)
#>       Sepal.Width          < 3.35 to the right, agree=0.833, adj=0.5, (0 split)
#> 
#> 
#> node number 2: 50 observations
#>   predicted class=setosa      expected loss=0  P(node) =0.333
#>     class counts:    50     0     0
#>    probabilities: 1.000 0.000 0.000 
#> 
#> 
#> node number 3: 100 observations,    complexity param = 0.44
#>   predicted class=versicolor  expected loss=0.5  P(node) =0.667
#>     class counts:     0    50    50
#>    probabilities: 0.000 0.500 0.500 
#>   left son=6 (54 obs)  right son=7 (46 obs)
#>   Primary splits:
#>       Petal.Width          < 1.75 to the left,  improve=39, (0 missing)
#>       Petal.Length         < 4.75 to the left,  improve=37.4, (0 missing)
#>       Sepal.Length         < 6.15 to the left,  improve=10.7, (0 missing)
#>       Sepal.Width          < 2.45 to the left,  improve=3.56, (0 missing)
#>   Surrogate splits:
#>       Petal.Length         < 4.75 to the left,  agree=0.91, adj=0.804, (0 split)
#>       Sepal.Length         < 6.15 to the left,  agree=0.73, adj=0.413, (0 split)
#>       Sepal.Width          < 2.95 to the left,  agree=0.67, adj=0.283, (0 split)
#> 
#> 
#> node number 6: 54 observations
#>   predicted class=versicolor  expected loss=0.0926  P(node) =0.36
#>     class counts:     0    49     5
#>    probabilities: 0.000 0.907 0.093 
#> 
#> 
#> node number 7: 46 observations
#>   predicted class=virginica   expected loss=0.0217  P(node) =0.307
#>     class counts:     0     1    45
#>    probabilities: 0.000 0.022 0.978 
#> 
node(r, type = "leaf")
#> 
#> node number 2: 50 observations
#>   predicted class=setosa      expected loss=0  P(node) =0.333
#>     class counts:    50     0     0
#>    probabilities: 1.000 0.000 0.000 
#> 
#> 
#> node number 6: 54 observations
#>   predicted class=versicolor  expected loss=0.0926  P(node) =0.36
#>     class counts:     0    49     5
#>    probabilities: 0.000 0.907 0.093 
#> 
#> 
#> node number 7: 46 observations
#>   predicted class=virginica   expected loss=0.0217  P(node) =0.307
#>     class counts:     0     1    45
#>    probabilities: 0.000 0.022 0.978 
#> 
```
