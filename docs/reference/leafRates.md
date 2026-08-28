# Misclassification rates per leaf node

Computes the number of correctly and incorrectly classified observations
in each terminal (leaf) node of a classification tree.

## Usage

``` r
leafRates(x)
```

## Arguments

- x:

  A fitted `rpart` classification object (i.e. `x$method != "anova"`).

## Value

An object of class `c("leafRates", "list")` with components:

- `node`:

  node ids of the leaf nodes.

- `freq`:

  Integer matrix with columns `"right"` and `"wrong"`.

- `p.row`:

  Row proportions of `freq`.

- `mfreq`:

  Total observations per leaf.

- `mperc`:

  Proportion of total observations per leaf.

## See also

Other tree: [`bestTree()`](bestTree.md), [`cParam()`](cParam.md),
[`node()`](node.md), [`plot.rpart()`](plot.rpart.md),
[`rules()`](rules.md), [`splits()`](splits.md)

## Examples

``` r
r <- rpart::rpart(Species ~ ., data = iris)
leafRates(r)
#> Warning: unknown format code 'abs' in 'fmt'; using the default format
#> Warning: unknown format code 'abs' in 'fmt'; using the default format
#>   node right wrong right% wrong% total perc%
#> 2    2    50     0 100.0%   0.0%    50 33.3%
#> 6    6    49     5  90.7%   9.3%    54 36.0%
#> 7    7    45     1  97.8%   2.2%    46 30.7%
```
