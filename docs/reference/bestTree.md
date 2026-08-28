# Best tree size using the 1-SE rule

Selects the most parsimonious tree whose cross-validated error is within
one standard error of the minimum, following Breiman et al. (1984).

## Usage

``` r
bestTree(x)
```

## Arguments

- x:

  A fitted `rpart` object.

## Value

A list with components:

- `leaves`:

  Number of terminal nodes in the best tree.

- `cp`:

  Complexity parameter to pass to `prune()`.

## References

Breiman, L., Friedman, J., Olshen, R., & Stone, C. (1984).
*Classification and Regression Trees*. Wadsworth.

## See also

Other tree: [`cParam()`](cParam.md), [`leafRates()`](leafRates.md),
[`node()`](node.md), [`plot.rpart()`](plot.rpart.md),
[`rules()`](rules.md), [`splits()`](splits.md)

## Examples

``` r
r <- rpart::rpart(Species ~ ., data = iris)
#> Registered S3 method overwritten by 'rpart':
#>   method     from 
#>   plot.rpart alloy
bt <- bestTree(r)
r.pruned <- rpart::prune(r, cp = bt$cp)
```
