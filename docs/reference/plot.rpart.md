# Plot an rpart tree using rpart.plot with node labels

Overrides the default `rpart` plot and delegates to
[`rpart.plot`](https://rdrr.io/pkg/rpart.plot/man/rpart.plot.html),
optionally overlaying node id labels on each box.

## Usage

``` r
# S3 method for class 'rpart'
plot(
  x = stop("no 'x' arg"),
  type = 2L,
  extra = "auto",
  under = FALSE,
  fallen.leaves = TRUE,
  digits = 2L,
  varlen = 0L,
  faclen = 0L,
  roundint = TRUE,
  cex = NULL,
  tweak = 1,
  clip.facs = FALSE,
  clip.right.labs = TRUE,
  snip = FALSE,
  box.palette = "auto",
  shadow.col = 0,
  node.labels = TRUE,
  ...
)
```

## Arguments

- x:

  A fitted `rpart` object.

- type:

  Integer passed to `rpart.plot`. Default `2`.

- extra:

  Passed to `rpart.plot`. Default `"auto"`.

- under:

  Logical. Default `FALSE`.

- fallen.leaves:

  Logical. Default `TRUE`.

- digits:

  Integer. Default `2`.

- varlen:

  Integer. Default `0` (full names).

- faclen:

  Integer. Default `0` (full factor labels).

- roundint:

  Logical. Default `TRUE`.

- cex:

  Numeric or `NULL`.

- tweak:

  Numeric. Default `1`.

- clip.facs:

  Logical. Default `FALSE`.

- clip.right.labs:

  Logical. Default `TRUE`.

- snip:

  Logical. Default `FALSE`.

- box.palette:

  Character or list. Default `"auto"` uses the package palette.

- shadow.col:

  Colour of node shadows. Default `0` (none).

- node.labels:

  Logical. If `TRUE` (default), node ids are printed above each box.

- ...:

  Further arguments passed to `rpart.plot`.

## Value

Invisibly returns the list returned by `rpart.plot`.

## See also

Other tree:
[`bestTree()`](https://andrisignorell.github.io/alloy/reference/bestTree.md),
[`cParam()`](https://andrisignorell.github.io/alloy/reference/cParam.md),
[`leafRates()`](https://andrisignorell.github.io/alloy/reference/leafRates.md),
[`node()`](https://andrisignorell.github.io/alloy/reference/node.md),
[`rules()`](https://andrisignorell.github.io/alloy/reference/rules.md),
[`splits()`](https://andrisignorell.github.io/alloy/reference/splits.md)
