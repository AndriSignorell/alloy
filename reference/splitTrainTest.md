# Split Data into Training, Validation and Test Sets

Splits a vector, matrix or data frame into two or more random subsets,
as used for training, validation and testing predictive models. The
split can be stratified, so that the composition of a grouping variable
is retained in every subset.

## Usage

``` r
splitTrainTest(x, p = 0.1, strata = NULL, output = c("data", "group"))
```

## Arguments

- x:

  an object to split, typically a vector, matrix or data frame

- p:

  the proportions of the subsets. A single number is the proportion
  assigned to the test set, the remainder going to the training set. A
  vector of length two or more gives the proportion of each subset in
  turn. The values need not sum to 1 and are rescaled if they do not, so
  ratios such as `c(6, 2, 2)` can be given directly. Names, where
  present, are used as subset names; otherwise `c("train", "test")` and
  `c("train", "validation", "test")` are supplied for length two and
  three.

- strata:

  an optional vector of the same length (or number of rows) as `x`,
  giving the strata to sample within. A data frame or list of several
  vectors is reduced to their interaction. Missing values form a stratum
  of their own.

- output:

  the value to return, either `"data"` (default) for the subsets
  themselves or `"group"` for a factor of subset memberships

## Value

For `output = "data"` a named list with one element per subset, in the
order given by `p`, each a subset of `x` of the same type.

For `output = "group"` a factor of length `n` whose levels are the
subset names, giving the subset each observation was assigned to.

## Details

Splitting data into disjoint subsets is a common strategy for evaluating
predictive models. The training set is used to fit the model, the test
set to assess predictive performance on unseen data. A three-way split
adds a validation set for tuning and model selection, keeping the test
set untouched until the final assessment.

Subset sizes are `n * p` rounded by the largest remainder method, so
they sum to `n` exactly and each subset is as close to its requested
proportion as integer sizes allow. A subset that would remain empty is
an error.

Stratified sampling draws within each stratum separately and so
reproduces the composition of `strata` in every subset. It is worth the
effort whenever a stratum is small enough that an unstratified draw
could leave it thinly represented, or absent, in one of the subsets:
rare classes in a classification problem, an unbalanced treatment
variable, few observations per site or period.

Rounding within a stratum is not independent of the other strata. The
fractional parts of `n * p` repeat across strata, so rounding each
stratum on its own would hand the leftover observation to the same
subset every time and bias the smallest subset downwards. The remainders
are therefore carried from one stratum to the next, and the strata are
visited in random order. A stratum smaller than the number of subsets is
not an error, it simply cannot contribute to all of them.

The assignment is drawn from R's global random number generator, whose
state the function does not modify. Call
[`set.seed()`](https://rdrr.io/r/base/Random.html) before the function
for reproducible results.

The subsets are disjoint and exhaustive, so `output = "group"` is a
complete description of the split. It can be stored alongside the data,
tabulated, or passed to [`split()`](https://rdrr.io/r/base/split.html)
to obtain the data subsets later.

## Examples

``` r
splitTrainTest(iris)
#> $train
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1            5.1         3.5          1.4         0.2     setosa
#> 2            4.9         3.0          1.4         0.2     setosa
#> 3            4.7         3.2          1.3         0.2     setosa
#> 4            4.6         3.1          1.5         0.2     setosa
#> 5            5.0         3.6          1.4         0.2     setosa
#> 6            5.4         3.9          1.7         0.4     setosa
#> 7            4.6         3.4          1.4         0.3     setosa
#> 8            5.0         3.4          1.5         0.2     setosa
#> 9            4.4         2.9          1.4         0.2     setosa
#> 10           4.9         3.1          1.5         0.1     setosa
#> 11           5.4         3.7          1.5         0.2     setosa
#> 12           4.8         3.4          1.6         0.2     setosa
#> 13           4.8         3.0          1.4         0.1     setosa
#> 14           4.3         3.0          1.1         0.1     setosa
#> 17           5.4         3.9          1.3         0.4     setosa
#> 18           5.1         3.5          1.4         0.3     setosa
#> 19           5.7         3.8          1.7         0.3     setosa
#> 20           5.1         3.8          1.5         0.3     setosa
#> 21           5.4         3.4          1.7         0.2     setosa
#> 22           5.1         3.7          1.5         0.4     setosa
#> 23           4.6         3.6          1.0         0.2     setosa
#> 24           5.1         3.3          1.7         0.5     setosa
#> 25           4.8         3.4          1.9         0.2     setosa
#> 26           5.0         3.0          1.6         0.2     setosa
#> 27           5.0         3.4          1.6         0.4     setosa
#> 28           5.2         3.5          1.5         0.2     setosa
#> 29           5.2         3.4          1.4         0.2     setosa
#> 30           4.7         3.2          1.6         0.2     setosa
#> 31           4.8         3.1          1.6         0.2     setosa
#> 32           5.4         3.4          1.5         0.4     setosa
#> 33           5.2         4.1          1.5         0.1     setosa
#> 35           4.9         3.1          1.5         0.2     setosa
#> 36           5.0         3.2          1.2         0.2     setosa
#> 37           5.5         3.5          1.3         0.2     setosa
#> 38           4.9         3.6          1.4         0.1     setosa
#> 39           4.4         3.0          1.3         0.2     setosa
#> 40           5.1         3.4          1.5         0.2     setosa
#> 41           5.0         3.5          1.3         0.3     setosa
#> 42           4.5         2.3          1.3         0.3     setosa
#> 43           4.4         3.2          1.3         0.2     setosa
#> 44           5.0         3.5          1.6         0.6     setosa
#> 45           5.1         3.8          1.9         0.4     setosa
#> 46           4.8         3.0          1.4         0.3     setosa
#> 47           5.1         3.8          1.6         0.2     setosa
#> 48           4.6         3.2          1.4         0.2     setosa
#> 49           5.3         3.7          1.5         0.2     setosa
#> 50           5.0         3.3          1.4         0.2     setosa
#> 53           6.9         3.1          4.9         1.5 versicolor
#> 54           5.5         2.3          4.0         1.3 versicolor
#> 55           6.5         2.8          4.6         1.5 versicolor
#> 56           5.7         2.8          4.5         1.3 versicolor
#> 58           4.9         2.4          3.3         1.0 versicolor
#> 59           6.6         2.9          4.6         1.3 versicolor
#> 60           5.2         2.7          3.9         1.4 versicolor
#> 61           5.0         2.0          3.5         1.0 versicolor
#> 62           5.9         3.0          4.2         1.5 versicolor
#> 63           6.0         2.2          4.0         1.0 versicolor
#> 64           6.1         2.9          4.7         1.4 versicolor
#> 65           5.6         2.9          3.6         1.3 versicolor
#> 67           5.6         3.0          4.5         1.5 versicolor
#> 68           5.8         2.7          4.1         1.0 versicolor
#> 69           6.2         2.2          4.5         1.5 versicolor
#> 70           5.6         2.5          3.9         1.1 versicolor
#> 71           5.9         3.2          4.8         1.8 versicolor
#> 72           6.1         2.8          4.0         1.3 versicolor
#> 73           6.3         2.5          4.9         1.5 versicolor
#> 74           6.1         2.8          4.7         1.2 versicolor
#> 75           6.4         2.9          4.3         1.3 versicolor
#> 76           6.6         3.0          4.4         1.4 versicolor
#> 78           6.7         3.0          5.0         1.7 versicolor
#> 79           6.0         2.9          4.5         1.5 versicolor
#> 81           5.5         2.4          3.8         1.1 versicolor
#> 82           5.5         2.4          3.7         1.0 versicolor
#> 83           5.8         2.7          3.9         1.2 versicolor
#> 84           6.0         2.7          5.1         1.6 versicolor
#> 85           5.4         3.0          4.5         1.5 versicolor
#> 86           6.0         3.4          4.5         1.6 versicolor
#> 87           6.7         3.1          4.7         1.5 versicolor
#> 89           5.6         3.0          4.1         1.3 versicolor
#> 90           5.5         2.5          4.0         1.3 versicolor
#> 93           5.8         2.6          4.0         1.2 versicolor
#> 94           5.0         2.3          3.3         1.0 versicolor
#> 95           5.6         2.7          4.2         1.3 versicolor
#> 96           5.7         3.0          4.2         1.2 versicolor
#> 97           5.7         2.9          4.2         1.3 versicolor
#> 98           6.2         2.9          4.3         1.3 versicolor
#> 99           5.1         2.5          3.0         1.1 versicolor
#> 100          5.7         2.8          4.1         1.3 versicolor
#> 101          6.3         3.3          6.0         2.5  virginica
#> 102          5.8         2.7          5.1         1.9  virginica
#> 105          6.5         3.0          5.8         2.2  virginica
#> 106          7.6         3.0          6.6         2.1  virginica
#> 107          4.9         2.5          4.5         1.7  virginica
#> 108          7.3         2.9          6.3         1.8  virginica
#> 109          6.7         2.5          5.8         1.8  virginica
#> 110          7.2         3.6          6.1         2.5  virginica
#> 111          6.5         3.2          5.1         2.0  virginica
#> 112          6.4         2.7          5.3         1.9  virginica
#> 113          6.8         3.0          5.5         2.1  virginica
#> 114          5.7         2.5          5.0         2.0  virginica
#> 115          5.8         2.8          5.1         2.4  virginica
#> 116          6.4         3.2          5.3         2.3  virginica
#> 117          6.5         3.0          5.5         1.8  virginica
#> 118          7.7         3.8          6.7         2.2  virginica
#> 119          7.7         2.6          6.9         2.3  virginica
#> 120          6.0         2.2          5.0         1.5  virginica
#> 121          6.9         3.2          5.7         2.3  virginica
#> 122          5.6         2.8          4.9         2.0  virginica
#> 123          7.7         2.8          6.7         2.0  virginica
#> 125          6.7         3.3          5.7         2.1  virginica
#> 126          7.2         3.2          6.0         1.8  virginica
#> 127          6.2         2.8          4.8         1.8  virginica
#> 128          6.1         3.0          4.9         1.8  virginica
#> 129          6.4         2.8          5.6         2.1  virginica
#> 130          7.2         3.0          5.8         1.6  virginica
#> 131          7.4         2.8          6.1         1.9  virginica
#> 132          7.9         3.8          6.4         2.0  virginica
#> 133          6.4         2.8          5.6         2.2  virginica
#> 134          6.3         2.8          5.1         1.5  virginica
#> 135          6.1         2.6          5.6         1.4  virginica
#> 136          7.7         3.0          6.1         2.3  virginica
#> 137          6.3         3.4          5.6         2.4  virginica
#> 138          6.4         3.1          5.5         1.8  virginica
#> 139          6.0         3.0          4.8         1.8  virginica
#> 140          6.9         3.1          5.4         2.1  virginica
#> 141          6.7         3.1          5.6         2.4  virginica
#> 142          6.9         3.1          5.1         2.3  virginica
#> 143          5.8         2.7          5.1         1.9  virginica
#> 144          6.8         3.2          5.9         2.3  virginica
#> 145          6.7         3.3          5.7         2.5  virginica
#> 146          6.7         3.0          5.2         2.3  virginica
#> 147          6.3         2.5          5.0         1.9  virginica
#> 148          6.5         3.0          5.2         2.0  virginica
#> 149          6.2         3.4          5.4         2.3  virginica
#> 150          5.9         3.0          5.1         1.8  virginica
#> 
#> $test
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 15           5.8         4.0          1.2         0.2     setosa
#> 16           5.7         4.4          1.5         0.4     setosa
#> 34           5.5         4.2          1.4         0.2     setosa
#> 51           7.0         3.2          4.7         1.4 versicolor
#> 52           6.4         3.2          4.5         1.5 versicolor
#> 57           6.3         3.3          4.7         1.6 versicolor
#> 66           6.7         3.1          4.4         1.4 versicolor
#> 77           6.8         2.8          4.8         1.4 versicolor
#> 80           5.7         2.6          3.5         1.0 versicolor
#> 88           6.3         2.3          4.4         1.3 versicolor
#> 91           5.5         2.6          4.4         1.2 versicolor
#> 92           6.1         3.0          4.6         1.4 versicolor
#> 103          7.1         3.0          5.9         2.1  virginica
#> 104          6.3         2.9          5.6         1.8  virginica
#> 124          6.3         2.7          4.9         1.8  virginica
#> 

set.seed(123)
d <- splitTrainTest(iris, p = 0.2)

str(d$train)
#> 'data.frame':    120 obs. of  5 variables:
#>  $ Sepal.Length: num  4.6 5 5.4 4.6 5 4.4 4.9 4.8 4.8 4.3 ...
#>  $ Sepal.Width : num  3.1 3.6 3.9 3.4 3.4 2.9 3.1 3.4 3 3 ...
#>  $ Petal.Length: num  1.5 1.4 1.7 1.4 1.5 1.4 1.5 1.6 1.4 1.1 ...
#>  $ Petal.Width : num  0.2 0.2 0.4 0.3 0.2 0.2 0.1 0.2 0.1 0.1 ...
#>  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
str(d$test)
#> 'data.frame':    30 obs. of  5 variables:
#>  $ Sepal.Length: num  5.1 4.9 4.7 5.4 5.1 5.7 5.2 5.2 5 4.6 ...
#>  $ Sepal.Width : num  3.5 3 3.2 3.7 3.5 3.8 3.5 4.1 3.2 3.2 ...
#>  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.5 1.5 1.2 1.4 ...
#>  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.3 0.3 0.2 0.1 0.2 0.2 ...
#>  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

# train, validation and test in a 6:2:2 ratio
d <- splitTrainTest(iris, p = c(6, 2, 2))
sapply(d, nrow)
#>      train validation       test 
#>         90         30         30 
##      train validation       test
##         90         30         30

# the same, with the species composition retained in every subset
d <- splitTrainTest(iris, p = c(6, 2, 2), strata = iris$Species)
sapply(d, function(z) table(z$Species))
#>            train validation test
#> setosa        30         10   10
#> versicolor    30         10   10
#> virginica     30         10   10
##            train validation test
## setosa        30         10   10
## versicolor    30         10   10
## virginica     30         10   10

# the memberships instead of the data
grp <- splitTrainTest(iris, p = c(6, 2, 2), output = "group")
table(grp)
#> grp
#>      train validation       test 
#>         90         30         30 

# any number of subsets, named as you like
d <- splitTrainTest(1:100, p = c(fit = 0.5, calib = 0.25, holdout = 0.25))
lengths(d)
#>     fit   calib holdout 
#>      50      25      25 
##     fit   calib holdout
##      50      25      25
```
