# Lift and gain table for a fitted model or predictor vector

Computes lift and cumulative gain over the score distribution of a
binary classifier. Lift quantifies how much better a model concentrates
the positive class in the top-scored cases than random selection would,
and is the standard evaluation view wherever only a limited fraction of
the cases can be acted upon - direct marketing, fraud triage, churn
prevention.

## Usage

``` r
lift(x, resp = NULL, nBins = 10)

# S3 method for class 'Lift'
print(x, digits = 3, ...)
```

## Arguments

- x:

  Either a fitted binary classification model of class `"FitMod"` (in
  which case predicted probabilities and the response are extracted
  automatically), or a numeric vector of predicted probabilities /
  scores when `resp` is supplied.

- resp:

  Optional factor or binary vector of true class labels. If `NULL`
  (default), `x` must be a `"FitMod"` object and the response is
  extracted via [`response`](response.md).

- nBins:

  Number of equally sized score groups. The default `10` yields the
  customary decile table.

- digits:

  Number of significant digits used for printing.

- ...:

  Further arguments, currently unused.

## Value

An object of class `"Lift"`, a data frame with one row per group and the
columns:

- bin:

  group number, 1 = highest scores.

- nObs:

  number of observations in the group.

- nPos:

  number of positives in the group.

- rate:

  hit rate within the group.

- lift:

  group hit rate divided by the base rate.

- depth:

  cumulative share of all observations up to this group.

- cumPos:

  cumulative number of positives.

- cumRate:

  cumulative hit rate.

- cumLift:

  cumulative hit rate divided by the base rate.

- gain:

  cumulative share of all positives captured.

The attributes `baseRate`, `nObs`, `nPos` and `positive` carry the
overall figures.

## Details

As in [`roc`](roc.md), the second column of `predict(x, type = "prob")`
is used as the predictor when `x` is a `"FitMod"` object - the positive
class is therefore the second factor level of the response. For models
with non-standard probability output, supply the predictor vector
explicitly via `x` and `resp`.

Cases are ranked by decreasing score and cut into `nBins` groups of
equal size. Within group \\i\\, lift is the hit rate divided by the
overall base rate; cumulative lift uses the pooled hit rate over groups
\\1..i\\. A cumulative lift of 2 at depth 0.2 means the top-scored fifth
of the cases contains twice the share of positives that a random fifth
would.

Ties in the score are broken by the sort order. With heavily tied scores

- a coarse tree with few distinct leaf probabilities, for instance -
  group boundaries fall inside tie groups and the per-group lift becomes
  correspondingly unstable; the cumulative curve is unaffected in
  expectation but jagged. Reduce `nBins` in that situation rather than
  interpreting individual bins.

Incomplete cases are dropped without an `na.rm` switch: a score/response
pair is structurally unusable when either side is missing.

Lift and ROC rest on the same ranking; neither says anything about
calibration. A model can show excellent lift while its predicted
probabilities are systematically biased.

## See also

[`roc`](roc.md), [`bestCut`](bestCut.md), [`response`](response.md),
[`pharos::plotLift()`](https://andrisignorell.github.io/pharos/reference/plotLift.html)

Other roc: [`bestCut()`](bestCut.md), [`roc()`](roc.md)

## Examples

``` r
fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
lift(fitLogit)
#> Warning: 'type' is ignored for classification models in predict.FitMod; use 'output' to control the return format.
#> 
#> Lift table (positive class: 1, base rate: 0.318)
#> 
#>  bin nObs nPos  rate  lift depth cumPos cumRate cumLift  gain
#>    1   40   24 0.600 1.890   0.1     24   0.600    1.89 0.189
#>    2   40   19 0.475 1.496   0.2     43   0.537    1.69 0.339
#>    3   40   21 0.525 1.654   0.3     64   0.533    1.68 0.504
#>    4   40   11 0.275 0.866   0.4     75   0.469    1.48 0.591
#>    5   40   12 0.300 0.945   0.5     87   0.435    1.37 0.685
#>    6   40    9 0.225 0.709   0.6     96   0.400    1.26 0.756
#>    7   40    8 0.200 0.630   0.7    104   0.371    1.17 0.819
#>    8   40   14 0.350 1.102   0.8    118   0.369    1.16 0.929
#>    9   40    5 0.125 0.394   0.9    123   0.342    1.08 0.969
#>   10   40    4 0.100 0.315   1.0    127   0.318    1.00 1.000
#> 
#> n = 400, positives = 127
#> 

# Supply predictor and response directly
p <- predict(fitLogit)[, 2]
lift(p, resp = Admit$admit)
#> 
#> Lift table (positive class: 1, base rate: 0.318)
#> 
#>  bin nObs nPos  rate  lift depth cumPos cumRate cumLift  gain
#>    1   40   24 0.600 1.890   0.1     24   0.600    1.89 0.189
#>    2   40   19 0.475 1.496   0.2     43   0.537    1.69 0.339
#>    3   40   21 0.525 1.654   0.3     64   0.533    1.68 0.504
#>    4   40   11 0.275 0.866   0.4     75   0.469    1.48 0.591
#>    5   40   12 0.300 0.945   0.5     87   0.435    1.37 0.685
#>    6   40    9 0.225 0.709   0.6     96   0.400    1.26 0.756
#>    7   40    8 0.200 0.630   0.7    104   0.371    1.17 0.819
#>    8   40   14 0.350 1.102   0.8    118   0.369    1.16 0.929
#>    9   40    5 0.125 0.394   0.9    123   0.342    1.08 0.969
#>   10   40    4 0.100 0.315   1.0    127   0.318    1.00 1.000
#> 
#> n = 400, positives = 127
#> 

# Coarser grouping
lift(fitLogit, nBins = 5)
#> Warning: 'type' is ignored for classification models in predict.FitMod; use 'output' to control the return format.
#> 
#> Lift table (positive class: 1, base rate: 0.318)
#> 
#>  bin nObs nPos  rate  lift depth cumPos cumRate cumLift  gain
#>    1   80   43 0.537 1.693   0.2     43   0.537    1.69 0.339
#>    2   80   32 0.400 1.260   0.4     75   0.469    1.48 0.591
#>    3   80   21 0.263 0.827   0.6     96   0.400    1.26 0.756
#>    4   80   22 0.275 0.866   0.8    118   0.369    1.16 0.929
#>    5   80    9 0.112 0.354   1.0    127   0.318    1.00 1.000
#> 
#> n = 400, positives = 127
#> 
```
