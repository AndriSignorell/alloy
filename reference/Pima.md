# Pima Indians diabetes data

Clinical measurements for 768 Pima Indian women, used to illustrate
binary classification models. Missing values (originally coded as 0)
have been replaced with `NA` for variables where 0 is physiologically
implausible.

## Usage

``` r
Pima
```

## Format

A data frame with 768 rows and 9 variables:

- pregnant:

  Number of times pregnant.

- glucose:

  Plasma glucose concentration (2-hour oral glucose tolerance test).

- pressure:

  Diastolic blood pressure (mm Hg).

- triceps:

  Triceps skin fold thickness (mm).

- insulin:

  2-hour serum insulin (\\\mu\\U/ml).

- mass:

  Body mass index.

- pedigree:

  Diabetes pedigree function.

- age:

  Age (years).

- diabetes:

  Factor: `"neg"` (no diabetes) or `"pos"` (diabetes).

## Source

<https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database>

## References

Smith, J. W., Everhart, J. E., Dickson, W. C., Knowler, W. C., &
Johannes, R. S. (1988). Using the ADAP learning algorithm to forecast
the onset of diabetes mellitus. *Proceedings of the Annual Symposium on
Computer Application in Medical Care*, 261–265.
