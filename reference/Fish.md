# A dataset containing information about 250 groups of visitors to a state park. This dataset is the standard textbook example for demonstrating Zero-Inflated Poisson (ZIP) and Zero-Inflated Negative Binomial (ZINB) regression models, as it contains an excess number of zeros in the count response variable.

A dataset containing information about 250 groups of visitors to a state
park. This dataset is the standard textbook example for demonstrating
Zero-Inflated Poisson (ZIP) and Zero-Inflated Negative Binomial (ZINB)
regression models, as it contains an excess number of zeros in the count
response variable.

## Usage

``` r
Fish
```

## Format

A data frame with 250 rows and 6 variables:

- nofish:

  Indicator for whether the group did not fish (1 = did not fish, 0 =
  fished).

- livebait:

  Indicator for whether live bait was used (1 = yes, 0 = no).

- camper:

  Indicator for whether the group brought a camper to the park (1 = yes,
  0 = no).

- persons:

  Total number of people in the group.

- child:

  Number of children in the group.

- xb:

  Linear predictor value for the count part of the model (generated
  simulation variable).

- zg:

  Linear predictor value for the zero-inflation part of the model
  (generated simulation variable).

- count:

  The number of fish caught (count response variable; target containing
  excess zeros).

## Source

<https://stats.oarc.ucla.edu/r/dae/zip/>
