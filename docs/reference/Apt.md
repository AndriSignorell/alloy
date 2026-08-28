# Academic aptitude scores (tobit example)

Scores on an academic aptitude test for 200 high school students,
right-censored at 800. Used to illustrate tobit regression.

## Usage

``` r
Apt
```

## Format

A data frame with 200 rows and 4 variables:

- apt:

  Academic aptitude score (censored at 800).

- id:

  ID for the student

- read:

  Reading score.

- math:

  Math score.

- prog:

  Factor: academic programme (`"academic"`, `"general"`,
  `"vocational"`).

## Source

<https://stats.oarc.ucla.edu/stata/dae/tobit-analysis/>
