# Worcester Heart Attack Study (WHAS100)

Follow-up data for 100 patients hospitalised with acute myocardial
infarction, used to illustrate Cox proportional hazards and parametric
survival models.

## Usage

``` r
Whas100
```

## Format

A data frame with 100 rows and 14 variables:

- id:

  Subject identification number.

- addate:

  Admission date to the hospital.

- foldate:

  Follow-up date.

- hosstay:

  Length of hospital stay (in days).

- foltime:

  Follow-up time from hospital admission to follow-up date (in days).

- folstatus:

  Vital status at follow-up (0 = alive, 1 = dead; event indicator).

- age:

  Age at hospital admission (in years).

- gender:

  Gender of the patient (0 = male, 1 = female).

- bmi:

  Body Mass Index (kg/m\\^2\\).

- agex:

  Age group (factor), derived from `age` via
  [`cutAge`](https://andrisignorell.github.io/DescToolsX/reference/cutAge.html).

## Source

<https://stats.oarc.ucla.edu/stata/examples/asa2/>

## References

Hosmer, D. W., Lemeshow, S., & May, S. (2008). *Applied Survival
Analysis*, 2nd ed. Wiley.
