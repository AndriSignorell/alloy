# Los Angeles High School Attendance and Test Scores Data

A dataset containing attendance records, demographic information, and
standardized test scores for 316 high school students from two schools
in Los Angeles. This dataset is frequently used to demonstrate count
data models like Poisson and Negative Binomial regression.

## Usage

``` r
data("Lahigh")
```

## Format

A data frame with 316 rows and 10 variables:

- id:

  Student identification number.

- gender:

  Student's gender (factor/categorical variable).

- ethnic:

  Student's ethnicity (categorical variable, e.g., Hispanic, White,
  Black, Asian).

- school:

  School attended by the student (Type 1 or Type 2).

- mathpr:

  Math percentile rank from the Comprehensive Tests of Basic Skills
  (CTBS).

- langpr:

  Language arts percentile rank from the Comprehensive Tests of Basic
  Skills (CTBS).

- mathnce:

  Math Normal Curve Equivalent (NCE) score.

- langnce:

  Language arts Normal Curve Equivalent (NCE) score.

- biling:

  Bilingual status or English proficiency level (e.g., LEP for Limited
  English Proficiency).

- daysabs:

  Number of days absent during the school year (count response).

## Source

<https://stats.idre.ucla.edu/stat/data/lahigh.csv>

<https://stats.oarc.ucla.edu/stata/output/poisson-regression/>
