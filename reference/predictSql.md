# Translate the predictions of a model into SQL

Writes the prediction rule of a model fitted with
[`fitMod`](https://andrisignorell.github.io/alloy/reference/fitMod.md)
as SQL code, so that the model can be scored directly in a database: as
an `UPDATE` of a column, a `SELECT`, a `CREATE TABLE ... AS` or as bare
expressions to paste into a query of one's own. The arguments `output`,
`type` and `s` have the meaning and the defaults they have in
[`predict.FitMod`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md).

## Usage

``` r
predictSql(
  object,
  table,
  output = c("prob", "class", "both", "where", "leaf"),
  s = "lambda.1se",
  type = NULL,
  statement = c("update", "select", "create", "expr"),
  into = NULL,
  id = NULL,
  target = NULL,
  vars = NULL,
  dialect = getOption("alloy.sqlDialect", "ansi"),
  ...
)

# S3 method for class 'PredictSql'
print(x, ...)
```

## Arguments

- object:

  A fitted model of class `"FitMod"`.

- table:

  Name of the database table holding the predictors. Not needed for
  `statement = "expr"`.

- output:

  For classification models one of `"prob"` (default), `"class"` or
  `"both"`; for `engine = "rpart"` additionally `"where"` and `"leaf"`.
  Ignored for regression and survival models.

- s:

  For `engine = "glmnet"` only: the penalty parameter \\\lambda\\,
  default `"lambda.1se"`.

- type:

  For regression models `"response"` (default) or `"link"`; for
  `"coxph"` `"risk"` (default) or `"lp"`; for the parametric survival
  models and `"tobit"` `"response"` (default) or `"lp"`; for `"beta"`
  also `"precision"` and `"variance"`; for `"zeroinfl"` `"response"`
  (default), `"count"` or `"zero"`. For `"logit"` and `"logitFirth"` an
  explicit `"link"` or `"response"` yields a single column, as in
  [`predict.FitMod()`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md).

- statement:

  The kind of SQL to write: `"update"` (default), `"select"`, `"create"`
  or `"expr"`, see Details.

- into:

  Names of the columns the predictions are written to. If `NULL`
  (default) they are `PRED` for a single numeric prediction, `P_<level>`
  for class probabilities and `PRED_CLASS` for the predicted class.

- id:

  The key of `table`, one or several column names. For `"select"` and
  `"create"` these columns are returned along with the predictions (if
  `NULL`, the default, all columns are). For `"update"` a key turns the
  statement into a `MERGE` that computes the linear predictor only once,
  see Details; with `dialect = "exasol"` this is the default even
  without a key (the rows are then identified by `ROWID`), and
  `id = FALSE` asks for the plain `UPDATE`.

- target:

  For `statement = "create"`: name of the new table.

- vars:

  Named character vector mapping variable names of the model to column
  names of the database, e.g. `c(Sepal.Length = "SEPAL_LENGTH")`.
  Variables not listed keep their name. The column names are used as
  given (they may be quoted or qualified).

- dialect:

  SQL dialect, one of `"ansi"`, `"exasol"` or `"mssql"`; the default is
  taken from `getOption("alloy.sqlDialect", "ansi")`. `"ansi"` is
  understood by Oracle, PostgreSQL, Snowflake, DuckDB and most other
  databases. `"exasol"` guards the operations that abort a statement in
  Exasol, see Details. `"mssql"` is for Microsoft SQL Server (`LOG()`
  for the natural logarithm, `SELECT ... INTO`).

- ...:

  Not used.

- x:

  An object of class `"PredictSql"`.

## Value

A character string of class `"PredictSql"` holding the statement,
printed with [`cat()`](https://rdrr.io/r/base/cat.html); for
`statement = "expr"` a named character vector with one expression per
output column. The attribute `"exprs"` always holds these expressions.

## Details

**Supported models.** All models with a linear predictor: `"lm"`,
`"lmrob"`, `"logit"`, `"logitFirth"`, `"poisson"`, `"quasipoisson"`,
`"gamma"`, `"negbin"`, `"beta"`, `"tobit"`, `"zeroinfl"`, `"glmnet"`,
`"coxph"`, `"weibull"`, `"exponential"`, `"lognormal"`, `"loglogistic"`;
further `"multinom"`, `"polr"` and `"rpart"`. For the mixed models
(`"lmMixed"`, `"logitMixed"`, ...) the population-level prediction is
written, i.e. that of `predict(object, re.form = NA)`; a message says
so. The SQL reproduces
[`predict.FitMod()`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md)
up to floating point rounding.

Not supported are `"nnet"`, `"C5.0"`, `"lda"`, `"qda"`, `"svm"`,
`"naiveBayes"`, `"randomForest"` and `"xgboost"`; further links without
a closed form in SQL (probit), Cox models with strata, and terms that
span several columns of the design matrix
([`poly()`](https://rdrr.io/r/stats/poly.html), `ns()`, `bs()`,
matrices).

**Terms.** Within the formula the operators `+ - * / ^` and the
functions `log`, `log2`, `log10`, `log1p`, `exp`, `expm1`, `sqrt`, `abs`
and [`I()`](https://rdrr.io/r/base/AsIs.html) are translated, as are
[`factor()`](https://rdrr.io/r/base/factor.html),
[`as.factor()`](https://rdrr.io/r/base/factor.html) and
[`cut()`](https://rdrr.io/r/base/cut.html) with constant breaks.
Interactions and any contrast coding are supported: the effect of each
factor level (or combination of levels) is computed in R and written as
one `CASE` expression. Anything else stops with an error rather than
producing SQL that might be wrong. Factor levels are written as string
literals; a numeric database column behind `factor(x)` relies on the
implicit conversion of the database.

**Missing values and unknown levels.** A `NULL` in a predictor yields a
`NULL` prediction (`na.pass`). So does a factor level that the model has
not seen: it does not silently fall into the reference category. This
holds for every predictor of the model, also for those whose
coefficients are zero (aliased, or shrunk to zero by `"glmnet"`): they
stay in the SQL with a coefficient of 0. For `"rpart"` there are no
surrogate splits in SQL; a row whose split variable is `NULL` gets a
`NULL` prediction, where
[`predict()`](https://rdrr.io/r/stats/predict.html) would use a
surrogate.

**Statements.** `"update"` writes the predictions into existing columns
of `table`; the columns must exist (the `ALTER TABLE` needed is in the
attribute `"ddl"` of the result). `"select"` and `"create"` return the
predictions along with `id`. `"expr"` returns the bare expressions as a
named character vector.

**Size of the SQL.** Intermediate results - the linear predictor, the
leaf of a tree - are needed several times: the inverse link refers to
the linear predictor three times, a class probability of a multinomial
model to all of them. Without `id` each reference is written out, and
the statement grows accordingly. With `id` the intermediate results are
computed once, in nested subqueries, and referred to by name. For
`"select"` and `"create"` the subqueries are the statement itself; for
`"update"` they are the source of a
`MERGE INTO table USING (...) ON (key)`, which requires `id` to identify
the rows of `table` uniquely. So give `id` whenever the table has a key.
In Exasol no key is needed: without `id` the `MERGE` joins on the pseudo
column `ROWID`.

**Exasol.** A logarithm of a value that is not positive, a square root
of a negative value or a division by zero is an error in Exasol and
aborts the whole statement, where R returns `-Inf`, `NaN` or `Inf` for
the row concerned. With `dialect = "exasol"` these operations are
guarded (`CASE WHEN x > 0 THEN LN(x) END`, `NULLIF(x, 0)`) and yield
`NULL`, so that a single such row costs its own prediction only. The
`ALTER TABLE` statements use `ADD COLUMN IF NOT EXISTS` and can be run
repeatedly. Unquoted column names are folded to upper case by Exasol, so
the variable names of the model need no mapping as long as the columns
were created unquoted.

Coefficients are written with the 15 to 17 significant digits that
reproduce the double exactly, in exponent notation, so that every
database treats them as floating point numbers.

## See also

[`predict.FitMod`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md)

Other modelling:
[`classification-metrics`](https://andrisignorell.github.io/alloy/reference/classification-metrics.md),
[`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md),
[`logitFirth`](https://andrisignorell.github.io/alloy/reference/logitFirth.md),
[`plot.FitMod()`](https://andrisignorell.github.io/alloy/reference/plot.FitMod.md),
[`predict.FitMod()`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md),
[`predictors()`](https://andrisignorell.github.io/alloy/reference/predictors.md),
[`print.FitMod()`](https://andrisignorell.github.io/alloy/reference/print.FitMod.md),
[`regression-metrics`](https://andrisignorell.github.io/alloy/reference/regression-metrics.md)

## Examples

``` r
fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")

# probability of the second class into an existing column
predictSql(fitLogit, table = "LAB.ADMIT", type = "response",
           into = "P_ADMIT")
#> UPDATE LAB.ADMIT
#> SET P_ADMIT =
#>   CASE WHEN (-3.9899790733310319e+00
#>   + 2.2644257861791518e-03 * gre
#>   + 8.0403754928022186e-01 * gpa
#>   + CASE
#>     WHEN rank = '1' THEN 0e+00
#>     WHEN rank = '2' THEN -6.7544292796356076e-01
#>     WHEN rank = '3' THEN -1.3402039164678872e+00
#>     WHEN rank = '4' THEN -1.5514636769180703e+00
#>   END) > 7e2 THEN 1e0 WHEN (-3.9899790733310319e+00
#>   + 2.2644257861791518e-03 * gre
#>   + 8.0403754928022186e-01 * gpa
#>   + CASE
#>     WHEN rank = '1' THEN 0e+00
#>     WHEN rank = '2' THEN -6.7544292796356076e-01
#>     WHEN rank = '3' THEN -1.3402039164678872e+00
#>     WHEN rank = '4' THEN -1.5514636769180703e+00
#>   END) < -7e2 THEN 0e0 ELSE 1e0 / (1e0 + EXP(-(-3.9899790733310319e+00
#>   + 2.2644257861791518e-03 * gre
#>   + 8.0403754928022186e-01 * gpa
#>   + CASE
#>     WHEN rank = '1' THEN 0e+00
#>     WHEN rank = '2' THEN -6.7544292796356076e-01
#>     WHEN rank = '3' THEN -1.3402039164678872e+00
#>     WHEN rank = '4' THEN -1.5514636769180703e+00
#>   END))) END;

# the same by key: a MERGE that computes the linear predictor once
predictSql(fitLogit, table = "LAB.ADMIT", type = "response",
           into = "P_ADMIT", id = "ID")
#> MERGE INTO LAB.ADMIT t
#> USING (
#>   SELECT ID,
#>     CASE WHEN ETA_ > 7e2 THEN 1e0 WHEN ETA_ < -7e2 THEN 0e0 ELSE 1e0 / (1e0 + EXP(-ETA_)) END AS P_ADMIT
#>   FROM (
#>     SELECT ID,
#>       -3.9899790733310319e+00
#>       + 2.2644257861791518e-03 * gre
#>       + 8.0403754928022186e-01 * gpa
#>       + CASE
#>         WHEN rank = '1' THEN 0e+00
#>         WHEN rank = '2' THEN -6.7544292796356076e-01
#>         WHEN rank = '3' THEN -1.3402039164678872e+00
#>         WHEN rank = '4' THEN -1.5514636769180703e+00
#>       END AS ETA_
#>     FROM LAB.ADMIT
#>   ) q1
#> ) q
#> ON (t.ID = q.ID)
#> WHEN MATCHED THEN UPDATE SET
#>   P_ADMIT = q.P_ADMIT;

# both class probabilities and the class, with the key
predictSql(fitLogit, table = "LAB.ADMIT", output = "both",
           statement = "select", id = "ID")
#> SELECT ID,
#>   (1e0 - P_) AS P_0,
#>   P_ AS P_1,
#>   CASE WHEN P_ > 5e-01 THEN '1' WHEN P_ <= 5e-01 THEN '0' END AS PRED_CLASS
#> FROM (
#>   SELECT ID,
#>     ETA_,
#>     CASE WHEN ETA_ > 7e2 THEN 1e0 WHEN ETA_ < -7e2 THEN 0e0 ELSE 1e0 / (1e0 + EXP(-ETA_)) END AS P_
#>   FROM (
#>     SELECT ID,
#>       -3.9899790733310319e+00
#>       + 2.2644257861791518e-03 * gre
#>       + 8.0403754928022186e-01 * gpa
#>       + CASE
#>         WHEN rank = '1' THEN 0e+00
#>         WHEN rank = '2' THEN -6.7544292796356076e-01
#>         WHEN rank = '3' THEN -1.3402039164678872e+00
#>         WHEN rank = '4' THEN -1.5514636769180703e+00
#>       END AS ETA_
#>     FROM LAB.ADMIT
#>   ) q1
#> ) q2;

# the linear predictor as a bare expression
predictSql(fitLogit, type = "link", statement = "expr")
#> -- PRED
#> -3.9899790733310319e+00
#>   + 2.2644257861791518e-03 * gre
#>   + 8.0403754928022186e-01 * gpa
#>   + CASE
#>     WHEN rank = '1' THEN 0e+00
#>     WHEN rank = '2' THEN -6.7544292796356076e-01
#>     WHEN rank = '3' THEN -1.3402039164678872e+00
#>     WHEN rank = '4' THEN -1.5514636769180703e+00
#>   END
```
