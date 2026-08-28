# 📦 alloy

**Title:** Unified Modeling Framework for Regression and
Classification  
**License:** GPL (≥ 2)

## 🧩 Overview

`alloy` is the modelling layer of the **DescToolsX ecosystem**. One call
to [`fitMod()`](reference/fitMod.md) covers regression and
classification methods of very different origins — from linear models
through survival and mixed models to trees, forests, boosting and neural
networks — and picks a sensible method automatically from the type of
the response.

Every fitted object carries the class `"FitMod"` on top of the original
model, so [`print()`](https://rdrr.io/r/base/print.html),
[`predict()`](https://rdrr.io/r/stats/predict.html),
[`coef()`](https://rdrr.io/r/stats/coef.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) behave the same
way whatever was fitted underneath, while all methods of the original
object remain available.

A set of diagnostics for binary responses complements the fitting: the
plots that a logistic model actually needs, rather than the ones that
work for a linear one.

📖 **Documentation:** <https://andrisignorell.github.io/alloy/>

## ⚙️ Installation

``` r

install.packages("alloy")
```

Or the development version from GitHub:

``` r

remotes::install_github("AndriSignorell/alloy")
```

## 📚 Core Features

### 🔹 Model Fitting

[`fitMod()`](reference/fitMod.md) dispatches to more than thirty fitting
functions:

- Linear and generalised linear: `lm`, `logit`, `poisson`,
  `quasipoisson`, `gamma`, `negbin`, `lmrob`
- Ordinal and multinomial: `polr`, `multinom`
- Censored and survival: `tobit`, `coxph`, `weibull`, `exponential`,
  `lognormal`, `loglogistic`
- Zero-inflated counts: `zeroinfl`
- Mixed models: `lmMixed`, `logitMixed`, `poissonMixed`, `negbinMixed`,
  `gammaMixed`
- Machine learning: `rpart`, `C5.0`, `randomForest`, `xgboost`,
  `glmnet`, `svm`, `naiveBayes`, `nnet`, `lda`, `qda`

### 🔹 Unified Methods

- [`print.FitMod()`](reference/print.FitMod.md) — coefficient tables
  with confidence intervals, reference-category headers and fit
  statistics, reportable as coefficients, odds ratios, incidence rate
  ratios, hazard ratios or time ratios
- [`predict.FitMod()`](reference/predict.FitMod.md) — probabilities,
  classes or both, with column order always aligned to the response
  levels
- [`plot.FitMod()`](reference/plot.FitMod.md) — the diagnostic panels
  appropriate to the model
- [`predictors()`](reference/predictors.md),
  [`response()`](reference/response.md),
  [`refLevel()`](reference/refLevel.md)

### 🔹 Diagnostics for Logistic Models

- [`plotBinnedResid()`](reference/plotBinnedResid.md) /
  [`binnedResid()`](reference/binnedResid.md) — functional form
- [`plotCalibration()`](reference/plotCalibration.md) — are the
  predicted risks right
- [`quantileResid()`](reference/quantileResid.md) — randomised quantile
  residuals, normal under a correct model
- [`plotInfluence()`](reference/plotInfluence.md) — which observations
  drive the fit
- [`plotSeparation()`](reference/plotSeparation.md) — do the predictions
  order the outcomes
- [`plotPartialResid()`](reference/plotPartialResid.md) — linearity in
  the logit, per term
- `?model-diagnostics-overview` — which plot answers which question

### 🔹 Model Evaluation

- [`roc()`](reference/roc.md), [`bestCut()`](reference/bestCut.md),
  [`confint.roc()`](reference/confint.roc.md),
  [`lift()`](reference/lift.md)
- [`pseudoR2()`](reference/pseudoR2.md) — McFadden, Cox-Snell,
  Nagelkerke, Tjur and others
- [`rSq()`](reference/rSq.md), [`coefCI()`](reference/coefCI.md),
  [`coeffDiffCI()`](reference/coeffDiffCI.md) — with parallel bootstrap
- [`vif()`](reference/vif.md) — VIF and generalised VIF
- [`varImp()`](reference/varImp.md),
  [`plot.varImp()`](reference/plot.varImp.md)
- [`splitTrainTest()`](reference/splitTrainTest.md)

### 🔹 Trees

- [`bestTree()`](reference/bestTree.md) (1-SE rule),
  [`cParam()`](reference/cParam.md),
  [`leafRates()`](reference/leafRates.md),
  [`node()`](reference/node.md), [`rules()`](reference/rules.md),
  [`splits()`](reference/splits.md),
  [`plot.rpart()`](reference/plot.rpart.md)

### 🔹 Model Comparison

- [`tMod()`](reference/tMod.md) — several models side by side, in a
  table or a plot
- [`tmodSummary()`](reference/tmodSummary.md) — the S3 generic behind it

### 🔹 Datasets

Teaching datasets for the model families: `Admit`, `Apt`, `BioChemists`,
`Contraception`, `Fish`, `IceCream`, `Lahigh`, `Ologit`, `Pima`,
`Whas100`.

## 🚀 Design Principles

- **One interface** — the same call, the same output, whatever is fitted
  underneath
- **Non-destructive** — `"FitMod"` layers on top of the original object;
  nothing of the underlying model is lost
- **Diagnostics that fit the model** — binary responses get the plots
  that work for them
- **Fast** — bootstrap routines implemented in C++ via Rcpp,
  RcppArmadillo and RcppParallel

## 🧪 Example

``` r

library(alloy)

# method chosen automatically from the response
fitMod(Sepal.Length ~ ., data = iris)

# explicit, and reported as odds ratios
fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
print(fitLogit, output = "or")

# the diagnostics a logistic model actually needs
op <- par(mfrow = c(2, 3))
plot(fitLogit)
par(op)

# several models side by side
tMod(lm(mpg ~ wt, mtcars), lm(mpg ~ wt + hp, mtcars))
```

## 🧱 The Suite

`alloy` builds on `bedrock` (base utilities), `pharos` (graphics) and
`DescToolsX` (descriptive statistics); the formal counterparts to its
diagnostic plots live in `lumen`.

## 🙏 Acknowledgements

Parts of the code and documentation were reviewed with the help of large
language models (OpenAI Codex, Anthropic Claude). Every suggestion was
assessed, edited and verified by the maintainer, who remains solely
responsible for the content of this package.

## 📜 License

GPL (≥ 2)
