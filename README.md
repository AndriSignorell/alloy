# 📦 alloy <img src="man/figures/logo.png" align="right" height="139" alt="alloy logo" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/alloy)](https://CRAN.R-project.org/package=alloy)
[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
<!-- badges: end -->

**Title:** Unified Modeling Framework for Regression and Classification\
**License:** GPL (≥ 2)

## 🧩 Overview

`alloy` is the modelling layer of the **DescToolsX ecosystem**. One call
to `fitMod()` covers regression and classification methods of very
different origins — from linear models through survival and mixed models
to trees, forests, boosting and neural networks — and picks a sensible
method automatically from the type of the response.

Every fitted object carries the class `"FitMod"` on top of the original
model, so `print()`, `predict()`, `coef()` and `plot()` behave the same
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

`fitMod()` dispatches to more than thirty fitting functions:

-   Linear and generalised linear: `lm`, `logit`, `poisson`,
    `quasipoisson`, `gamma`, `negbin`, `lmrob`
-   Ordinal and multinomial: `polr`, `multinom`
-   Censored and survival: `tobit`, `coxph`, `weibull`, `exponential`,
    `lognormal`, `loglogistic`
-   Zero-inflated counts: `zeroinfl`
-   Mixed models: `lmMixed`, `logitMixed`, `poissonMixed`,
    `negbinMixed`, `gammaMixed`
-   Machine learning: `rpart`, `C5.0`, `randomForest`, `xgboost`,
    `glmnet`, `svm`, `naiveBayes`, `nnet`, `lda`, `qda`

### 🔹 Unified Methods

-   `print.FitMod()` — coefficient tables with confidence intervals,
    reference-category headers and fit statistics, reportable as
    coefficients, odds ratios, incidence rate ratios, hazard ratios or
    time ratios
-   `predict.FitMod()` — probabilities, classes or both, with column
    order always aligned to the response levels
-   `plot.FitMod()` — the diagnostic panels appropriate to the model
-   `predictors()`, `response()`, `refLevel()`

### 🔹 Diagnostics for Logistic Models

-   `plotBinnedResid()` / `binnedResid()` — functional form
-   `plotCalibration()` — are the predicted risks right
-   `quantileResid()` — randomised quantile residuals, normal under a
    correct model
-   `plotInfluence()` — which observations drive the fit
-   `plotSeparation()` — do the predictions order the outcomes
-   `plotPartialResid()` — linearity in the logit, per term
-   `?model-diagnostics-overview` — which plot answers which question

### 🔹 Model Evaluation

-   `roc()`, `bestCut()`, `confint.roc()`, `lift()`
-   `pseudoR2()` — McFadden, Cox-Snell, Nagelkerke, Tjur and others
-   `rSq()`, `coefCI()`, `coeffDiffCI()` — with parallel bootstrap
-   `vif()` — VIF and generalised VIF
-   `varImp()`, `plot.varImp()`
-   `splitTrainTest()`

### 🔹 Trees

-   `bestTree()` (1-SE rule), `cParam()`, `leafRates()`, `node()`,
    `rules()`, `splits()`, `plot.rpart()`

### 🔹 Model Comparison

-   `tMod()` — several models side by side, in a table or a plot
-   `tmodSummary()` — the S3 generic behind it

### 🔹 Datasets

Teaching datasets for the model families: `Admit`, `Apt`, `BioChemists`,
`Contraception`, `Fish`, `IceCream`, `Lahigh`, `Ologit`, `Pima`,
`Whas100`.

## 🚀 Design Principles

-   **One interface** — the same call, the same output, whatever is
    fitted underneath
-   **Non-destructive** — `"FitMod"` layers on top of the original
    object; nothing of the underlying model is lost
-   **Diagnostics that fit the model** — binary responses get the plots
    that work for them
-   **Fast** — bootstrap routines implemented in C++ via Rcpp,
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
