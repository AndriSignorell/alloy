
#' alloy: A Unified Modeling Framework for Regression and Classification
#'
#' @description
#' `alloy` provides a single, consistent interface for fitting, displaying and
#' comparing a wide range of statistical and machine-learning models.  The
#' modeler specifies *what* model they want (e.g. `"logit"`, `"weibull"`,
#' `"randomForest"`) rather than *how* to call the underlying function –
#' the package handles the rest.
#'
#' @section Main function:
#' The entry point is \code{\link{fitMod}}, which accepts a standard R formula
#' and a `fitfn` argument naming the model type.  When `fitfn` is omitted the
#' appropriate method is chosen automatically from the response variable type.
#'
#' Supported model families:
#' \describe{
#'   \item{Continuous outcome}{`lm`, `lmrob`, `gamma`, `tobit`}
#'   \item{Binary outcome}{`logit`}
#'   \item{Count outcome}{`poisson`, `quasipoisson`, `negbin`, `zeroinfl`}
#'   \item{Ordered / nominal outcome}{`polr`, `multinom`}
#'   \item{Survival outcome}{`coxph`, `weibull`, `exponential`,
#'     `lognormal`, `loglogistic`}
#'   \item{Mixed models}{`lmMixed`, `logitMixed`, `poissonMixed`,
#'     `negbinMixed`, `gammaMixed`}
#'   \item{Machine learning}{`randomForest`, `nnet`, `rpart`, `C5.0`,
#'     `svm`, `naiveBayes`, `lda`, `qda`, `glmnet`, `xgboost`}
#' }
#'
#' @section Unified output:
#' \code{\link{print.FitMod}} produces Stata-style output with confidence
#' intervals, p-values, and reference category headers, consistently across
#' all model types.  The `output` argument controls the scale:
#' \code{"coef"}, \code{"or"}, \code{"irr"}, \code{"hr"}, \code{"tr"},
#' or \code{"genuine"} (original model output).
#'
#' \code{\link{predict.FitMod}} returns a numeric vector for regression and
#' survival models, and a tidy \code{data.frame} for classifiers
#' (\code{output = "prob"}, \code{"class"}, or \code{"both"}).
#'
#' @section Additional functions:
#' \describe{
#'   \item{\code{\link{varImp}}}{Variable importance for ML models
#'     (Cleveland dot plot via \code{\link{plot.varImp}}).}
#'   \item{\code{\link{tMod}}}{Side-by-side comparison of multiple models.}
#'   \item{\code{\link{pseudoR2}}}{Pseudo-R\eqn{^2} measures for GLMs.}
#'   \item{\code{\link{vif}}}{Variance inflation factors (VIF / GVIF).}
#'   \item{\code{\link{coefCI}}, \code{\link{rSqCI}}}{Bootstrap CIs for
#'     coefficients and R\eqn{^2}.}
#'   \item{\code{\link[DescToolsX]{conf}}}{Confusion matrix and classification metrics.}
#'   \item{\code{\link{roc}}, \code{\link{bestCut}}}{ROC analysis.}
#'   \item{\code{\link{refLevel}}}{Reference levels of factor predictors.}
#'   \item{\code{\link{rules}}, \code{\link{node}}, \code{\link{cParam}},
#'     \code{\link{bestTree}}, \code{\link{leafRates}},
#'     \code{\link{splits}}}{Utilities for \code{rpart} trees.}
#' }
#'
#' @section Datasets:
#' The package ships with several benchmark datasets used in the vignette:
#' \code{\link{Admit}}, \code{\link{Apt}}, \code{\link{BioChemists}},
#' \code{\link{Contraception}}, \code{\link{Fish}}, \code{\link{IceCream}},
#' \code{\link{Lahigh}}, \code{\link{Ologit}}, \code{\link{Pima}},
#' \code{\link{Whas100}}.
#'
#' @section Design philosophy:
#' Modelers think in models, not in function calls.  `alloy` follows the
#' principle that `fitMod(y ~ x, data, fitfn = "logit")` is preferable to
#' `glm(y ~ x, data, family = "binomial")` – the intent is stated directly,
#' and the package handles package-specific quirks, defaults, and post-processing
#' transparently.  Output follows Stata conventions where applicable.
#'
#' @references
#' UCLA Statistical Methods and Data Analytics:
#' \url{https://stats.oarc.ucla.edu}
#'
#' @seealso
#' \code{\link{fitMod}}, \code{\link{print.FitMod}},
#' \code{\link{predict.FitMod}}
#'
#' @keywords internal
"_PACKAGE"



