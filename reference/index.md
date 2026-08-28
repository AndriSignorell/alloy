# Package index

## Model Fitting & Interface

The unified fitting function and the methods that behave the same way
whatever model was fitted underneath.

- [`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md)
  : Fit a statistical or machine-learning model with automatic method
  selection
- [`print(`*`<FitMod>`*`)`](https://andrisignorell.github.io/alloy/reference/print.FitMod.md)
  : Print method for FitMod objects
- [`predict(`*`<FitMod>`*`)`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md)
  : Predict method for FitMod objects
- [`plot(`*`<FitMod>`*`)`](https://andrisignorell.github.io/alloy/reference/plot.FitMod.md)
  : Diagnostic Plots for FitMod Models
- [`predictors()`](https://andrisignorell.github.io/alloy/reference/predictors.md)
  : Predictors of a Fitted Model
- [`response()`](https://andrisignorell.github.io/alloy/reference/response.md)
  : Extract the response variable from a fitted model
- [`refLevel()`](https://andrisignorell.github.io/alloy/reference/refLevel.md)
  : Reference level of factor predictors in a model

## Diagnostics for Binary Responses

The plots a logistic model needs, where the residual-versus-fitted plot
of a linear model says nothing.

- [`model-diagnostics-overview`](https://andrisignorell.github.io/alloy/reference/model-diagnostics-overview.md)
  : Diagnostics for Logistic Models in alloy
- [`plotBinnedResid()`](https://andrisignorell.github.io/alloy/reference/plotBinnedResid.md)
  : Binned Residual Plot
- [`binnedResid()`](https://andrisignorell.github.io/alloy/reference/binnedResid.md)
  : Binned Residuals
- [`panelBinnedResid()`](https://andrisignorell.github.io/alloy/reference/panelBinnedResid.md)
  : Facet Panel for Binned Residuals
- [`plotPartialResid()`](https://andrisignorell.github.io/alloy/reference/plotPartialResid.md)
  : Partial Residual Plot for Linearity in the Logit
- [`plotCalibration()`](https://andrisignorell.github.io/alloy/reference/plotCalibration.md)
  : Calibration Curve
- [`quantileResid()`](https://andrisignorell.github.io/alloy/reference/quantileResid.md)
  : Randomized Quantile Residuals
- [`plotInfluence()`](https://andrisignorell.github.io/alloy/reference/plotInfluence.md)
  : Influence Plot for Logistic Models
- [`plotSeparation()`](https://andrisignorell.github.io/alloy/reference/plotSeparation.md)
  : Separation Plot

## Discrimination & Lift

ROC curves, optimal cut-points, and the decile view used wherever only a
fraction of the cases can be acted upon.

- [`roc()`](https://andrisignorell.github.io/alloy/reference/roc.md) :
  ROC curve for a fitted model or predictor vector
- [`confint(`*`<roc>`*`)`](https://andrisignorell.github.io/alloy/reference/confint.roc.md)
  : Confidence intervals for ROC curve coordinates
- [`bestCut()`](https://andrisignorell.github.io/alloy/reference/bestCut.md)
  : Best cut-point of an ROC curve
- [`lift()`](https://andrisignorell.github.io/alloy/reference/lift.md)
  [`print(`*`<Lift>`*`)`](https://andrisignorell.github.io/alloy/reference/lift.md)
  : Lift and gain table for a fitted model or predictor vector

## Model Fit & Coefficients

Goodness-of-fit measures, coefficient intervals, collinearity, and
variable importance.

- [`pseudoR2()`](https://andrisignorell.github.io/alloy/reference/pseudoR2.md)
  : Pseudo R-Squared Measures for Regression Models
- [`rSq()`](https://andrisignorell.github.io/alloy/reference/rSq.md) :
  R-squared of a Linear Model
- [`coefCI()`](https://andrisignorell.github.io/alloy/reference/coefCI.md)
  : Bootstrap Confidence Intervals for Linear Model Coefficients
- [`coeffDiffCI()`](https://andrisignorell.github.io/alloy/reference/coeffDiffCI.md)
  : Confidence Interval for the Difference of Two Regression
  Coefficients
- [`vif()`](https://andrisignorell.github.io/alloy/reference/vif.md) :
  Variance Inflation Factors (VIF / GVIF)
- [`varImp()`](https://andrisignorell.github.io/alloy/reference/varImp.md)
  : Variable importance for machine learning models
- [`plot(`*`<varImp>`*`)`](https://andrisignorell.github.io/alloy/reference/plot.varImp.md)
  : Cleveland dot plot for variable importance

## Trees

Pruning, node structure, decision rules, and plotting of rpart trees.

- [`bestTree()`](https://andrisignorell.github.io/alloy/reference/bestTree.md)
  : Best tree size using the 1-SE rule
- [`cParam()`](https://andrisignorell.github.io/alloy/reference/cParam.md)
  : Complexity parameter table for an rpart tree
- [`node()`](https://andrisignorell.github.io/alloy/reference/node.md) :
  Structured node information for an rpart tree
- [`splits()`](https://andrisignorell.github.io/alloy/reference/splits.md)
  : Split labels for each node of an rpart tree
- [`rules()`](https://andrisignorell.github.io/alloy/reference/rules.md)
  : Decision rules for an rpart tree
- [`leafRates()`](https://andrisignorell.github.io/alloy/reference/leafRates.md)
  : Misclassification rates per leaf node
- [`plot(`*`<rpart>`*`)`](https://andrisignorell.github.io/alloy/reference/plot.rpart.md)
  : Plot an rpart tree using rpart.plot with node labels

## Model Comparison

Several fitted models side by side, in a table or a plot.

- [`tMod()`](https://andrisignorell.github.io/alloy/reference/tMod.md)
  [`print(`*`<TMod>`*`)`](https://andrisignorell.github.io/alloy/reference/tMod.md)
  [`plot(`*`<TMod>`*`)`](https://andrisignorell.github.io/alloy/reference/tMod.md)
  : Compare multiple statistical models
- [`tmodSummary()`](https://andrisignorell.github.io/alloy/reference/tmodSummary.md)
  : Extract model summaries for model comparison

## Data Splitting & Utilities

Training and test sets, and methods filling gaps in other packages.

- [`splitTrainTest()`](https://andrisignorell.github.io/alloy/reference/splitTrainTest.md)
  : Split Data into Training and Test Sets
- [`model.matrix(`*`<gls>`*`)`](https://andrisignorell.github.io/alloy/reference/model.matrix.gls.md)
  : Model matrix for gls objects

## Datasets

Teaching datasets illustrating the model families covered by fitMod().

- [`Admit`](https://andrisignorell.github.io/alloy/reference/Admit.md) :
  Graduate school admissions
- [`Pima`](https://andrisignorell.github.io/alloy/reference/Pima.md) :
  Pima Indians diabetes data
- [`BioChemists`](https://andrisignorell.github.io/alloy/reference/BioChemists.md)
  : Biochemists' publication counts
- [`Fish`](https://andrisignorell.github.io/alloy/reference/Fish.md) : A
  dataset containing information about 250 groups of visitors to a state
  park. This dataset is the standard textbook example for demonstrating
  Zero-Inflated Poisson (ZIP) and Zero-Inflated Negative Binomial (ZINB)
  regression models, as it contains an excess number of zeros in the
  count response variable.
- [`Lahigh`](https://andrisignorell.github.io/alloy/reference/Lahigh.md)
  : Los Angeles High School Attendance and Test Scores Data
- [`IceCream`](https://andrisignorell.github.io/alloy/reference/IceCream.md)
  : Ice cream flavour preference
- [`Ologit`](https://andrisignorell.github.io/alloy/reference/Ologit.md)
  : Ordinal outcome: applying to graduate school
- [`Apt`](https://andrisignorell.github.io/alloy/reference/Apt.md) :
  Academic aptitude scores (tobit example)
- [`Whas100`](https://andrisignorell.github.io/alloy/reference/Whas100.md)
  : Worcester Heart Attack Study (WHAS100)
- [`Contraception`](https://andrisignorell.github.io/alloy/reference/Contraception.md)
  : Contraceptive use in Bangladesh
