# Package index

## Model Fitting & Interface

The unified fitting function and the methods that behave the same way
whatever model was fitted underneath.

- [`fitMod()`](fitMod.md) : Fit a statistical or machine-learning model
  with automatic method selection
- [`print(`*`<FitMod>`*`)`](print.FitMod.md) : Print method for FitMod
  objects
- [`predict(`*`<FitMod>`*`)`](predict.FitMod.md) : Predict method for
  FitMod objects
- [`plot(`*`<FitMod>`*`)`](plot.FitMod.md) : Diagnostic Plots for FitMod
  Models
- [`predictors()`](predictors.md) : Predictors of a Fitted Model
- [`response()`](response.md) : Extract the response variable from a
  fitted model
- [`refLevel()`](refLevel.md) : Reference level of factor predictors in
  a model

## Diagnostics for Binary Responses

The plots a logistic model needs, where the residual-versus-fitted plot
of a linear model says nothing.

- [`model-diagnostics-overview`](model-diagnostics-overview.md) :
  Diagnostics for Logistic Models in alloy
- [`plotBinnedResid()`](plotBinnedResid.md) : Binned Residual Plot
- [`binnedResid()`](binnedResid.md) : Binned Residuals
- [`panelBinnedResid()`](panelBinnedResid.md) : Facet Panel for Binned
  Residuals
- [`plotPartialResid()`](plotPartialResid.md) : Partial Residual Plot
  for Linearity in the Logit
- [`plotCalibration()`](plotCalibration.md) : Calibration Curve
- [`quantileResid()`](quantileResid.md) : Randomized Quantile Residuals
- [`plotInfluence()`](plotInfluence.md) : Influence Plot for Logistic
  Models
- [`plotSeparation()`](plotSeparation.md) : Separation Plot

## Discrimination & Lift

ROC curves, optimal cut-points, and the decile view used wherever only a
fraction of the cases can be acted upon.

- [`roc()`](roc.md) : ROC curve for a fitted model or predictor vector
- [`confint(`*`<roc>`*`)`](confint.roc.md) : Confidence intervals for
  ROC curve coordinates
- [`bestCut()`](bestCut.md) : Best cut-point of an ROC curve
- [`lift()`](lift.md) [`print(`*`<Lift>`*`)`](lift.md) : Lift and gain
  table for a fitted model or predictor vector

## Model Fit & Coefficients

Goodness-of-fit measures, coefficient intervals, collinearity, and
variable importance.

- [`pseudoR2()`](pseudoR2.md) : Pseudo R-Squared Measures for Regression
  Models
- [`rSq()`](rSq.md) : R-squared of a Linear Model
- [`coefCI()`](coefCI.md) : Bootstrap Confidence Intervals for Linear
  Model Coefficients
- [`coeffDiffCI()`](coeffDiffCI.md) : Confidence Interval for the
  Difference of Two Regression Coefficients
- [`vif()`](vif.md) : Variance Inflation Factors (VIF / GVIF)
- [`varImp()`](varImp.md) : Variable importance for machine learning
  models
- [`plot(`*`<varImp>`*`)`](plot.varImp.md) : Cleveland dot plot for
  variable importance

## Trees

Pruning, node structure, decision rules, and plotting of rpart trees.

- [`bestTree()`](bestTree.md) : Best tree size using the 1-SE rule
- [`cParam()`](cParam.md) : Complexity parameter table for an rpart tree
- [`node()`](node.md) : Structured node information for an rpart tree
- [`splits()`](splits.md) : Split labels for each node of an rpart tree
- [`rules()`](rules.md) : Decision rules for an rpart tree
- [`leafRates()`](leafRates.md) : Misclassification rates per leaf node
- [`plot(`*`<rpart>`*`)`](plot.rpart.md) : Plot an rpart tree using
  rpart.plot with node labels

## Model Comparison

Several fitted models side by side, in a table or a plot.

- [`tMod()`](tMod.md) [`print(`*`<TMod>`*`)`](tMod.md)
  [`plot(`*`<TMod>`*`)`](tMod.md) : Compare multiple statistical models
- [`tmodSummary()`](tmodSummary.md) : Extract model summaries for model
  comparison

## Data Splitting & Utilities

Training and test sets, and methods filling gaps in other packages.

- [`splitTrainTest()`](splitTrainTest.md) : Split Data into Training and
  Test Sets
- [`model.matrix(`*`<gls>`*`)`](model.matrix.gls.md) : Model matrix for
  gls objects

## Datasets

Teaching datasets illustrating the model families covered by fitMod().

- [`Admit`](Admit.md) : Graduate school admissions
- [`Pima`](Pima.md) : Pima Indians diabetes data
- [`BioChemists`](BioChemists.md) : Biochemists' publication counts
- [`Fish`](Fish.md) : A dataset containing information about 250 groups
  of visitors to a state park. This dataset is the standard textbook
  example for demonstrating Zero-Inflated Poisson (ZIP) and
  Zero-Inflated Negative Binomial (ZINB) regression models, as it
  contains an excess number of zeros in the count response variable.
- [`Lahigh`](Lahigh.md) : Los Angeles High School Attendance and Test
  Scores Data
- [`IceCream`](IceCream.md) : Ice cream flavour preference
- [`Ologit`](Ologit.md) : Ordinal outcome: applying to graduate school
- [`Apt`](Apt.md) : Academic aptitude scores (tobit example)
- [`Whas100`](Whas100.md) : Worcester Heart Attack Study (WHAS100)
- [`Contraception`](Contraception.md) : Contraceptive use in Bangladesh
