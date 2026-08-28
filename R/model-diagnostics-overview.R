#' Diagnostics for Logistic Models in alloy
#'
#' @description
#' A fitted logistic model cannot be checked with the plots that work for
#' a linear one. With a binary response the residuals carry no scatter to
#' inspect - they lie on the two curves \eqn{-\hat p} and \eqn{1-\hat p} -
#' so every diagnostic here either aggregates observations before looking
#' at them, or transforms them into something that is continuous under the
#' model.
#'
#' **Panels of `plot()`** — drawn by [plot.FitMod()] via `which`:
#'
#' | Panel | Function | Question |
#' |---|---|---|
#' | 1 | [plotBinnedResid()] | is the functional form right |
#' | 2 | [plotCalibration()] | are the predicted risks right |
#' | 3 | [quantileResid()] + `plotQQ()` | is the whole distribution right |
#' | 4 | [plotInfluence()] | which observations drive the fit |
#' | 5 | [plotSeparation()] | do the predictions order the outcomes |
#'
#' **Called directly** — one panel per term, so no fixed panel number:
#'
#' | Function | Question |
#' |---|---|
#' | [plotPartialResid()] | is this term linear in the logit |
#' | [plotBinnedResid()] with `var` | where in this predictor does the fit break |
#'
#' @section Choosing among them:
#' The three that answer different questions are the calibration curve,
#' the binned residuals per predictor, and the influence plot: level,
#' shape, and individual observations. The others refine rather than add.
#'
#' A ROC curve is not in this list on purpose. It measures discrimination
#' and is invariant to any monotone transformation of the predicted
#' probabilities, so it cannot detect miscalibration and barely responds
#' to a misspecified functional form. It answers a real question, just not
#' this one - see [roc()] and [cStat()].
#'
#' The formal counterparts to these plots live in \pkg{lumen}:
#' `hosmerLemeshowTest()` for the grouped goodness-of-fit test that panel 2
#' shows graphically, and `leCessieTest()` for the smoothed-residual test.
#' A test returns one p-value; the plot says where the misfit is, which is
#' the part that tells you what to change.
#'
#' @section On the development sample:
#' Calibration intercept and slope are 0 and 1 by construction when a
#' logistic model is evaluated on the data it was fitted to, so their
#' in-sample values say nothing about overfitting. Pass `newdata` to
#' [plotCalibration()], or resample, before reading them as evidence.
#'
#' @concept regression-diagnostics
#' @concept binary-outcome
#' @concept calibration
#'
#' @name model-diagnostics-overview
NULL
