# =========================================================================
# Internal helpers for the logistic model diagnostics
# =========================================================================


# -------------------------------------------------------------------------
# Extract the binomial parts of a fitted model
# -------------------------------------------------------------------------

# Returns the raw glm together with the quantities every diagnostic needs:
# the fitted probabilities, the response as a proportion of successes, the
# binomial denominators (prior weights) and the number of observations.
#
# For a binomial glm, fit$y holds the response as a *proportion* and
# fit$prior.weights the number of trials, so the success counts are y * m.
# This is the only place that knowledge is encoded.
#' @keywords internal
.logitParts <- function(x, argName = "x") {

  fit <- .stripFitMod(x)

  if (!inherits(fit, "glm") ||
      !(fit$family$family %in% c("binomial", "quasibinomial")))
    stop(gettextf(
      "'%s' must be a fitted logistic model (engine = \"logit\")", argName),
      domain = NA)

  p <- stats::fitted(fit)

  m <- fit$prior.weights
  if (is.null(m)) m <- rep(1, length(p))

  y <- if (is.null(fit$y)) .numericResponse(fit) else as.numeric(fit$y)

  list(
    fit = fit,
    p   = p,
    y   = y,
    m   = as.numeric(m),
    n   = length(p)
  )
}


# Peel the FitMod layer off, leaving the natural model object. Works for a
# plain glm as well, so every diagnostic accepts both.
#' @keywords internal
.stripFitMod <- function(x) {
  if (!inherits(x, "FitMod"))
    return(x)
  if (inherits(x, "FitMod.lme4") || inherits(x, "FitMod.xgboost"))
    return(x$model)
  class(x) <- setdiff(class(x), "FitMod")
  x
}


# Response as numeric 0/1, for objects that do not carry $y
#' @keywords internal
.numericResponse <- function(fit) {
  res <- stats::model.response(stats::model.frame(fit))
  if (is.factor(res))  res <- as.integer(res) - 1L
  if (is.logical(res)) res <- as.integer(res)
  as.numeric(res)
}


# -------------------------------------------------------------------------
# Binning
# -------------------------------------------------------------------------

# Number of bins, following Gelman and Hill (2007): sqrt(n) once there is
# enough data for it, a fixed 10 in the middle range, and n / 2 for very
# small samples, where sqrt(n) would leave one point per bin.
#' @keywords internal
.defaultBins <- function(n) {
  if (n >= 100L)     floor(sqrt(n))
  else if (n > 10L)  10L
  else               max(2L, floor(n / 2))
}


# Split a binning variable into groups.
#
# A factor is grouped by its levels - binning a factor by quantiles would
# be meaningless, and one bin per level is exactly what is wanted. A
# numeric variable is cut at its own quantiles, so the bins carry roughly
# equal counts rather than equal width; ties can collapse breaks, which is
# reported rather than silently accepted.
#' @keywords internal
.binGroups <- function(x, nBins) {

  if (is.factor(x) || is.character(x) || is.logical(x))
    return(factor(x))

  brk <- unique(stats::quantile(x, probs = seq(0, 1, length.out = nBins + 1L),
                                na.rm = TRUE, names = FALSE))

  if (length(brk) - 1L < 2L)
    stop("the binning variable has too few distinct values for 2 bins")

  if (length(brk) - 1L < nBins)
    warning(gettextf(
      "only %d distinct bins could be formed instead of the requested %d",
      length(brk) - 1L, nBins), domain = NA)

  cut(x, breaks = brk, include.lowest = TRUE)
}


# Aggregate residuals within bins.
#
# se is the standard error of the *mean* residual in the bin. Under the
# model the per-observation variance of y - p is p (1 - p) / m, so the
# model-based standard error is the exact binomial one and does not need
# the bin to be large; the empirical version estimates the same quantity
# from the spread within the bin and is the more sceptical choice, since
# it does not assume the model is right.
#' @keywords internal
.binStats <- function(bin, xVal, resid, p, m, method, z) {

  keep  <- !is.na(bin) & !is.na(resid) & !is.na(xVal)
  bin   <- droplevels(factor(bin[keep]))
  xVal  <- xVal[keep]
  resid <- resid[keep]
  p     <- p[keep]
  m     <- m[keep]

  n  <- as.vector(tapply(resid, bin, length))
  xm <- as.vector(tapply(xVal,  bin, mean))
  ym <- as.vector(tapply(resid, bin, mean))

  se <- switch(
    method,
    model = sqrt(as.vector(tapply(p * (1 - p) / m, bin, mean)) / n),
    empirical = {
      s <- as.vector(tapply(resid, bin, stats::sd))
      s / sqrt(n)
    }
  )

  if (method == "empirical" && any(n < 2L))
    warning("bins with a single observation have no empirical standard error")

  data.frame(
    bin   = levels(bin),
    x     = xm,
    y     = ym,
    n     = n,
    se    = se,
    lci   = -z * se,
    uci   =  z * se,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


# -------------------------------------------------------------------------
# Probabilities on the logit scale
# -------------------------------------------------------------------------

# Fitted probabilities of exactly 0 or 1 (complete separation, or simply a
# very sharp model) have an infinite logit. Clamping keeps the calibration
# regression finite; it is a deliberate change of the data and therefore
# reported, never silent. The message carries no count, so that the same
# warning from many folds of crossVal() is collected into one line; the
# upper bound is written as 1 - eps, which %g would round to 1.
#' @keywords internal
.clampProb <- function(p, eps = 1e-10) {
  hit <- p < eps | p > 1 - eps
  if (any(hit))
    warning(gettextf(
      "fitted probabilities of 0 or 1 were clamped to [%g, 1 - %g] to keep the logit finite",
      eps, eps), call. = FALSE, domain = NA)
  pmin(pmax(p, eps), 1 - eps)
}


# Calibration-in-the-large and calibration slope: logistic regressions of
# the binary response on the linear predictor logit(p), as an offset and as
# the only regressor. Shared by plotCalibration() and crossVal().
#
# The two fits are independent. The slope is NA if the logits of the two
# classes do not overlap (complete or quasi-complete separation: the ML
# estimate does not exist) or are constant (not identifiable); either fit
# is NA if it fails or does not converge. Each case raises a warning, and
# the warnings of glm.fit() are passed on, not muffled.
#' @keywords internal
.calibrationCoefs <- function(p, y) {

  eta <- stats::qlogis(.clampProb(p))
  one <- matrix(1, length(y))

  calFit <- function(x, offset, coefIdx, what) {
    fit <- tryCatch(
      stats::glm.fit(x, y, offset = offset, family = stats::binomial()),
      error = function(e) {
        warning(gettextf("the %s fit failed: %s", what, conditionMessage(e)),
                call. = FALSE)
        NULL
      })
    if (is.null(fit))
      return(NA_real_)
    if (!fit$converged || fit$boundary) {
      warning(gettextf("the %s fit did not converge; set to NA.", what),
              call. = FALSE)
      return(NA_real_)
    }
    unname(fit$coefficients[coefIdx])
  }

  a <- eta[y == 0]
  b <- eta[y == 1]

  slope <- if (diff(range(eta)) == 0) {
    warning("constant predictions: the calibration slope is not ",
            "identifiable; set to NA.", call. = FALSE)
    NA_real_
  } else if (length(a) == 0L || length(b) == 0L) {
    warning("a single class: the calibration slope is not identifiable; ",
            "set to NA.", call. = FALSE)
    NA_real_
  } else if (max(a) <= min(b) || max(b) <= min(a)) {
    warning("the predictions separate the classes: the calibration slope ",
            "does not exist; set to NA.", call. = FALSE)
    NA_real_
  } else {
    calFit(cbind(one, eta), NULL, 2L, "calibration slope")
  }

  list(intercept = calFit(one, eta, 1L, "calibration intercept"),
       slope     = slope)
}


# -------------------------------------------------------------------------
# Loess smoother for the diagnostic plots
# -------------------------------------------------------------------------

# Local linear loess of y on x (weights w), as drawn by plotCalibration()
# and plotPartialResid().
# The default surface = "interpolate" evaluates the fit only at the vertices
# of a k-d tree and interpolates between them; with many observations the
# curve then shows kinks at the cell boundaries, which read like breakpoints
# of the data. Up to maxDirect observations the fit is computed directly at
# every point (a few seconds for 10,000), beyond that the cells are made
# finer.
#' @keywords internal
.loessSmooth <- function(x, y, w = NULL, maxDirect = 20000L) {
  ctrl <- if (length(x) <= maxDirect) stats::loess.control(surface = "direct")
          else stats::loess.control(cell = 0.02)
  # do.call() hands over the values: written as an argument, 'weights'
  # would be looked up by model.frame() and could find stats::weights
  do.call(stats::loess,
          list(formula = y ~ x, data = data.frame(x = x, y = y), weights = w,
               degree = 1L, family = "gaussian", control = ctrl))
}


# -------------------------------------------------------------------------
# A default title that fits
# -------------------------------------------------------------------------

# deparse1(formula(fit)) is unusable as a title as soon as a model has more
# than a handful of terms - it runs off both edges of the panel and takes
# the response with it. Shortening is done on the terms, not on the
# characters: the response and as many predictors as fit are kept, the rest
# are counted. That is device independent, so the title does not change
# when the plot is resized, and the two things a reader needs - what is
# being modelled and how big the model is - always survive.
#' @keywords internal
.modelTitle <- function(fit, prefix, width = 54L) {

  # a formula as well (crossVal() keeps no model); one with "." cannot be
  # expanded without its data and is shown as written
  fo  <- if (inherits(fit, "formula")) fit else stats::formula(fit)
  lhs <- deparse1(fo[[2L]])
  rhs <- tryCatch(attr(stats::terms(fit), "term.labels"),
                  error = function(e) deparse1(fo[[3L]]))

  if (!length(rhs))
    return(paste0(prefix, ": ", lhs, " ~ 1"))

  full <- paste(lhs, "~", paste(rhs, collapse = " + "))

  if (nchar(full) <= width)
    return(paste0(prefix, ": ", full))

  # room for the " + n more" tail, whose width depends on the count
  budget <- width - nchar(lhs) - 3L - nchar(length(rhs)) - 8L

  used <- 0L
  keep <- 0L

  for (i in seq_along(rhs)) {
    add <- nchar(rhs[i]) + if (i > 1L) 3L else 0L
    if (used + add > budget) break
    used <- used + add
    keep <- i
  }

  if (keep == 0L)
    return(sprintf("%s: %s ~ %d terms", prefix, lhs, length(rhs)))

  sprintf("%s: %s ~ %s + %d more", prefix, lhs,
          paste(rhs[seq_len(keep)], collapse = " + "), length(rhs) - keep)
}


# -------------------------------------------------------------------------
# Rounding for display
# -------------------------------------------------------------------------

# zapsmall() is no help for a lone value: it rounds relative to the largest
# magnitude in its argument, which for a single number is that number
# itself, so nothing is ever zapped. Rounding to the digits actually shown
# is what is meant here. The guard catches negative zero, which round()
# produces from a small negative value and which would print with a leading
# minus in some formats.
#' @keywords internal
.roundForDisplay <- function(x, digits) {
  y <- round(x, digits)
  if (isTRUE(all.equal(y, 0))) 0 else y
}
