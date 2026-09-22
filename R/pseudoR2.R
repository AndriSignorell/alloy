
#' Pseudo R-Squared Measures for Regression Models
#'
#' Computes a set of pseudo R-squared statistics for fitted regression models
#' where the ordinary coefficient of determination is not defined, such as
#' logistic, Poisson or ordinal regression.
#'
#' The following measures are available. Which of them can be computed depends
#' on the model class, the family and the link function; measures that are not
#' defined for a given fit are omitted from the result.
#' \describe{
#'   \item{\code{McFadden}}{likelihood ratio index}
#'   \item{\code{McFaddenAdj}}{adjusted likelihood ratio index, penalized for
#'     the number of estimated parameters}
#'   \item{\code{CoxSnell}}{maximum likelihood R-squared}
#'   \item{\code{Nagelkerke}}{Cox-Snell R-squared, rescaled to a maximum of 1}
#'   \item{\code{AldrichNelson}}{based on the likelihood ratio statistic}
#'   \item{\code{VeallZimmermann}}{correction of Aldrich-Nelson}
#'   \item{\code{McKelveyZavoina}}{latent variable R-squared, logit and probit
#'     links only}
#'   \item{\code{Efron}}{one minus the ratio of the residual to the total sum
#'     of squares of the response, on the scale of the response. Only for an
#'     identity link does this coincide with the squared correlation between
#'     observed and fitted values}
#'   \item{\code{Tjur}}{coefficient of discrimination, the mean fitted
#'     probability of the successes minus that of the failures, binomial
#'     responses only}
#'   \item{\code{AIC}, \code{BIC}}{information criteria of the fitted model}
#'   \item{\code{logLik}, \code{logLik0}}{log-likelihood of the fitted and of
#'     the null model}
#'   \item{\code{G2}}{likelihood ratio statistic}
#' }
#'
#' @param fit a fitted model object of class \code{glm}, \code{multinom}
#'   (\pkg{nnet}), \code{polr} (\pkg{MASS}) or \code{vglm} (\pkg{VGAM})
#' @param which character vector naming the measures to return, or
#'   \code{"all"} for everything available
#'
#' @return a named numeric vector holding the requested measures
#'
#' @details
#' All measures are derived from the log-likelihoods of the fitted and of the
#' intercept-only model, not from the deviance ratio; the two coincide only
#' where the saturated log-likelihood vanishes.
#'
#' For \code{glm} objects the null model is refitted through
#' \code{\link[stats]{glm.fit}} on the model's own response, prior weights and
#' offset. Aggregated responses (\code{cbind(success, failure)}), frequency
#' weights and offsets are therefore handled correctly, and the original data
#' need not be accessible. For \code{polr} and \code{multinom}, the null model
#' is refitted on the full model's model frame, preserving its analysis sample,
#' weights and offsets. If that frame was not stored, it is rebuilt from the
#' call, which does require the original data to be available. For
#' \code{vglm} the null model is refitted with \code{\link[stats]{update}},
#' which requires the original data as well.
#'
#' Where prior weights are present, the sample size entering Cox-Snell,
#' Nagelkerke, Aldrich-Nelson and Veall-Zimmermann is their sum rather than the
#' number of rows, and McKelvey-Zavoina, Efron and Tjur are computed with the
#' weights as case counts.
#'
#' For binomial \code{glm} and \code{vglm} objects all measures refer to the
#' single trial (Bernoulli) level. \code{\link[stats]{logLik}} of an
#' aggregated fit
#' (\code{cbind(success, failure)} or proportions with weights) contains the
#' constant \eqn{\sum \log\binom{n_i}{k_i}}{sum(lchoose(n, k))}, which the
#' disaggregated 0/1 fit of the same model lacks. The constant cancels in
#' \code{G2}, but not in ratios of log-likelihoods, so McFadden, Nagelkerke
#' and the like would depend on how the data happen to be stored. Here
#' \code{logLik}, \code{logLik0}, \code{AIC} and \code{BIC} are therefore
#' computed without that constant, with \code{BIC} penalized by the number of
#' trials, and both representations give identical results. For aggregated
#' fits these values consequently differ from \code{stats::logLik()},
#' \code{stats::AIC()} and \code{stats::BIC()}.
#'
#' For \code{vglm} objects the package \pkg{VGAM} must be installed and the
#' model should have been fitted with \code{model = TRUE}, so that the model
#' frame can be extracted.
#'
#' @references
#' McFadden, D. (1974) Conditional logit analysis of qualitative choice
#' behavior. In: Zarembka, P. (ed.) \emph{Frontiers in Econometrics},
#' Academic Press, New York, 105-142.
#'
#' Cox, D. R., Snell, E. J. (1989) \emph{Analysis of Binary Data},
#' 2nd ed., Chapman and Hall, London.
#'
#' Nagelkerke, N. J. D. (1991) A note on a general definition of the
#' coefficient of determination. \emph{Biometrika}, 78(3), 691-692.
#'
#' Veall, M. R., Zimmermann, K. F. (1996) Pseudo-R2 measures for some common
#' limited dependent variable models. \emph{Journal of Economic Surveys},
#' 10(3), 241-259.
#'
#' Tjur, T. (2009) Coefficients of determination in logistic regression models.
#' \emph{The American Statistician}, 63(4), 366-372.
#'
#' @examples
#' fit <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#'
#' pseudoR2(fit)
#' ## [1] 0.7178751
#'
#' pseudoR2(fit, which = c("Nagelkerke", "Tjur"))
#' pseudoR2(fit, which = "all")
#'
#' # aggregated and disaggregated binomial data give the same result
#' d.agg <- aggregate(cbind(am, n = 1) ~ cyl, data = mtcars, FUN = sum)
#' fitAgg <- glm(cbind(am, n - am) ~ factor(cyl), data = d.agg,
#'               family = binomial)
#' fitInd <- glm(am ~ factor(cyl), data = mtcars, family = binomial)
#'
#' rbind(aggregated = pseudoR2(fitAgg, which = "all"),
#'       single = pseudoR2(fitInd, which = "all"))
#'
#' @family regression.utils
#' @concept model-evaluation
#' @concept goodness-of-fit
#'
#' @export
pseudoR2 <- function(fit, which = "McFadden") {

  all <- identical(which, "all")

  # matched up front, so that a typo does not cost a null model refit
  if (!all) {
    which <- .matchMeasures(which)
  }

  info <- .getModelInfo(fit)

  res <- .coreMeasures(info)

  if (info$type %in% c("glm", "vglm")) {
    res <- c(res, .extraMeasures(fit, info, res))
  }

  res <- res[intersect(.pseudoR2Measures, names(res))]

  if (all) {
    return(res)
  }

  missing <- setdiff(which, names(res))

  if (length(missing) > 0L) {
    stop(gettextf(
      "measure not defined for this fit: %s",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  res[which]
}




# == internal helper functions =============================================


#' Vocabulary of all measures, also fixing their order in the result
#'
#' @keywords internal
#' @noRd
.pseudoR2Measures <- c(
  "McFadden", "McFaddenAdj", "CoxSnell", "Nagelkerke",
  "AldrichNelson", "VeallZimmermann", "McKelveyZavoina",
  "Efron", "Tjur",
  "AIC", "BIC", "logLik", "logLik0", "G2"
)




#' Everything the measures need, collected from the fit in one place
#'
#' \code{n} is the number of observations, \code{nEff} the sample size the
#' measures are scaled by, which differs from \code{n} under prior weights.
#' The degrees of freedom and the log-likelihood are taken from
#' \code{\link[stats]{logLik}} rather than from class specific components, as
#' the latter are named inconsistently across the supported classes.
#'
#' @keywords internal
#' @noRd
.getModelInfo <- function(x) {

  type <- if (inherits(x, "glm")) {
    "glm"
  } else if (inherits(x, "vglm")) {
    "vglm"
  } else if (inherits(x, "multinom")) {
    "multinom"
  } else if (inherits(x, "polr")) {
    "polr"
  } else {
    stop(gettextf(
      "no pseudo R-squared available for an object of class %s",
      dQuote(class(x)[1L])
    ), call. = FALSE)
  }

  if (type == "vglm" && !requireNamespace("VGAM", quietly = TRUE)) {
    stop("package 'VGAM' is required for vglm models", call. = FALSE)
  }

  loglik <- stats::logLik(x)

  # logLik() does not attach nobs everywhere, and polr keeps the number of
  # rows in $n but the weighted case count in $nobs
  n <- attr(loglik, "nobs")

  if (is.null(n)) {
    n <- if (type == "vglm") {
      stats::nobs(x)
    } else {
      x[["nobs"]] %||% x[["n"]] %||% stats::nobs(x)
    }
  }

  edf <- attr(loglik, "df")

  if (is.null(edf)) {
    edf <- if (type == "vglm") x@rank else x$rank
  }

  info <- list(
    type = type,
    fit = x,
    logLik = as.numeric(loglik),
    logLik0 = .nullLogLik(x, type),
    n = n,
    nEff = n,
    edf = edf,
    AIC = stats::AIC(x),
    BIC = stats::BIC(x)
  )

  # prior weights carry the sample size, e.g. with aggregated binomial data
  w <- .priorWeights(x, type)

  if (!is.null(w)) {
    info$nEff <- sum(w)
  }

  # trial level likelihood, invariant to aggregation, see details
  bin <- .binomialResponse(x, type)

  if (!is.null(bin)) {

    info$logLik <- .bernoulliLogLik(bin$y, bin$mu, bin$w)
    info$AIC <- -2 * info$logLik + 2 * edf
    info$BIC <- -2 * info$logLik + log(info$nEff) * edf
  }

  info
}




#' Log-likelihood of the intercept-only model
#'
#' For glm the null model is refitted on the model's own response, prior
#' weights and offset, which avoids rebuilding a formula. That matters for
#' aggregated responses, where the response term is not a plain variable name,
#' and for offsets, which a formula rebuilt from the model frame would silently
#' drop.
#'
#' @keywords internal
#' @noRd
.nullLogLik <- function(x, type) {

  if (type == "glm") {

    y <- .glmResponse(x)
    n <- NROW(y)

    fit0 <- stats::glm.fit(
      x = matrix(1, nrow = n, ncol = 1L),
      y = y,
      weights = x$prior.weights,
      offset = if (is.null(x$offset)) rep(0, n) else x$offset,
      family = x$family
    )

    if (identical(.familyName(x, type), "binomial")) {
      return(.bernoulliLogLik(y, fit0$fitted.values, x$prior.weights))
    }

    # as in logLik.glm: the dispersion counts as a parameter in these families
    p0 <- 1L + as.integer(
      x$family$family %in% c("gaussian", "Gamma", "inverse.gaussian")
    )

    return(p0 - fit0$aic / 2)
  }

  fit0 <- tryCatch(

    if (type %in% c("multinom", "polr")) {
      .nullRefit(x, type)
    } else {
      stats::update(x, . ~ 1)
    },

    error = function(e) {
      stop(gettextf(
        "the null model could not be refitted: %s",
        conditionMessage(e)
      ), call. = FALSE)
    }
  )

  bin <- .binomialResponse(fit0, type)

  if (!is.null(bin)) {
    return(.bernoulliLogLik(bin$y, bin$mu, bin$w))
  }

  as.numeric(stats::logLik(fit0))
}




#' Measures available for every supported model class
#'
#' @keywords internal
#' @noRd
.coreMeasures <- function(info) {

  l1 <- info$logLik
  l0 <- info$logLik0

  n <- info$nEff

  g2 <- -2 * (l0 - l1)

  coxSnell <- 1 - exp(-g2 / n)

  c(
    McFadden = 1 - l1 / l0,
    McFaddenAdj = 1 - (l1 - info$edf) / l0,
    CoxSnell = coxSnell,
    Nagelkerke = coxSnell / (1 - exp(2 * l0 / n)),
    AIC = info$AIC,
    BIC = info$BIC,
    logLik = l1,
    logLik0 = l0,
    G2 = g2
  )
}




#' Measures relying on the linear predictor or the fitted values
#'
#' Only those that are defined for the given link and response are returned.
#'
#' @keywords internal
#' @noRd
.extraMeasures <- function(x, info, core) {

  p <- .getPredictions(x, info$type)

  n <- info$nEff

  # unname(), otherwise the names of the operands are inherited and the
  # result is called AldrichNelson.G2
  g2 <- unname(core["G2"])
  l0 <- unname(core["logLik0"])

  res <- c(
    AldrichNelson = g2 / (g2 + n),
    VeallZimmermann = (g2 / (g2 + n)) * (2 * l0 - n) / (2 * l0)
  )

  # latent variable residual variance, defined for these two links only
  s2 <- if (isTRUE(p$link == "logit")) {
    pi^2 / 3
  } else if (isTRUE(p$link == "probit")) {
    1
  } else {
    NA_real_
  }

  if (!is.na(s2)) {

    # weights as case counts, n being their sum
    eta <- as.vector(p$eta)
    sse <- sum(p$w * (eta - stats::weighted.mean(eta, p$w))^2)

    res["McKelveyZavoina"] <- sse / (sum(p$w) * s2 + sse)
  }

  # Efron and Tjur need a univariate response
  if (!is.null(p$y) && NCOL(p$y) == 1L) {

    y <- as.vector(p$y)
    yhat <- as.vector(p$fitted)
    w <- p$w

    # quasibinomial has no likelihood, but the same response structure
    isBinomial <- p$family %in% c("binomial", "quasibinomial")

    # within cell sum of squares of the single 0/1 trials behind a
    # proportion, zero for binary responses
    within <- if (isBinomial) sum(w * y * (1 - y)) else 0

    res["Efron"] <- 1 -
      (sum(w * (y - yhat)^2) + within) /
      (sum(w * (y - stats::weighted.mean(y, w))^2) + within)

    # mean fitted probability of the successes minus that of the failures,
    # counted over the trials behind the proportions
    if (isBinomial && any(y > 0) && any(y < 1)) {
      res["Tjur"] <- sum(w * y * yhat) / sum(w * y) -
        sum(w * (1 - y) * yhat) / sum(w * (1 - y))
    }
  }

  res
}




#' Linear predictor, fitted values and response on a common shape
#'
#' @keywords internal
#' @noRd
.getPredictions <- function(x, type) {

  if (type == "glm") {

    # taken from the object, so that no predict() method is dispatched
    return(list(
      type = type,
      eta = x$linear.predictors,
      fitted = x$fitted.values,
      y = .glmResponse(x),
      w = x$prior.weights,
      family = .familyName(x, type),
      link = x$family$link
    ))
  }

  eta <- VGAM::predictvglm(x, type = "link")

  list(
    type = type,
    eta = eta,
    fitted = VGAM::predictvglm(x, type = "response"),
    y = x@y,
    w = .priorWeights(x, type) %||% rep(1, NROW(eta)),
    family = .familyName(x, type),
    link = .linkName(x)
  )
}



#' Intercept-only refit of a polr or multinom fit
#'
#' The null model is fitted on the model frame of the full fit, which freezes
#' its analysis sample: update(. ~ 1) would bring rows back that were dropped
#' for a missing predictor.
#'
#' @keywords internal
#' @noRd
.nullRefit <- function(x, type) {

  mf <- .modelFrame(x)

  nullData <- data.frame(.nullRow = seq_len(nrow(mf)))
  nullData[[".nullResponse"]] <- stats::model.response(mf)
  nullWeights <- stats::model.weights(mf)
  nullOffset <- stats::model.offset(mf)

  nullFormula <- .nullResponse ~ 1

  if (!is.null(nullOffset)) {
    nullData[[".nullOffset"]] <- nullOffset
    nullFormula <- .nullResponse ~ 1 + offset(.nullOffset)
  }

  # update.formula() can retain the original formula environment.
  # Embed the actual objects, not symbols naming local variables: the
  # model-frame evaluation must not have to look up nullWeights et al.
  nullCall <- stats::update(x, formula. = nullFormula, evaluate = FALSE)
  nullCall$formula <- nullFormula
  nullCall$data <- nullData
  nullCall$weights <- nullWeights

  # The retained model frame has already applied the original subset;
  # original predictor contrasts no longer belong to the null design.
  nullCall$subset <- NULL
  nullCall$contrasts <- NULL
  nullCall$na.action <- quote(stats::na.fail)

  if (type == "multinom") {
    nullCall$trace <- FALSE
  }

  eval(nullCall, envir = environment())
}





#' Model frame of a fit, also where its own method refuses
#'
#' MASS:::model.frame.polr() rebuilds the original call as a model.frame()
#' call but leaves the arguments of the fit in place, so that a stored
#' model = FALSE is taken for a variable of length one and the call fails.
#' The frame is then rebuilt here from the arguments model.frame() knows,
#' the way model.frame.default() does it, which keeps the analysis sample of
#' the original fit: formula, data, subset, weights and the na.action.
#'
#' @keywords internal
#' @noRd
.modelFrame <- function(x) {

  mf <- tryCatch(stats::model.frame(x), error = function(e) NULL)

  if (!is.null(mf)) {
    return(mf)
  }

  cl <- stats::getCall(x)

  if (is.null(cl)) {
    stop("the fit has no call to rebuild its model frame from", call. = FALSE)
  }

  keep <- c("formula", "data", "subset", "weights", "na.action")

  cl <- cl[c(1L, match(keep, names(cl), nomatch = 0L))]
  cl[[1L]] <- quote(stats::model.frame)

  eval(cl, environment(stats::terms(x)) %||% parent.frame())
}



#' Measure names, matched strictly
#'
#' match.arg(several.ok = TRUE) drops what it cannot match as long as one
#' element matches, so a typo in a vector of names would pass unnoticed.
#'
#' @keywords internal
#' @noRd
.matchMeasures <- function(which) {

  if (!is.character(which)) {
    stop("'which' must be a character vector", call. = FALSE)
  }

  # charmatch: NA where nothing matches, 0 where the match is ambiguous
  idx <- charmatch(which, .pseudoR2Measures)

  bad <- which[is.na(idx)]

  if (length(bad) > 0L) {
    stop(gettextf(
      "unknown measure: %s\nit should be one of %s",
      paste(bad, collapse = ", "),
      paste(dQuote(.pseudoR2Measures), collapse = ", ")
    ), call. = FALSE)
  }

  ambiguous <- which[idx == 0L]

  if (length(ambiguous) > 0L) {
    stop(gettextf(
      "ambiguous measure: %s", paste(ambiguous, collapse = ", ")
    ), call. = FALSE)
  }

  .pseudoR2Measures[idx]
}




#' Family name of a fit, on the naming of stats
#'
#' @keywords internal
#' @noRd
.familyName <- function(x, type) {

  if (type == "glm") {
    return(x$family$family)
  }

  fam <- x@family@vfamily[1L]

  # VGAM calls the binomial family binomialff, and posbinomial et al. are
  # different models, hence no prefix matching here
  if (fam %in% c("binomialff", "binomial")) "binomial" else fam
}




#' Link name of a vglm, on the naming of stats
#'
#' VGAM renamed its link functions to logitlink(), probitlink() etc. in 1.1-0,
#' the old names are still accepted in fits.
#'
#' @keywords internal
#' @noRd
.linkName <- function(x) {

  # careful: all(NULL == "logit") is TRUE, hence the length check
  lk <- x@misc$link

  if (length(lk) == 0L) {
    return(NA_character_)
  }

  lk <- sub("link$", "", lk)

  if (all(lk == "logit")) {
    "logit"
  } else if (all(lk == "probit")) {
    "probit"
  } else {
    NA_character_
  }
}




#' Prior weights of a fit, the case counts of aggregated data
#'
#' @keywords internal
#' @noRd
.priorWeights <- function(x, type) {

  w <- if (type == "glm") {
    x$prior.weights
  } else if (type == "vglm") {
    x@prior.weights
  }

  if (length(w) == 0L) {
    return(NULL)
  }

  as.vector(w)
}




#' Response of a binomial fit, or NULL where that is not what the fit is
#'
#' Collects what the trial level likelihood needs. Multivariate responses are
#' left alone, there is no single proportion to work with then.
#'
#' @keywords internal
#' @noRd
.binomialResponse <- function(x, type) {

  if (!type %in% c("glm", "vglm") ||
      !identical(.familyName(x, type), "binomial")) {
    return(NULL)
  }

  y <- if (type == "glm") .glmResponse(x) else x@y
  mu <- if (type == "glm") x$fitted.values else VGAM::fittedvlm(x)

  if (NCOL(y) != 1L || NCOL(mu) != 1L) {
    return(NULL)
  }

  y <- as.vector(y)

  list(
    y = y,
    mu = as.vector(mu),
    w = .priorWeights(x, type) %||% rep(1, length(y))
  )
}




#' Response of a glm on the scale glm.fit works with
#'
#' glm(y = FALSE) drops the response; it is then recovered from the fitted
#' values and the response residuals, as residuals.glm() does. The rounding
#' error of that detour is removed at 0 and 1, where it would push the
#' response out of the family's domain (binomial, poisson) and make glm.fit()
#' stop.
#'
#' @keywords internal
#' @noRd
.glmResponse <- function(x) {

  if (!is.null(x$y)) {
    return(x$y)
  }

  # Work on a local copy: na.exclude would otherwise pad the residuals
  # with NAs while fitted.values still contains only the analysis sample.
  x$na.action <- NULL
  y <- x$fitted.values + stats::residuals(x, type = "response")

  tol <- sqrt(.Machine$double.eps)
  y[abs(y) < tol] <- 0
  y[abs(y - 1) < tol] <- 1

  y
}




#' Weighted log-likelihood of the single Bernoulli trials
#'
#' Equals stats::logLik() of a binomial glm minus sum(lchoose(n, k)), i.e.
#' the log-likelihood of the same model fitted to the disaggregated 0/1 data.
#' 0 * log(0) is taken as 0.
#'
#' @keywords internal
#' @noRd
.bernoulliLogLik <- function(y, mu, w) {

  # Drop zero counts before evaluating logarithms: 0 * (-Inf) is NaN.
  keep <- w != 0
  y <- y[keep]
  mu <- mu[keep]
  w <- w[keep]

  ll <- ifelse(y > 0, y * log(mu), 0) +
    ifelse(y < 1, (1 - y) * log1p(-mu), 0)

  sum(w * ll)
}
