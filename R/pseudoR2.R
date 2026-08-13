
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
#'   \item{\code{Efron}}{squared correlation between observed and predicted
#'     values}
#'   \item{\code{Tjur}}{coefficient of discrimination, binary responses only}
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
#' need not be accessible. For the remaining classes the null model is refitted
#' with \code{\link[stats]{update}}, which requires the data of the original
#' call to be available.
#'
#' Where prior weights are present, the sample size entering Cox-Snell,
#' Nagelkerke, Aldrich-Nelson and Veall-Zimmermann is their sum rather than the
#' number of rows.
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
#' @family regression.utils
#' @concept model-evaluation
#' @concept goodness-of-fit
#'
#' @export
pseudoR2 <- function(fit, which = "McFadden") {

  all <- identical(which, "all")

  # matched up front, so that a typo does not cost a null model refit
  if (!all) {
    which <- match.arg(which, .pseudoR2Measures, several.ok = TRUE)
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
  if (type == "glm") {
    info$nEff <- sum(x$prior.weights)
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

  # glm(y = FALSE) leaves no response behind, then only the refit remains
  if (type == "glm" && !is.null(x$y)) {

    n <- NROW(x$y)

    fit0 <- stats::glm.fit(
      x = matrix(1, nrow = n, ncol = 1L),
      y = x$y,
      weights = x$prior.weights,
      offset = if (is.null(x$offset)) rep(0, n) else x$offset,
      family = x$family
    )

    # as in logLik.glm: the dispersion counts as a parameter in these families
    p0 <- 1L + as.integer(
      x$family$family %in% c("gaussian", "Gamma", "inverse.gaussian")
    )

    return(p0 - fit0$aic / 2)
  }

  fit0 <- tryCatch(

    if (type == "multinom") {
      stats::update(x, . ~ 1, trace = FALSE)
    } else {
      stats::update(x, . ~ 1)
    },

    error = function(e) {
      stop(gettextf(
        "the null model could not be refitted, the data of the original call are probably out of scope: %s",
        conditionMessage(e)
      ), call. = FALSE)
    }
  )

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

    sse <- sum((p$eta - mean(p$eta))^2)

    res["McKelveyZavoina"] <- sse / (n * s2 + sse)
  }

  # Efron and Tjur need a univariate response
  if (!is.null(p$y) && NCOL(p$y) == 1L) {

    y <- as.vector(p$y)
    yhat <- as.vector(p$fitted)

    res["Efron"] <- 1 - sum((y - yhat)^2) / sum((y - mean(y))^2)

    isBinary <- identical(p$family, "binomial") &&
      length(unique(y)) == 2L

    if (isBinary) {
      res["Tjur"] <- unname(diff(tapply(yhat, y, mean)))
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
      eta = x$linear.predictors,
      fitted = x$fitted.values,
      y = x$y,
      family = x$family$family,
      link = x$family$link
    ))
  }

  # careful: all(NULL == "logit") is TRUE, hence the length check
  lk <- x@misc$link

  link <- if (length(lk) > 0L && all(lk == "logit")) {
    "logit"
  } else if (length(lk) > 0L && all(lk == "probit")) {
    "probit"
  } else {
    NA_character_
  }

  list(
    eta = VGAM::predictvglm(x, type = "link"),
    fitted = VGAM::predictvglm(x, type = "response"),
    y = x@y,
    family = x@family@vfamily[1L],
    link = link
  )
}
