
# Firth logistic regression: penalized likelihood ratio tests and profile
# penalized likelihood intervals
#
# fitMod(..., engine = "logitFirth") fits with brglm2::brglmFit (Jeffreys
# prior penalty, l*(beta) = l(beta) + 1/2 log|I(beta)|) and prepends the
# class "logitFirth", so that drop1() and confint() dispatch to the methods
# below. Both rest on the penalized likelihood, as recommended by Heinze &
# Schemper (2002) and implemented in logistf:
#
#   - drop1(): 2 * (l*(beta_hat) - l*(beta_hat_0)), beta_hat_0 maximizing l*
#     with the coefficients of the term fixed at 0,
#   - confint(): the values b with 2 * (l*(beta_hat) - l*_profile(b)) equal
#     to the chi-square quantile.
#
# The restricted fits keep the penalty of the full model (the information
# matrix of the full design), which is why a refit with the term's columns
# removed - as drop1.brglmFit() does - is not the same test. brglm2's own
# confint() method gives Wald intervals, which fail exactly where Firth's
# method is needed: near separation.


#' Penalized Likelihood Inference for Firth Logistic Regression
#'
#' Term-wise penalized likelihood ratio tests and profile penalized
#' likelihood confidence intervals for logistic models fitted with
#' `fitMod(..., engine = "logitFirth")`.
#'
#' @param object a model fitted with `fitMod(..., engine = "logitFirth")`
#' @param scope the terms to be tested, by default all terms that can be
#'   dropped, see [drop1()]
#' @param test `"Chisq"` (default) for the penalized likelihood ratio test,
#'   `"none"` for the statistics only
#' @param parm the coefficients to compute intervals for, as names or
#'   indices. Default all.
#' @param level the confidence level. Default 0.95.
#' @param ... further arguments, ignored
#'
#' @details
#' Firth's method maximizes the penalized log-likelihood
#' \eqn{l^*(\beta) = l(\beta) + \frac{1}{2}\log|I(\beta)|}. Inference
#' follows the penalized likelihood as well (Heinze and Schemper, 2002):
#' the test statistic of a term is
#' \eqn{2\,(l^*(\hat\beta) - l^*(\hat\beta_0))}, where \eqn{\hat\beta_0}
#' maximizes \eqn{l^*} with the coefficients of the term fixed at 0, and is
#' referred to a chi-square distribution with as many degrees of freedom as
#' the term has coefficients. A confidence interval collects all values of
#' a coefficient that such a test would not reject.
#'
#' The restricted fits keep the penalty of the full model. A refit without
#' the columns of the term, as in \code{brglm2:::drop1.brglmFit}, changes
#' the penalty and compares unpenalized deviances instead; the Wald
#' intervals of \code{brglm2:::confint.brglmFit} are unreliable near
#' separation, the situation Firth's method is used for.
#'
#' The methods apply to the Jeffreys penalty with the logit link
#' (\code{type = "MPL_Jeffreys"}, the default of the engine, or the
#' equivalent \code{"AS_mean"} and \code{"AS_mixed"} in
#' \code{brglm2::brglmFit}). For other types the methods of \pkg{brglm2}
#' are used.
#'
#' A bound that cannot be found within a wide search range (the profile
#' does not reach the critical value) is returned as `-Inf` or `Inf` with
#' a warning.
#'
#' @return `drop1()` returns an object of class `"anova"` with the columns
#' `Df`, `PLR` (the penalized likelihood ratio statistic) and, for
#' `test = "Chisq"`, `Pr(>Chi)`. `confint()` returns a matrix with one row
#' per coefficient and the lower and upper bound in its columns.
#'
#' @references
#' Firth, D. (1993) Bias reduction of maximum likelihood estimates.
#' \emph{Biometrika} 80, 27-38.
#'
#' Heinze, G. and Schemper, M. (2002) A solution to the problem of
#' separation in logistic regression. \emph{Statistics in Medicine} 21,
#' 2409-2419.
#'
#' @examples
#' if (requireNamespace("brglm2", quietly = TRUE)) {
#'   fit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logitFirth")
#'   drop1(fit)
#'   confint(fit)
#' }
#'
#' @name logitFirth
#' @family modelling
#' @concept regression
#' @concept binary-outcome
NULL


#' @rdname logitFirth
#' @export
drop1.logitFirth <- function(object, scope, test = c("Chisq", "none"), ...) {

  test <- match.arg(test)

  if (!.isJeffreysLogit(object))
    return(NextMethod())

  d <- .firthData(object)
  full <- .firthFit(d, start = d$coef)

  asgn <- attr(d$X, "assign")
  labs <- attr(stats::terms(object), "term.labels")

  if (missing(scope))
    scope <- stats::drop.scope(object)
  else {
    if (!is.character(scope))
      scope <- attr(stats::terms(stats::update.formula(object, scope)),
                    "term.labels")
    if (!all(scope %in% labs))
      stop("'scope' is not a subset of the term labels.")
  }

  df <- plr <- numeric(length(scope))

  for (i in seq_along(scope)) {
    drop <- asgn == match(scope[i], labs)
    start <- full$coef
    start[drop] <- 0
    red <- .firthFit(d, start = start, free = !drop)
    df[i] <- sum(drop)
    plr[i] <- max(0, 2 * (full$loglik - red$loglik))
  }

  res <- data.frame(Df = c(NA, df), PLR = c(NA, plr),
                    row.names = c("<none>", scope), check.names = FALSE)
  if (test == "Chisq")
    res[["Pr(>Chi)"]] <- c(NA, stats::pchisq(plr, df, lower.tail = FALSE))

  structure(res,
            heading = c("Single term deletions (Firth logistic regression)\n",
                        "Penalized likelihood ratio tests\n"),
            class = c("anova", "data.frame"))
}


#' @rdname logitFirth
#' @export
confint.logitFirth <- function(object, parm, level = 0.95, ...) {

  if (!.isJeffreysLogit(object))
    return(NextMethod())

  d <- .firthData(object)
  full <- .firthFit(d, start = d$coef)
  cf <- full$coef

  if (missing(parm))
    parm <- seq_along(cf)
  else if (is.character(parm))
    parm <- match(parm, names(cf))
  if (anyNA(parm) || any(parm < 1L | parm > length(cf)))
    stop("'parm' does not match the coefficients.")

  crit <- stats::qchisq(level, 1)
  se <- sqrt(diag(solve(full$info)))

  ci <- t(vapply(parm, function(j)
    c(.firthProfileBound(d, full, j, -1, crit, se[j]),
      .firthProfileBound(d, full, j,  1, crit, se[j])),
    numeric(2L)))

  a <- (1 - level) / 2
  dimnames(ci) <- list(names(cf)[parm],
                       paste(format(100 * c(a, 1 - a), trim = TRUE,
                                    scientific = FALSE, digits = 3), "%"))
  ci
}



# == internal helper functions ============================================


# The penalized likelihood methods hold for Firth's penalty, 1/2 log|I|,
# with the logit link only. Mean bias reduction (AS_mean, and AS_mixed,
# which differs from it only for a dispersion parameter) coincides with it
# for this link; MPL_Jeffreys does with the default power a = 1/2.
.isJeffreysLogit <- function(object) {
  type <- object$type
  identical(object$family$family, "binomial") &&
    identical(object$family$link, "logit") &&
    (isTRUE(type %in% c("AS_mean", "AS_mixed")) ||
       (identical(type, "MPL_Jeffreys") &&
          isTRUE(all.equal(object$control$a, 0.5))))
}


# Design, response (as proportion), prior weights (trials) and offset
.firthData <- function(object) {

  X <- stats::model.matrix(object)
  cf <- stats::coef(object)

  if (anyNA(cf))
    stop("penalized likelihood inference needs a model without aliased ",
         "coefficients.")

  n <- nrow(X)
  w <- object$prior.weights
  off <- object$offset

  list(X      = X,
       y      = as.numeric(object$y),
       w      = if (is.null(w)) rep(1, n) else as.numeric(w),
       offset = if (is.null(off)) rep(0, n) else as.numeric(off),
       coef   = cf,
       ctrl   = object$control)
}


# Firth logistic regression by modified Newton-Raphson (as in logistf):
# the score is modified by the leverages h of the FULL design, only the
# coefficients flagged 'free' are updated, the others stay at 'start'.
# Steps are capped and halved until the penalized log-likelihood rises.
.firthFit <- function(d, start, free = rep(TRUE, length(start)),
                      maxit = 100L, tol = 1e-8, maxStep = 5) {

  X <- d$X
  beta <- stats::setNames(as.numeric(start), colnames(X))

  lstar <- function(beta) {
    eta <- drop(X %*% beta) + d$offset
    p <- stats::plogis(eta)
    W <- d$w * p * (1 - p)
    R <- qr.R(qr(X * sqrt(W)))
    list(p = p, W = W,
         loglik = sum(d$w * (d$y * stats::plogis(eta, log.p = TRUE) +
                             (1 - d$y) * stats::plogis(-eta, log.p = TRUE))) +
           sum(log(abs(diag(R)))))    # 1/2 log|X'WX|
  }

  cur <- lstar(beta)
  converged <- !any(free)

  for (it in seq_len(maxit)) {

    if (!any(free)) break

    XW <- X * sqrt(cur$W)
    Q <- qr.Q(qr(XW))
    h <- rowSums(Q^2)
    U <- drop(crossprod(X, d$w * (d$y - cur$p) + h * (0.5 - cur$p)))
    info <- crossprod(XW)

    if (max(abs(U[free])) < tol) {
      converged <- TRUE
      break
    }

    delta <- numeric(length(beta))
    delta[free] <- solve(info[free, free, drop = FALSE], U[free])
    if ((mx <- max(abs(delta))) > maxStep)
      delta <- delta * maxStep / mx

    for (k in 0:25) {
      new <- lstar(beta + delta)
      if (is.finite(new$loglik) && new$loglik >= cur$loglik - 1e-10) break
      delta <- delta / 2
    }

    beta <- beta + delta
    cur <- new
  }

  if (!converged)
    warning("the Firth fit did not converge in ", maxit, " iterations.",
            call. = FALSE)

  list(coef = beta, loglik = cur$loglik,
       info = crossprod(X * sqrt(cur$W)), converged = converged)
}


# One bound of the profile penalized likelihood interval of coefficient j:
# the root of 2 * (l*max - l*(beta_j = b)) - crit on the side 'dir'
.firthProfileBound <- function(d, full, j, dir, crit, se) {

  est <- full$coef[j]
  free <- seq_along(full$coef) != j

  dev <- function(b) {
    start <- full$coef
    start[j] <- b
    fit <- .firthFit(d, start = start, free = free)
    2 * (full$loglik - fit$loglik) - crit
  }

  # bracket the root by doubling the distance from the estimate
  step <- max(se, 1e-4) * 2
  b <- est + dir * step
  for (k in 1:30) {
    if (dev(b) > 0) break
    step <- step * 2
    b <- est + dir * step
    if (k == 30L) {
      warning(gettextf(
        "the profile of '%s' does not reach the critical value; ",
        names(est)), "the bound is set to ", if (dir < 0) "-Inf" else "Inf",
        ".", call. = FALSE)
      return(dir * Inf)
    }
  }

  stats::uniroot(dev, sort(c(est, b)), tol = 1e-7)$root
}
