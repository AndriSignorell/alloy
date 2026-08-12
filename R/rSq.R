
#' R-squared of a Linear Model
#'
#' Computes the (adjusted) R-squared of a linear model fitted via
#' \code{\link[stats]{lm}}, optionally together with a bootstrap confidence
#' interval based on a parallel \pkg{RcppParallel} implementation.
#'
#' If \code{conf.level} is \code{NA} (the default), the coefficient is taken
#' directly from the fitted model and a single number is returned. Only when a
#' confidence level is supplied is the bootstrap started.
#'
#' The bootstrap resamples observations (pairs bootstrap): for each replicate
#' the model is refitted on the resampled rows of the model matrix and the
#' R-squared is recomputed. The interval is formed from the empirical
#' quantiles of the resulting distribution. Degenerate bootstrap samples
#' (singular design matrices) are discarded.
#'
#' The point estimate follows the definition used by
#' \code{\link[stats]{summary.lm}}, which for models without intercept is based
#' on the uncentred sum of squares and therefore not comparable to the value of
#' a model with intercept. For adjusted R-squared
#' \deqn{R^2_{adj} = 1 - (1 - R^2)\frac{n - i}{n - p}}
#' is used, where \eqn{p} is the rank of the model and \eqn{i} is 1 for a model
#' with intercept and 0 otherwise.
#'
#' \code{sides} names the side on which the finite bound lies, so \code{"left"}
#' yields \eqn{[lci, \infty)} and \code{"right"} yields \eqn{(-\infty, uci]},
#' with the full \eqn{\alpha} on that single side. This reverses the meaning the
#' argument has in \pkg{DescTools}, where it names the direction of the
#' alternative hypothesis. Since R-squared cannot exceed 1, the open side of a
#' \code{"left"} interval is reported as 1 rather than as \code{Inf}; for
#' \code{"right"} the open side is 0, or \code{-Inf} for the adjusted
#' coefficient, which is not bounded below.
#'
#' The bootstrap branch supports neither weighted models nor models without
#' intercept, as the C++ routine centres unconditionally and ignores weights.
#' Both cases are rejected with an error; the point estimate remains available
#' via \code{conf.level = NA}.
#'
#' @param fit an object of class \code{"lm"}.
#' @param conf.level confidence level of the interval. Set to \code{NA}
#'   (default) to return the point estimate only, without bootstrapping.
#' @param sides a character string naming the side on which the finite bound
#'   lies, must be one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}.
#' @param adjusted logical; if \code{TRUE} (default) the adjusted R-squared is
#'   reported, otherwise the ordinary one.
#' @param R number of bootstrap replicates, defaults to 2000.
#' @param seed integer seed for the bootstrap. If \code{NULL} (default) the
#'   seed is drawn from R's random number stream, so that \code{set.seed()}
#'   governs the result.
#' @param ... further arguments, currently unused.
#'
#' @return If \code{conf.level} is \code{NA}, a single numeric value.
#'   Otherwise a named numeric vector with the elements
#'   \describe{
#'     \item{est}{the (adjusted) R-squared of the fitted model.}
#'     \item{lci}{the lower confidence limit.}
#'     \item{uci}{the upper confidence limit.}
#'   }
#'
#' @family regression.utils
#' @concept regression
#' @concept confidence-interval
#' @concept bootstrap
#'
#' @seealso \code{\link[stats]{lm}}, \code{\link[stats]{summary.lm}}
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#'
#' rSq(fit)
#' ## [1] 0.8148
#'
#' rSq(fit, adjusted = FALSE)
#' ## [1] 0.8268
#'
#' rSq(fit, conf.level = 0.95, seed = 123)
#'
#' rSq(fit, conf.level = 0.95, sides = "left", R = 1000, seed = 123)
#'
#' @export
rSq <- function(fit,
                conf.level = NA,
                sides = c("two.sided", "left", "right"),
                adjusted = TRUE,
                R = 2000,
                seed = NULL,
                ...) {

  if(!inherits(fit, "lm"))
    stop("'fit' must be an object of class \"lm\"")

  sides <- match.arg(sides)

  est <- .rSq(fit, adjusted = adjusted)

  if(is.na(conf.level))
    return(est)

  if(!is.null(fit$weights))
    stop("weighted models are not supported by the bootstrap, use conf.level = NA")

  if(attr(fit$terms, "intercept") == 0L)
    stop("models without intercept are not supported by the bootstrap, use conf.level = NA")

  alpha <- 1 - conf.level
  if(sides != "two.sided")
    alpha <- 2 * alpha

  # draw the seed from R's stream, so that set.seed() controls the bootstrap
  if(is.null(seed))
    seed <- sample.int(.Machine$integer.max, 1L)

  res <- rsq_boot_cpp(X = stats::model.matrix(fit),
                      y = stats::model.response(stats::model.frame(fit)),
                      B = R,
                      alpha = alpha,
                      adjusted = adjusted,
                      seed = as.integer(seed))

  # take the estimate from the fit rather than from the C++ routine, so that
  # both branches of the function report exactly the same value
  res[["est"]] <- est

  # sides names the side carrying the finite bound (design_rules.md 4.1);
  # R-squared is bounded above by 1, so the open side is reported at that
  # boundary rather than as Inf
  if(sides == "left")  res[["uci"]] <- 1
  if(sides == "right") res[["lci"]] <- if(adjusted) -Inf else 0

  res

}


# R-squared of an lm object, following the definition of stats:::summary.lm()
# (uncentred sums of squares without intercept, weighted mean for weighted
# fits), but without computing the coefficient table.

.rSq <- function(fit, adjusted = TRUE) {

  f <- fit$fitted.values
  r <- fit$residuals
  w <- fit$weights

  n <- if(is.null(fit$qr)) length(f) else NROW(fit$qr$qr)
  rdf <- fit$df.residual
  dfInt <- attr(fit$terms, "intercept")

  if(is.null(w)) {
    mss <- if(dfInt) sum((f - mean(f))^2) else sum(f^2)
    rss <- sum(r^2)

  } else {
    mss <- if(dfInt) sum(w * (f - sum(w * f) / sum(w))^2) else sum(w * f^2)
    rss <- sum(w * r^2)
  }

  res <- mss / (mss + rss)

  if(adjusted)
    res <- 1 - (1 - res) * ((n - dfInt) / rdf)

  res

}
