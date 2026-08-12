
#' Confidence Interval for the Difference of Two Regression Coefficients
#'
#' Computes the Wald confidence interval for the difference
#' \eqn{b_2 - b_1} of two coefficients of a fitted linear model, using
#' \eqn{Var(b_2 - b_1) = Var(b_1) + Var(b_2) - 2\,Cov(b_1, b_2)} and the
#' t-distribution with the model's residual degrees of freedom.
#'
#' @param x a fitted model object of class \code{"lm"}.
#' @param coeff two coefficients, given as names or as (integer) positions
#'   in \code{coef(x)}. The difference is taken as second minus first,
#'   \code{coeff[2] - coeff[1]}.
#' @param conf.level confidence level of the interval, default \code{0.95}.
#' @param sides a character string specifying the side of the confidence
#'   interval, one of \code{"two.sided"} (default), \code{"left"}, or
#'   \code{"right"}.
#' @param vcov. covariance matrix of the coefficient estimates, or a
#'   function to compute it from \code{x} (default \code{\link{vcov}}).
#'   Supply e.g. \code{sandwich::vcovHC} for heteroskedasticity-robust
#'   intervals.
#'
#' @return a named numeric vector with components \code{diff},
#'   \code{lci}, and \code{uci}.
#'
#' @examples
#' fit <- lm(mpg ~ cyl + disp + hp, data = mtcars)
#' coeffDiffCI(fit, c("cyl", "hp"))
#'
#' @export
coeffDiffCI <- function(x, coeff, conf.level = 0.95,
                        sides = c("two.sided", "left", "right"),
                        vcov. = vcov) {

  sides <- match.arg(sides)

  if (length(coeff) != 2L || anyDuplicated(coeff))
    stop("'coeff' must identify two distinct coefficients", call. = FALSE)

  b <- coef(x)

  # translate positions to names right away: coef() keeps aliased (NA)
  # entries while vcov() drops them, so numeric indices would misalign
  # between the two
  if (is.numeric(coeff))
    coeff <- names(b)[coeff]

  miss <- setdiff(coeff, names(b))
  if (length(miss) || anyNA(coeff))
    stop(sprintf("unknown coefficient(s): %s",
                 paste(miss, collapse = ", ")), call. = FALSE)

  est <- b[coeff]
  if (anyNA(est))
    stop("coefficient(s) not estimable (aliased)", call. = FALSE)

  V <- if (is.function(vcov.)) vcov.(x) else vcov.
  V <- V[coeff, coeff]

  d  <- unname(est[2L] - est[1L])
  se <- sqrt(V[1L, 1L] + V[2L, 2L] - 2 * V[1L, 2L])

  if (sides != "two.sided")
    conf.level <- 1 - 2 * (1 - conf.level)

  a <- qt((1 - conf.level) / 2, df = df.residual(x)) * se

  res <- c(diff = d, lci = d + a, uci = d - a)

  if (sides == "left") {
    res["uci"] <- Inf
  } else if (sides == "right") {
    res["lci"] <- -Inf
  }

  res
}
