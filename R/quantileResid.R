#' Randomized Quantile Residuals
#'
#' Computes randomized quantile residuals (Dunn and Smyth, 1996) for a
#' fitted logistic model. Unlike Pearson or deviance residuals, they are
#' exactly standard normal when the model is correct, whatever the fitted
#' probabilities are - which is what makes a Q-Q plot readable for a
#' binary response in the first place.
#'
#' @details
#' For a discrete response the probability integral transform does not
#' produce a uniform variable, because the distribution function jumps at
#' the observed values. Randomization closes the gap: with
#' \eqn{a_i = F(y_i - 1)} and \eqn{b_i = F(y_i)} under the fitted model, a
#' draw \eqn{u_i \sim U(a_i, b_i)} is uniform on \eqn{(0,1)} whenever the
#' model holds, and \eqn{r_i = \Phi^{-1}(u_i)} is standard normal.
#'
#' For the binomial case both bounds are available in closed form, so no
#' simulation of replicate data sets is needed - one uniform draw per
#' observation is the entire computation.
#'
#' The residuals are random: two calls on the same model return different
#' values, and any conclusion that changes between draws is not a
#' conclusion about the model. Use \code{\link{set.seed}} for
#' reproducibility, and \code{nSim > 1} to see the spread across draws.
#'
#' @section Power for a Bernoulli response:
#' With \eqn{m = 1} the randomization interval spans the entire jump of
#' the distribution function, so the uniform draw contributes about as
#' much variation as the data do, and the residuals stay close to normal
#' even when the mean structure is wrong. In a simulation with a squared
#' term omitted (\eqn{n = 2000}), \code{\link{plotBinnedResid}} put 35 of
#' 44 bins outside the 95\% band while a Kolmogorov-Smirnov test on these
#' residuals returned \eqn{p = 0.55}.
#'
#' Read the Q-Q plot for what it can see - a few extreme observations, and
#' the distributional fit of grouped data (\eqn{m > 1}), where the
#' intervals are narrow and the residuals do have power. For the mean
#' structure use the binned residuals.
#'
#' @param x a fitted logistic model of class \code{"FitMod"} (fitted with
#'   \code{fitfn = "logit"}) or a binomial \code{\link[stats]{glm}}.
#' @param nSim number of independent randomizations. \code{1} (default)
#'   returns a vector, larger values a matrix with one column per draw.
#'
#' @return a numeric vector of length \eqn{n}, or an \eqn{n \times}
#'   \code{nSim} matrix when \code{nSim > 1}. The names (or row names) are
#'   those of the model's fitted values.
#'
#' @references
#' Dunn, P. K. and Smyth, G. K. (1996) Randomized quantile residuals.
#' \emph{Journal of Computational and Graphical Statistics}, \bold{5}(3),
#' 236--244.
#'
#' @seealso
#' [model-diagnostics-overview] for an overview of the diagnostics for
#' logistic models in alloy.
#'
#' @concept regression-diagnostics
#' @concept binary-outcome
#' @concept goodness-of-fit
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#'
#' set.seed(1)
#' r <- quantileResid(fitLogit)
#' pharos::plotQQ(r)
#'
#' # spread across randomizations
#' apply(quantileResid(fitLogit, nSim = 20), 2, function(z) shapiro.test(z)$p.value)
#'
#' @export
quantileResid <- function(x, nSim = 1L) {

  parts <- .logitParts(x)

  if (!is.numeric(nSim) || length(nSim) != 1L || is.na(nSim) ||
      nSim < 1 || nSim != round(nSim))
    stop("argument 'nSim' must be a single positive whole number")

  nSim <- as.integer(nSim)

  succ <- round(parts$y * parts$m)

  # Lower bound of the jump at the observed count, upper bound at the count
  # itself; pbinom() returns 0 for a negative quantile, which is exactly
  # what is wanted for succ == 0.
  lo <- stats::pbinom(succ - 1, size = parts$m, prob = parts$p)
  hi <- stats::pbinom(succ,     size = parts$m, prob = parts$p)

  res <- vapply(
    seq_len(nSim),
    function(i) stats::qnorm(stats::runif(parts$n, min = lo, max = hi)),
    numeric(parts$n)
  )

  rownames(res) <- names(parts$p)

  if (nSim == 1L) res[, 1L] else res
}
