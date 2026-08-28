#' Diagnostic Plots for FitMod Models
#'
#' Draws the diagnostic panels for a fitted model. For a logistic model
#' the panels are the ones that work for a binary response; for every
#' other model type the call is passed on to the plot method of the
#' underlying model object.
#'
#' @details
#' The residual-versus-fitted plot that \code{\link[stats]{plot.lm}} draws
#' is empty for a binary response - the residuals fall on the two curves
#' \eqn{-p} and \eqn{1-p} and nothing else can be read from them. The
#' panels here replace it:
#'
#' \describe{
#'   \item{1}{binned residuals against the fitted probabilities
#'     (\code{\link{plotBinnedResid}}) - overall functional form}
#'   \item{2}{calibration curve (\code{\link{plotCalibration}}) - are the
#'     predicted risks right at the level they claim}
#'   \item{3}{Q-Q plot of randomized quantile residuals
#'     (\code{\link{quantileResid}}) - the one residual definition that is
#'     normal under a correct binary model}
#'   \item{4}{influence (\code{\link{plotInfluence}}) - which observations
#'     the model fits badly, and which of those move it}
#'   \item{5}{separation (\code{\link{plotSeparation}}) - how well the
#'     predictions order the outcomes}
#' }
#'
#' Linearity in the logit is checked per predictor and therefore has no
#' fixed panel number; call \code{\link{plotPartialResid}} for the terms
#' in question, or \code{\link{plotBinnedResid}} with \code{var} set.
#'
#' No layout is set: with more panels selected than the device holds, the
#' method asks before each new page when the session is interactive, as
#' \code{\link[stats]{plot.lm}} does. Arranging panels on one page is the
#' caller's business (\code{par(mfrow = c(2, 3))}).
#'
#' @param x a fitted model of class \code{"FitMod"}.
#' @param which panels to draw, a subset of \code{1:5}. See Details.
#' @param ask logical; ask before drawing each panel that would start a
#'   new page. Defaults to \code{TRUE} when more panels are requested than
#'   the current layout holds and the device is interactive.
#' @param ... further arguments passed to every panel drawn. Arguments
#'   that only one panel understands (\code{var}, \code{newdata},
#'   \code{metric}, ...) are better given by calling that panel function
#'   directly.
#'
#' @return invisibly, a named list with the return value of each panel
#'   drawn.
#'
#' @seealso
#' [model-diagnostics-overview] for an overview of the diagnostics for
#' logistic models in alloy.
#'
#' @family modelling
#' @concept regression-diagnostics
#' @concept binary-outcome
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#'
#' op <- par(mfrow = c(2, 3))
#' plot(fitLogit)
#' par(op)
#'
#' plot(fitLogit, which = 2)
#'
#' # other model types keep their own diagnostics
#' plot(fitMod(Fertility ~ ., swiss))
#'
#' @export
plot.FitMod <- function(x, which = 1:5, ask = NULL, ...) {

  fit <- .stripFitMod(x)

  # Everything that is not a logistic model keeps the diagnostics of its
  # own class - this method sits in front of plot.lm(), plot.rpart() and
  # the rest by virtue of the class order, and must not swallow them.
  isLogit <- inherits(fit, "glm") &&
    fit$family$family %in% c("binomial", "quasibinomial")

  if (!isLogit)
    return(invisible(graphics::plot(fit, ...)))

  if (!is.numeric(which) || !length(which) || !all(which %in% 1:5))
    stop("argument 'which' must be a subset of 1:5")

  which <- unique(as.integer(which))

  if (is.null(ask))
    ask <- length(which) > prod(graphics::par("mfcol")) &&
      grDevices::dev.interactive()

  if (isTRUE(ask)) {
    oask <- grDevices::devAskNewPage(TRUE)
    on.exit(grDevices::devAskNewPage(oask))
  }

  # The Q-Q panel gets its title here rather than from plotQQ's default,
  # but a main= handed to plot() still wins - assembling the call keeps
  # that from becoming a duplicated formal.
  dots   <- list(...)
  qqArgs <- c(list(quantileResid(fit)), dots)
  if (is.null(dots$main))
    qqArgs$main <- "randomized quantile residuals"
  if (is.null(dots$xlab)) qqArgs$xlab <- "theoretical quantiles"
  if (is.null(dots$ylab)) qqArgs$ylab <- "sample quantiles"

  res <- list()

  for (i in which) {

    panel <- switch(
      i,
      plotBinnedResid(fit, ...),
      plotCalibration(fit, ...),
      do.call(plotQQ, qqArgs),
      plotInfluence(fit, ...),
      plotSeparation(fit, ...)
    )

    # Single bracket with an explicit list(): res[["3"]] <- NULL would
    # delete the element instead of storing it, and a panel that returns
    # nothing would silently vanish from the result - which is exactly
    # what plotQQ() does.
    res[as.character(i)] <- list(panel)
  }

  invisible(res)
}
