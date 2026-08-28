#' Influence Plot for Logistic Models
#'
#' Plots a lack-of-fit measure against the fitted probabilities, with the
#' symbol area proportional to Cook's distance. Poorly fitted observations
#' and influential ones are two different things, and this puts both in
#' one panel: the height says the model does not describe the point, the
#' size says the point moves the model.
#'
#' @details
#' The vertical axis is one of the leave-one-out change statistics of
#' Hosmer, Lemeshow and Sturdivant (2013), computed from the standardized
#' Pearson residual \eqn{r_{s}} and the leverage \eqn{h}:
#'
#' \describe{
#'   \item{\code{"chisq"}}{\eqn{\Delta\chi^2 = r_{s}^2}, the decrease in
#'     the Pearson statistic when the observation is deleted. Values above
#'     roughly 4 mark points the model fits poorly.}
#'   \item{\code{"deviance"}}{\eqn{\Delta D = d^2/(1-h)}, the same idea on
#'     the deviance scale. Less sensitive to a single extreme point.}
#' }
#'
#' Cook's distance combines both, \eqn{r_{s}^2 h / \{(1-h)p\}}: a large
#' residual at low leverage changes nothing, and high leverage with a
#' small residual means the model already accommodates the point. Only the
#' product is worth acting on, which is why it is the bubble area rather
#' than a third panel.
#'
#' @section What the plot has to look like:
#' With ungrouped data the vertical axis is not free to vary. At
#' negligible leverage, \eqn{\Delta\chi^2} is \eqn{(1-\hat p)/\hat p} for
#' an event and \eqn{\hat p/(1-\hat p)} for a non-event, so every point
#' lies on one of two curves: an arm rising to the left (events at small
#' \eqn{\hat p}) and one rising to the right (non-events at large
#' \eqn{\hat p}). The tall points at both ends are geometry, not a
#' finding, and reading them as outliers is the standard misreading of
#' this plot.
#'
#' The two arms are therefore drawn as reference curves (\code{reference}).
#' What is worth looking at is the departure from them: leverage lifts a
#' point above its arm by the factor \eqn{1/(1-h)}, so vertical distance
#' from the curve - not height above zero - is what marks an observation
#' the model cannot accommodate. A raised floor in the middle of the range
#' and large bubbles spread across it are the other two signatures of a
#' model in trouble.
#'
#' @details
#' The statistics are computed per observation. Hosmer, Lemeshow and
#' Sturdivant define them per \emph{covariate pattern}, which differs
#' whenever observations share identical predictor values - with a
#' continuous predictor in the model, patterns and observations coincide
#' and the distinction is empty; with purely categorical predictors it is
#' not, and the per-observation version understates the influence of a
#' whole pattern.
#'
#' @param x a fitted logistic model of class \code{"FitMod"} (fitted with
#'   \code{fitfn = "logit"}) or a binomial \code{\link[stats]{glm}}.
#' @param metric the change statistic on the vertical axis,
#'   \code{"chisq"} (default) or \code{"deviance"}. See Details.
#' @param main main title. \code{NULL} (default) derives one from the
#'   model formula; \code{""}, \code{NA} or \code{FALSE} suppress it.
#' @param xlab,ylab axis labels. \code{NULL} derives them from
#'   \code{metric}.
#' @param xlim,ylim axis limits.
#' @param threshold horizontal reference line marking a poorly fitted
#'   observation. \code{NULL} (default) draws none for ungrouped data and
#'   uses 4 for grouped data (\eqn{m > 1}); \code{NA} always draws none.
#'   See the section below for why the usual cut-off does not apply to a
#'   binary response.
#' @param col,border fill and border colour of the bubbles.
#' @param cex scaling factor applied to the bubble areas.
#' @param grid,box background grid and plot box, following the flexible
#'   \code{TRUE}/\code{FALSE}/\code{NA}/\code{list()} pattern.
#' @param reference the two theoretical arms the points fall on at zero
#'   leverage. \code{TRUE} (default), \code{FALSE}, or a named list passed
#'   to \code{lines}. Drawn for ungrouped data only.
#' @param labels labelling of the most influential observations. A number
#'   (default \code{5}) labels that many largest Cook's distances,
#'   \code{FALSE} none, \code{TRUE} labels everything above
#'   \code{threshold}, a named list is passed to \code{boxedText}.
#' @param stamp corner stamp, passed to the graphics framework.
#' @param ... further graphical parameters passed to \code{par()} via the
#'   internal framework.
#'
#' @return invisibly, a data frame with one row per observation and the
#'   columns \code{p}, \code{hat}, \code{residStd}, \code{dChisq},
#'   \code{dDeviance} and \code{cook}, sorted as the data are.
#'
#' @references
#' Hosmer, D. W., Lemeshow, S. and Sturdivant, R. X. (2013) \emph{Applied
#' Logistic Regression}, 3rd ed., New York: Wiley, ch. 5.
#'
#' @seealso
#' [model-diagnostics-overview] for an overview of the diagnostics for
#' logistic models in alloy.
#'
#' @concept regression-diagnostics
#' @concept binary-outcome
#' @concept outlier-detection
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#'
#' plotInfluence(fitLogit)
#' plotInfluence(fitLogit, metric = "deviance", labels = 3)
#'
#' inf <- plotInfluence(fitLogit)
#' head(inf[order(-inf$cook), ])
#'
#' @export
plotInfluence <- function(x,
                          main = NULL,
                          xlab = "fitted probability",
                          ylab = NULL,
                          xlim = NULL,
                          ylim = NULL,
                          metric = c("chisq", "deviance"),
                          threshold = NULL,
                          col = .useTheme,
                          border = .useTheme,
                          cex = 1,
                          grid = .useTheme,
                          box = .useTheme,
                          reference = TRUE,
                          labels = 5,
                          stamp = .useTheme,
                          ...) {

  metric <- match.arg(metric)

  parts <- .logitParts(x)
  fit   <- parts$fit

  h  <- stats::hatvalues(fit)
  rp <- stats::residuals(fit, type = "pearson")
  rd <- stats::residuals(fit, type = "deviance")

  # Standardization by 1 - h is what turns a raw residual into the
  # leave-one-out change; without it the two axes of this plot measure
  # nearly the same thing.
  rs <- rp / sqrt(1 - h)

  tab <- data.frame(
    p         = parts$p,
    hat       = h,
    residStd  = rs,
    dChisq    = rs^2,
    dDeviance = rd^2 / (1 - h),
    cook      = stats::cooks.distance(fit),
    row.names = names(parts$p)
  )

  yVal <- if (metric == "chisq") tab$dChisq else tab$dDeviance

  if (is.null(ylab))
    ylab <- if (metric == "chisq") "change in Pearson chi-squared"
            else                   "change in deviance"

  # The conventional cut-off of 4 is a chi-squared quantile and applies to
  # grouped data, where a covariate pattern has m > 1 trials. With m = 1 it
  # is crossed by every event below p = 0.2 and every non-event above 0.8,
  # so the line would only redraw the reference arms and invite a reading
  # they do not support. It is therefore off unless the data are grouped.
  if (is.null(threshold))
    threshold <- if (all(parts$m == 1)) NA else 4

  # --- style ------------------------------------------------------------
  th      <- getTheme()
  colBub  <- if (identical(col, .useTheme))
    addOpacity(th$twin[1L], 0.35) else col
  borBub  <- if (identical(border, .useTheme)) th$twin[1L] else border

  main <- .resolveTitle(main, default = .modelTitle(fit, "influence"))

  if (is.null(xlim)) xlim <- c(0, 1)
  if (is.null(ylim)) ylim <- c(0, max(yVal, na.rm = TRUE) * 1.05)

  # --- which points get a label ----------------------------------------
  lab <- .influenceLabels(labels, tab, yVal, threshold)

  .withGraphicsState({

    .applyParFromDots(
      ...,
      defaults = list(mar = c(bottom = 5, left = 4.6,
                              top = .marTop(main), right = 3.1))
    )

    plot(NA, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
         main = main, axes = FALSE)

    .drawGrid(grid)

    if (!is.na(threshold))
      graphics::abline(h = threshold, col = th$twin[2L], lty = "dashed")

    if (!isFALSE(reference) && !identical(reference, NA) &&
        all(parts$m == 1)) {

      pGrid <- seq(0.001, 0.999, length.out = 400L)
      arms  <- if (metric == "chisq")
        list((1 - pGrid) / pGrid, pGrid / (1 - pGrid))
      else
        list(-2 * log(pGrid), -2 * log(1 - pGrid))

      for (arm in arms)
        callIf(graphics::lines, reference,
               defaults = list(x = pGrid, y = arm,
                               col = th$twin[2L], lwd = 1, lty = "dotted"))
    }

    plotBubble(tab$p, yVal, area = tab$cook, add = TRUE,
               col = colBub, border = borBub, cex = cex)

    graphics::axis(1)
    graphics::axis(2, las = 1)
    .drawBox(box)

    if (!is.null(lab) && length(lab$which))
      callIf(
        boxedText, if (is.list(labels)) labels else TRUE,
        defaults = list(
          x = tab$p[lab$which], y = yVal[lab$which], labels = lab$text,
          # pos = 3 puts the box on top of its own bubble; sideways is the
          # only direction that stays clear of it, and the side has to flip
          # near the right edge or the label leaves the panel
          pos = ifelse(tab$p[lab$which] < mean(xlim), 4L, 2L),
          offset = 0.6, cex = 0.7, col = th$twin[2L])
      )

  }, stamp = stamp)

  invisible(tab)
}


# -------------------------------------------------------------------------
# Which observations to label
# -------------------------------------------------------------------------

# A number labels that many largest Cook's distances - the default, and the
# only selection that means anything with m = 1, where height above zero is
# a function of the fitted probability rather than a measure of misfit.
# TRUE labels everything above the threshold where one exists, and falls
# back to the five largest distances where it does not. A list is a
# styling instruction, not a selection.
#' @keywords internal
.influenceLabels <- function(labels, tab, yVal, threshold, k = 5L) {

  if (isFALSE(labels) || is.null(labels) || identical(labels, NA))
    return(NULL)

  idx <- if (is.numeric(labels) && !is.logical(labels)) {
    utils::head(order(tab$cook, decreasing = TRUE),
                min(as.integer(labels), nrow(tab)))
  } else if (is.na(threshold)) {
    utils::head(order(tab$cook, decreasing = TRUE), min(k, nrow(tab)))
  } else {
    which(yVal > threshold)
  }

  list(which = idx, text = rownames(tab)[idx])
}
