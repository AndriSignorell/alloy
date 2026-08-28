#' Separation Plot
#'
#' Draws one thin vertical line per observation, ordered by predicted
#' probability and coloured by the observed outcome. A model that
#' separates the classes shows the events collected on the right; one that
#' does not shows them evenly mixed. The whole sample fits in a single
#' strip, which makes it a compact companion to a numeric summary such as
#' the c statistic.
#'
#' @details
#' The plot answers a different question from
#' \code{\link{plotCalibration}}: separation is about the \emph{ordering}
#' of the predictions, calibration about their \emph{level}. A model can
#' rank perfectly and still predict risks twice too high, and the ROC
#' curve - being invariant to any monotone transformation of the
#' predictions - cannot see the difference either. Read the two plots
#' together.
#'
#' The marker on the axis sits at the expected number of events,
#' \eqn{\sum \hat p_i}, counted from the right. If it lands where the
#' observed events start, the model gets the overall event rate right.
#'
#' @param x a fitted logistic model of class \code{"FitMod"} (fitted with
#'   \code{fitfn = "logit"}) or a binomial \code{\link[stats]{glm}}.
#' @param main main title. \code{NULL} (default) derives one from the
#'   model formula; \code{""}, \code{NA} or \code{FALSE} suppress it.
#' @param xlab,ylab axis labels.
#' @param col colours for non-events and events, in that order.
#'   \code{.useTheme} (default) resolves against the active theme.
#' @param box plot box, following the flexible
#'   \code{TRUE}/\code{FALSE}/\code{NA}/\code{list()} pattern.
#' @param line the predicted probabilities drawn across the strip.
#'   \code{TRUE} (default), \code{FALSE}, or a named list passed to
#'   \code{lines}.
#' @param expected marker for the expected number of events. \code{TRUE}
#'   (default), \code{FALSE}, or a named list passed to \code{points}.
#' @param legend legend for the two outcome colours. \code{TRUE}
#'   (default), \code{FALSE}, or a named list passed to \code{legend}.
#' @param stamp corner stamp, passed to the graphics framework.
#' @param ... further graphical parameters passed to \code{par()} via the
#'   internal framework.
#'
#' @return invisibly, a data frame with the columns \code{p} and \code{y},
#'   sorted by \code{p}.
#'
#' @references
#' Greenhill, B., Ward, M. D. and Sacks, A. (2011) The separation plot: a
#' new visual method for evaluating the fit of binary models.
#' \emph{American Journal of Political Science}, \bold{55}(4), 991--1002.
#'
#' @seealso
#' [model-diagnostics-overview] for an overview of the diagnostics for
#' logistic models in alloy.
#'
#' @concept regression-diagnostics
#' @concept binary-outcome
#' @concept classification
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#'
#' plotSeparation(fitLogit)
#'
#' @export
plotSeparation <- function(x,
                           main = NULL,
                           xlab = "observations ordered by predicted probability",
                           ylab = "",
                           col = .useTheme,
                           box = .useTheme,
                           line = TRUE,
                           expected = TRUE,
                           legend = TRUE,
                           stamp = .useTheme,
                           ...) {

  parts <- .logitParts(x)

  if (any(parts$m != 1))
    stop("the separation plot is defined for binary observations; ",
         "this model has grouped (m > 1) responses")

  ord <- order(parts$p)
  p   <- parts$p[ord]
  y   <- parts$y[ord]
  n   <- length(p)

  th  <- getTheme()
  colUse <- if (identical(col, .useTheme))
    c(addOpacity(th$twin[1L], 0.30), th$twin[2L]) else col

  if (length(colUse) != 2L)
    stop("argument 'col' must give exactly two colours (non-event, event)")

  main <- .resolveTitle(main, default = .modelTitle(parts$fit, "separation"))

  .withGraphicsState({

    .applyParFromDots(
      ...,
      defaults = list(mar = c(bottom = 4, left = 1.5,
                              top = .marTop(main), right = 1.5))
    )

    plot(NA, xlim = c(0, n), ylim = c(0, 1), xlab = xlab, ylab = ylab,
         main = main, axes = FALSE)

    # One rectangle per observation, drawn without a border: at n in the
    # hundreds the borders alone would fill the strip.
    graphics::rect(
      xleft   = seq_len(n) - 1,
      ybottom = 0,
      xright  = seq_len(n),
      ytop    = 1,
      col     = colUse[y + 1L],
      border  = NA
    )

    callIf(graphics::lines, line,
           defaults = list(x = seq_len(n) - 0.5, y = p,
                           col = th$points$col, lwd = 2))

    callIf(graphics::points, expected,
           defaults = list(x = n - sum(p), y = 0, pch = 17,
                           col = th$points$col, cex = 1.2, xpd = TRUE))

    .drawBox(box)

    callIf(graphics::legend, legend,
           defaults = list(x = "topleft", legend = c("non-event", "event"),
                           fill = colUse, border = NA, bty = "n",
                           cex = 0.8, inset = c(0, -0.02)))

  }, stamp = stamp)

  invisible(data.frame(p = p, y = y, row.names = names(p)))
}
