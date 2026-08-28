#' Partial Residual Plot for Linearity in the Logit
#'
#' Plots the partial (component-plus-) residuals of one predictor against
#' its values, with a smoother. The logistic model assumes the log odds
#' are linear in each continuous predictor, and this is the plot that
#' checks that assumption term by term.
#'
#' @details
#' The partial residual for term \eqn{j} is
#' \deqn{u_i = \hat\beta_j x_{ij} + (y_i - \hat p_i)/\{\hat p_i(1-\hat p_i)\},}
#' the fitted contribution of the term plus the working residual on the
#' logit scale. If the term enters correctly, the smoother follows the
#' straight line; curvature is a direct reading of the transformation the
#' term wants - a bend of one sign suggesting a square term, a logarithmic
#' shape suggesting a log.
#'
#' The working residual has variance \eqn{1/\{m\hat p(1-\hat p)\}}, so
#' points at extreme fitted probabilities are enormously more variable
#' than those in the middle. The smoother is weighted accordingly
#' (\eqn{w_i = m_i \hat p_i(1-\hat p_i)}); without the weights the tails
#' of the plot dominate the curve and suggest structure that is not there.
#'
#' A useful confirmation costs one line: refit with a spline in the
#' suspect term and compare. If the smoother bends but the likelihood
#' ratio test does not, the bend is noise.
#'
#' @param x a fitted logistic model of class \code{"FitMod"} (fitted with
#'   \code{fitfn = "logit"}) or a binomial \code{\link[stats]{glm}}.
#' @param term name of the predictor. \code{NULL} (default) uses the first
#'   continuous term in the model; \code{\link{predictors}(x, numeric =
#'   TRUE)} lists the ones this plot can be drawn for.
#' @param main main title. \code{NULL} (default) derives one from
#'   \code{term}; \code{""}, \code{NA} or \code{FALSE} suppress it.
#' @param xlab,ylab axis labels. \code{NULL} derives them from
#'   \code{term}.
#' @param xlim,ylim axis limits.
#' @param col,bg,pch,cex point colour, fill, symbol and size.
#'   \code{.useTheme} (default) resolves against the active theme.
#' @param grid,box background grid and plot box, following the flexible
#'   \code{TRUE}/\code{FALSE}/\code{NA}/\code{list()} pattern.
#' @param smooth the loess smoother and its band. \code{TRUE} (default),
#'   \code{FALSE}, or a named list passed to \code{lines.loess}.
#' @param lm the fitted linear contribution of the term. \code{TRUE}
#'   (default), \code{FALSE}, or a named list passed to \code{lines}.
#' @param stamp corner stamp, passed to the graphics framework.
#' @param ... further graphical parameters passed to \code{par()} via the
#'   internal framework.
#'
#' @return invisibly, a data frame with the columns \code{x} (the
#'   predictor), \code{partial} (the partial residual) and \code{weight}.
#'
#' @seealso
#' [model-diagnostics-overview] for an overview of the diagnostics for
#' logistic models in alloy.
#'
#' @concept regression-diagnostics
#' @concept binary-outcome
#' @concept scatterplot
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#'
#' plotPartialResid(fitLogit, term = "gre")
#'
#' # confirm a suspected bend against a spline fit
#' fitSpline <- fitMod(admit ~ splines::ns(gre, 3) + gpa + rank, Admit,
#'                     fitfn = "logit")
#' anova(fitLogit, fitSpline, test = "LRT")
#'
#' @export
plotPartialResid <- function(x,
                             term = NULL,
                             main = NULL,
                             xlab = NULL,
                             ylab = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             col = .useTheme,
                             bg = .useTheme,
                             pch = .useTheme,
                             cex = .useTheme,
                             grid = .useTheme,
                             box = .useTheme,
                             smooth = TRUE,
                             lm = TRUE,
                             stamp = .useTheme,
                             ...) {

  parts <- .logitParts(x)
  fit   <- parts$fit

  term <- .resolveTerm(fit, term)

  mf  <- stats::model.frame(fit)
  xv  <- mf[[term]]

  if (!is.numeric(xv))
    stop(gettextf(
      "'%s' is not numeric; linearity in the logit is only defined for a continuous term",
      term), domain = NA)

  # The coefficient of a numeric term appears under its own name in the
  # design matrix, unless it entered through a transformation - in which
  # case there is no single slope to add back and the plot is refused
  # rather than drawn against an arbitrary column.
  cf <- stats::coef(fit)
  if (!(term %in% names(cf)))
    stop(gettextf(
      "no single coefficient for '%s'; a transformed term has no partial residual plot of its own",
      term), domain = NA)

  p    <- parts$p
  w    <- parts$m * p * (1 - p)
  work <- (parts$y - p) / (p * (1 - p))

  partial <- unname(cf[term]) * xv + work

  tab <- data.frame(x = xv, partial = partial, weight = w,
                    row.names = names(p))

  # --- style ------------------------------------------------------------
  th    <- getTheme()
  colPt <- if (identical(col, .useTheme)) th$points$col else col
  bgPt  <- if (identical(bg,  .useTheme)) th$points$bg  else bg
  pchPt <- if (identical(pch, .useTheme)) th$points$pch else pch
  cexPt <- if (identical(cex, .useTheme)) th$points$cex else cex

  main <- .resolveTitle(main, default = paste("partial residuals:", term))
  if (is.null(xlab)) xlab <- term
  if (is.null(ylab)) ylab <- paste("partial residual (logit scale)")

  if (is.null(xlim)) xlim <- range(xv, na.rm = TRUE)
  if (is.null(ylim)) ylim <- stats::quantile(partial, c(0.005, 0.995),
                                             na.rm = TRUE, names = FALSE)

  .withGraphicsState({

    .applyParFromDots(
      ...,
      defaults = list(mar = c(bottom = 5, left = 4.6,
                              top = .marTop(main), right = 3.1))
    )

    plot(NA, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
         main = main, axes = FALSE)

    .drawGrid(grid)

    graphics::points(xv, partial, pch = pchPt, col = colPt, bg = bgPt,
                     cex = cexPt)

    callIf(graphics::abline, lm,
           defaults = list(a = 0, b = unname(cf[term]),
                           col = th$twin[2L], lty = "dashed"))

    if (!isFALSE(smooth) && !identical(smooth, NA)) {
      lo <- stats::loess(partial ~ xv, weights = w, degree = 1L,
                         family = "gaussian")
      callIf(graphics::lines, smooth,
             defaults = list(x = lo, col = th$twin[1L]))
    }

    graphics::axis(1)
    graphics::axis(2, las = 1)
    .drawBox(box)

  }, stamp = stamp)

  invisible(tab)
}


# -------------------------------------------------------------------------
# Pick the term to plot
# -------------------------------------------------------------------------

#' @keywords internal
.resolveTerm <- function(fit, term) {

  mf    <- stats::model.frame(fit)
  preds <- attr(stats::terms(fit), "term.labels")
  num   <- preds[vapply(preds, function(nm)
    !is.null(mf[[nm]]) && is.numeric(mf[[nm]]), logical(1L))]

  if (is.null(term)) {
    if (!length(num))
      stop("the model has no continuous predictor to check for linearity")
    return(num[1L])
  }

  if (!is.character(term) || length(term) != 1L)
    stop("argument 'term' must name a single predictor")

  if (!(term %in% preds))
    stop(gettextf("'%s' is not a predictor in the model", term), domain = NA)

  term
}
