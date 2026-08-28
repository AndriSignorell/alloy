#' Binned Residual Plot
#'
#' Plots the mean response residual within bins of the fitted probabilities
#' or of a single predictor, together with a pointwise band under the
#' fitted model. This is the workhorse diagnostic for a binary response,
#' where a plain residual-versus-fitted plot only shows the two trivial
#' curves \eqn{-p} and \eqn{1 - p}.
#'
#' @details
#' Observations are grouped into bins of roughly equal size (quantiles of
#' the binning variable), and each bin contributes one point: the mean of
#' the binning variable against the mean residual \eqn{y - \hat p}. Under
#' a correct model the mean residual in a bin is centred at zero with
#' standard error \eqn{\sqrt{\overline{p(1-p)/m}/n_b}}, and roughly
#' \code{conf.level} of the points should fall inside the band.
#'
#' What the plot shows is not scatter but \emph{shape}. A run of points
#' outside the band on one side, or a systematic curve across the range,
#' is the signature of a missing term or a wrong functional form - which
#' is why the same plot against each continuous predictor
#' (\code{var = "age"}) is worth more than the one against the fitted
#' values: it says \emph{where} the model is wrong, not just \emph{that}
#' it is.
#'
#' A factor passed as \code{var} is grouped by its levels rather than by
#' quantiles, giving one point per level.
#'
#' \code{method} controls the band only:
#' \describe{
#'   \item{\code{"model"}}{binomial standard error implied by the fitted
#'     probabilities. Exact under the model and usable in small bins.}
#'   \item{\code{"empirical"}}{standard error from the spread within the
#'     bin. Assumes nothing about the model, but needs bins large enough
#'     to estimate a variance.}
#' }
#'
#' @param x a fitted logistic model of class \code{"FitMod"} (fitted with
#'   \code{fitfn = "logit"}) or a binomial \code{\link[stats]{glm}}.
#' @param var the binning variable. \code{NULL} (default) bins by the
#'   fitted probabilities; a character string names a variable in the
#'   model frame; a numeric or factor vector of length \eqn{n} is used
#'   directly.
#' @param main main title. \code{NULL} (default) derives one from
#'   \code{var}; \code{""}, \code{NA} or \code{FALSE} suppress it.
#' @param xlab,ylab axis labels. \code{NULL} derives them from the binning
#'   variable.
#' @param xlim,ylim axis limits. \code{NULL} (default) uses the range of
#'   the binned values and of the band.
#' @param nBins number of bins. \code{NULL} (default) uses
#'   \eqn{\lfloor\sqrt{n}\rfloor} for \eqn{n \geq 100}, 10 for
#'   \eqn{10 < n < 100} and \eqn{n/2} below that. Ignored for a factor.
#' @param conf.level level of the pointwise band. Default \code{0.95}.
#' @param method standard error of the band, \code{"model"} (default) or
#'   \code{"empirical"}. See Details.
#' @param col,bg,pch,cex point colour, fill, symbol and size.
#'   \code{.useTheme} (default) resolves against the active theme.
#' @param border colour of the band border. \code{NA} (default) draws none.
#' @param grid,box background grid and plot box, following the flexible
#'   \code{TRUE}/\code{FALSE}/\code{NA}/\code{list()} pattern.
#' @param labels bin labels for points falling outside the band.
#'   \code{FALSE} (default) draws none, \code{TRUE} labels them with the
#'   bin range, a named list is passed to \code{boxedText}.
#' @param stamp corner stamp, passed to the graphics framework.
#' @param ... further graphical parameters passed to \code{par()} via the
#'   internal framework.
#'
#' @return invisibly, the \code{\link{binnedResid}} table: one row per bin
#'   with the columns \code{bin}, \code{x}, \code{y} (the mean residual),
#'   \code{n}, \code{se} and the band bounds \code{lci} and \code{uci}.
#'
#' @references
#' Gelman, A. and Hill, J. (2007) \emph{Data Analysis Using Regression and
#' Multilevel/Hierarchical Models}. Cambridge University Press, ch. 5.
#'
#' @seealso
#' \code{\link{binnedResid}} for the numbers without a plot and for
#' several predictors at once; [model-diagnostics-overview] for an overview
#' of the diagnostics for logistic models in alloy.
#'
#' @concept regression-diagnostics
#' @concept binary-outcome
#' @concept scatterplot
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#'
#' plotBinnedResid(fitLogit)
#'
#' # against a single predictor - this is where a wrong functional form shows
#' plotBinnedResid(fitLogit, var = "gre")
#'
#' # one panel per predictor: compute once, then facet with free x scales
#' bins <- binnedResid(fitLogit, var = predictors(fitLogit))
#' plotFacet(bins, dim = c(1, 3), panelFun = panelBinnedResid,
#'           xlim = lapply(bins, function(b) range(b$x)),
#'           ylim = range(unlist(lapply(bins, function(b) c(b$lci, b$uci)))),
#'           stripLabels = vars, ylab = "mean residual")
#'
#' @export
plotBinnedResid <- function(x,
                            var = NULL,
                            main = NULL,
                            xlab = NULL,
                            ylab = NULL,
                            xlim = NULL,
                            ylim = NULL,
                            nBins = NULL,
                            conf.level = 0.95,
                            method = c("model", "empirical"),
                            col = .useTheme,
                            bg = .useTheme,
                            pch = .useTheme,
                            cex = .useTheme,
                            border = NA,
                            grid = .useTheme,
                            box = .useTheme,
                            labels = FALSE,
                            stamp = .useTheme,
                            ...) {

  mc <- match.call()

  # the arithmetic lives in binnedResid(); this function is the drawing of
  # it, and the two must not be able to drift apart
  tab <- binnedResid(
    x,
    var        = if (is.character(var)) var else eval.parent(mc$var),
    nBins      = nBins,
    conf.level = conf.level,
    method     = match.arg(method)
  )

  if (!is.data.frame(tab))
    stop("'var' must name a single binning variable; ",
         "use binnedResid() with plotFacet() for several")

  isCat <- attr(tab, "categorical")

  # --- resolve style ----------------------------------------------------
  th     <- getTheme()
  colPt  <- if (identical(col, .useTheme)) th$points$col else col
  bgPt   <- if (identical(bg,  .useTheme)) th$points$bg  else bg
  pchPt  <- if (identical(pch, .useTheme)) th$points$pch else pch
  cexPt  <- if (identical(cex, .useTheme)) th$points$cex else cex
  colBand <- addOpacity(th$twin[1L], 0.20)

  main <- .resolveTitle(main,
                        default = paste("binned residuals ~", attr(tab, "label")))
  if (is.null(xlab)) xlab <- attr(tab, "label")
  if (is.null(ylab)) ylab <- "mean residual"

  if (is.null(xlim))
    xlim <- if (isCat) c(0.5, nrow(tab) + 0.5) else range(tab$x, na.rm = TRUE)
  if (is.null(ylim)) ylim <- range(c(tab$y, tab$lci, tab$uci), na.rm = TRUE)

  .withGraphicsState({

    .applyParFromDots(
      ...,
      defaults = list(mar = c(bottom = 5, left = 4.6,
                              top = .marTop(main), right = 3.1))
    )

    plot(NA, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
         main = main, axes = FALSE)

    .drawGrid(grid)

    # The band is drawn before the points so it never covers them; it is a
    # reference, not data.
    ord <- order(tab$x)
    graphics::polygon(
      band(x = tab$x[ord], y = cbind(tab$uci[ord], tab$lci[ord])),
      col = colBand, border = border
    )

    graphics::abline(h = 0, col = th$twin[1L], lwd = 1.5)

    if (isCat)
      graphics::axis(1, at = tab$x, labels = tab$bin)
    else
      graphics::axis(1)

    graphics::axis(2, las = 1)

    graphics::points(tab$x, tab$y, pch = pchPt, col = colPt,
                     bg = bgPt, cex = cexPt)

    .drawBox(box)

    outside <- !is.na(tab$y) & (tab$y < tab$lci | tab$y > tab$uci)

    if (any(outside) && !isFALSE(labels))
      callIf(
        boxedText, labels,
        defaults = list(x = tab$x[outside], y = tab$y[outside],
                        labels = tab$bin[outside], pos = 3, cex = 0.7,
                        col = th$twin[2L])
      )

  }, stamp = stamp)

  invisible(tab)
}
