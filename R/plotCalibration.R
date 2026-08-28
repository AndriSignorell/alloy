#' Calibration Curve
#'
#' Plots observed event rates against predicted probabilities, with a
#' smoother and its confidence band, the diagonal of perfect calibration,
#' and the calibration intercept and slope. Where a goodness-of-fit test
#' returns a single p-value, this shows \emph{where} and \emph{by how
#' much} the predictions are off.
#'
#' @details
#' Two numbers summarise the curve, both from a logistic regression of the
#' response on the linear predictor \eqn{\hat\eta = \mathrm{logit}(\hat p)}:
#'
#' \describe{
#'   \item{intercept}{\eqn{\alpha} from \eqn{y \sim \mathrm{offset}
#'     (\hat\eta)} - calibration-in-the-large. Zero when the predicted
#'     risks are right on average; negative when the model predicts too
#'     much risk overall.}
#'   \item{slope}{\eqn{\beta} from \eqn{y \sim \hat\eta}. One when the
#'     predictions are neither too extreme nor too flat; below one is the
#'     signature of overfitting, the usual finding when a model is
#'     evaluated on the data it was fitted to.}
#' }
#'
#' On the development sample both are one and zero \emph{by construction}
#' for a logistic model fitted by maximum likelihood - the curve then only
#' shows departures from linearity in the logit, not overfitting. The
#' numbers earn their meaning on new data (\code{newdata}) or a resampled
#' estimate; \code{plotCalibration} reports them either way, and says
#' which case it is in the annotation.
#'
#' The smoother is a loess of the binary response on the fitted
#' probability. Its band is pointwise and rests on constant variance,
#' which a binary response does not have; read it as an indication of
#' where the data are thin, not as a simultaneous confidence region.
#'
#' @param x a fitted logistic model of class \code{"FitMod"} (fitted with
#'   \code{fitfn = "logit"}) or a binomial \code{\link[stats]{glm}}.
#' @param newdata optional data frame for evaluating calibration out of
#'   sample. \code{NULL} (default) uses the training data.
#' @param main main title. \code{NULL} (default) derives one from the
#'   model formula; \code{""}, \code{NA} or \code{FALSE} suppress it.
#' @param xlab,ylab axis labels.
#' @param xlim,ylim axis limits. \code{NULL} (default) uses
#'   \eqn{[0, 1]} clipped to the range of the predictions.
#' @param nBins number of bins for the observed proportions drawn on top
#'   of the smoother. \code{NULL} (default) uses 10; \code{0} or
#'   \code{FALSE} suppresses them.
#' @param conf.level level of the confidence band. Default \code{0.95}.
#' @param col,bg,pch,cex colour, fill, symbol and size of the binned
#'   points. \code{.useTheme} (default) resolves against the active theme.
#' @param grid,box background grid and plot box, following the flexible
#'   \code{TRUE}/\code{FALSE}/\code{NA}/\code{list()} pattern.
#' @param smooth the loess smoother and its band. \code{TRUE} (default)
#'   draws it with defaults, \code{FALSE}/\code{NA} suppresses it, a named
#'   list is passed to \code{lines.loess} (e.g.
#'   \code{list(bandArgs = FALSE)}).
#' @param rug marks for the individual predictions, events above and
#'   non-events below the panel. \code{TRUE} (default), \code{FALSE}, or a
#'   named list passed to \code{rug}.
#' @param legend annotation with intercept, slope and Brier score.
#'   \code{TRUE} (default), \code{FALSE}, or a named list passed to
#'   \code{boxedText}. Its \code{x} element may be one of the named
#'   positions of \code{\link[pharos]{abcCoords}} (\code{"topleft"} by
#'   default, \code{"bottomright"} where the curve runs through the upper
#'   corner).
#' @param stamp corner stamp, passed to the graphics framework.
#' @param ... further graphical parameters passed to \code{par()} via the
#'   internal framework.
#'
#' @return invisibly, a list with the components \code{intercept},
#'   \code{slope}, \code{brier} (scaled and unscaled), \code{bins} (the
#'   binned observed proportions) and \code{inSample}.
#'
#' @references
#' Steyerberg, E. W. et al. (2010) Assessing the performance of prediction
#' models: a framework for traditional and novel measures.
#' \emph{Epidemiology}, \bold{21}(1), 128--138.
#'
#' Van Calster, B. et al. (2019) Calibration: the Achilles heel of
#' predictive analytics. \emph{BMC Medicine}, \bold{17}, 230.
#'
#' @seealso
#' [model-diagnostics-overview] for an overview of the diagnostics for
#' logistic models in alloy.
#'
#' @concept regression-diagnostics
#' @concept calibration
#' @concept binary-outcome
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#'
#' plotCalibration(fitLogit)
#'
#' # out of sample, where intercept and slope carry information
#' idx <- sample(nrow(Admit), nrow(Admit) * 0.7)
#' fitTrain <- fitMod(admit ~ gre + gpa + rank, Admit[idx, ], fitfn = "logit")
#' plotCalibration(fitTrain, newdata = Admit[-idx, ])
#'
#' @export
plotCalibration <- function(x,
                            newdata = NULL,
                            main = NULL,
                            xlab = "predicted probability",
                            ylab = "observed proportion",
                            xlim = NULL,
                            ylim = NULL,
                            nBins = NULL,
                            conf.level = 0.95,
                            col = .useTheme,
                            bg = .useTheme,
                            pch = .useTheme,
                            cex = .useTheme,
                            grid = .useTheme,
                            box = .useTheme,
                            smooth = TRUE,
                            rug = TRUE,
                            legend = TRUE,
                            stamp = .useTheme,
                            ...) {

  conf.level <- checkConfLevel(conf.level)

  parts <- .logitParts(x)

  if (any(parts$m != 1))
    stop("calibration is defined for binary observations; ",
         "this model has grouped (m > 1) responses")

  # --- predictions and response ----------------------------------------
  if (is.null(newdata)) {
    p <- parts$p
    y <- parts$y
    inSample <- TRUE
  } else {
    p <- stats::predict(parts$fit, newdata = newdata, type = "response")
    y <- .responseFromData(parts$fit, newdata)
    inSample <- FALSE
  }

  ok <- !is.na(p) & !is.na(y)
  p  <- p[ok]
  y  <- y[ok]

  # --- calibration intercept and slope ----------------------------------
  eta <- stats::qlogis(.clampProb(p))

  intercept <- unname(stats::coef(
    stats::glm(y ~ 1, family = stats::binomial, offset = eta)))
  slope <- unname(stats::coef(
    stats::glm(y ~ eta, family = stats::binomial))[2L])

  brier       <- brierScore(y, p)
  brierScaled <- brierScore(y, p, scaled = TRUE)

  # --- binned observed proportions --------------------------------------
  if (is.null(nBins)) nBins <- 10L
  drawBins <- !isFALSE(nBins) && !identical(nBins, 0)

  bins <- if (drawBins) {
    g <- droplevels(.binGroups(p, as.integer(nBins)))
    data.frame(
      bin  = levels(g),
      p    = as.vector(tapply(p, g, mean)),
      obs  = as.vector(tapply(y, g, mean)),
      n    = as.vector(tapply(y, g, length)),
      row.names = NULL, stringsAsFactors = FALSE
    )
  } else NULL

  # --- style ------------------------------------------------------------
  th    <- getTheme()
  colPt <- if (identical(col, .useTheme)) th$points$col else col
  bgPt  <- if (identical(bg,  .useTheme)) th$points$bg  else bg
  pchPt <- if (identical(pch, .useTheme)) th$points$pch else pch
  cexPt <- if (identical(cex, .useTheme)) th$points$cex else cex

  main <- .resolveTitle(main, default = .modelTitle(parts$fit, "calibration"))

  rng <- range(p, na.rm = TRUE)
  if (is.null(xlim)) xlim <- rng
  if (is.null(ylim)) ylim <- c(0, 1)

  .withGraphicsState({

    .applyParFromDots(
      ...,
      defaults = list(mar = c(bottom = 5, left = 4.6,
                              top = .marTop(main), right = 3.1))
    )

    plot(NA, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
         main = main, axes = FALSE)

    .drawGrid(grid)

    # Perfect calibration, drawn first: everything else is read as a
    # departure from this line.
    graphics::abline(0, 1, col = th$twin[2L], lty = "dashed")

    if (!isFALSE(smooth) && !identical(smooth, NA)) {
      lo <- stats::loess(y ~ p, degree = 1L, family = "gaussian")
      callIf(
        graphics::lines, smooth,
        defaults = list(x = lo, col = th$twin[1L],
                        bandArgs = list(conf.level = conf.level))
      )
    }

    if (drawBins)
      graphics::points(bins$p, bins$obs, pch = pchPt, col = colPt,
                       bg = bgPt, cex = cexPt)

    if (!isFALSE(rug) && !identical(rug, NA)) {
      callIf(graphics::rug, rug,
             defaults = list(x = p[y == 1], side = 3, col = th$twin[1L]))
      callIf(graphics::rug, rug,
             defaults = list(x = p[y == 0], side = 1, col = th$twin[1L]))
    }

    graphics::axis(1)
    graphics::axis(2, las = 1)
    .drawBox(box)

    if (!isFALSE(legend) && !identical(legend, NA)) {

      # boxedText() writes at user coordinates and has no keyword
      # positions of its own; abcCoords() supplies both the anchor and the
      # matching adj, with the inset in character units rather than as a
      # fraction of the axis range.
      anchor <- abcCoords(.legendAnchor(legend), region = "plot",
                          cex = 0.8, inset = 1)

      callIf(
        boxedText, .dropAnchor(legend),
        defaults = list(
          x = anchor$xy$x, y = anchor$xy$y, adj = anchor$adj,
          labels = paste0(
            # in-sample the intercept is zero by construction, but only up
            # to the convergence tolerance - printed raw it arrives as
            # -3.7e-13 and reads like a finding
            "intercept: ", fm(.roundForDisplay(intercept, 2), digits = 2), "\n",
            "slope: ",     fm(.roundForDisplay(slope, 2),     digits = 2), "\n",
            "Brier: ",     fm(brier,     digits = 3),
            if (inSample) "\n(in-sample fit)" else "\n(validation sample)"),
          cex = 0.8, bg = addOpacity("white", 0.8), border = NA)
      )
    }

  }, stamp = stamp)

  invisible(list(
    intercept   = intercept,
    slope       = slope,
    brier       = brier,
    brierScaled = brierScaled,
    bins        = bins,
    inSample    = inSample
  ))
}


# -------------------------------------------------------------------------
# Response of a validation sample, coded as the model codes it
# -------------------------------------------------------------------------

# The response has to be looked up in newdata with the model's own terms,
# so that a factor is mapped to 0/1 the same way glm() mapped it during
# fitting - not by whatever order the levels happen to have in newdata.
#' @keywords internal
.responseFromData <- function(fit, newdata) {

  respName <- all.vars(stats::formula(fit))[1L]

  if (!(respName %in% names(newdata)))
    stop(gettextf("the response '%s' is missing from 'newdata'", respName),
         domain = NA)

  y <- newdata[[respName]]

  if (is.factor(y) || is.character(y)) {
    lev <- levels(stats::model.frame(fit)[[1L]])
    if (is.null(lev)) lev <- sort(unique(as.character(y)))
    y <- as.integer(factor(as.character(y), levels = lev)) - 1L
  } else if (is.logical(y)) {
    y <- as.integer(y)
  }

  y <- as.numeric(y)

  if (!all(y %in% c(0, 1) | is.na(y)))
    stop("the response in 'newdata' is not binary")

  y
}


# -------------------------------------------------------------------------
# Named anchor for the annotation
# -------------------------------------------------------------------------

# The legend argument follows the flexible pattern, so its list form may
# carry graphical parameters for boxedText() and, in x, a named position.
# The two are separated here: the position goes to abcCoords(), the rest
# stays a boxedText() argument. A numeric x is passed through untouched
# and simply overrides the anchor.
#' @keywords internal
.legendAnchor <- function(legend, default = "topleft") {
  if (is.list(legend) && is.character(legend$x)) legend$x else default
}

#' @keywords internal
.dropAnchor <- function(legend) {
  if (is.list(legend) && is.character(legend$x)) legend$x <- NULL
  legend
}
