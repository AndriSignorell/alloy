#' Binned Residuals
#'
#' Computes the mean response residual within bins of the fitted
#' probabilities or of one or more predictors, together with the pointwise
#' band implied by the model. This is the computation behind
#' \code{\link{plotBinnedResid}}, separated out so that the numbers can be
#' had without a device and several predictors can be prepared in one call.
#'
#' @details
#' Observations are grouped into bins of roughly equal size (quantiles of
#' the binning variable) and each bin contributes one row: the mean of the
#' binning variable, the mean residual \eqn{y - \hat p}, the bin size and
#' the band. Under a correct model the mean residual in a bin is centred at
#' zero with standard error \eqn{\sqrt{\overline{p(1-p)/m}/n_b}}.
#'
#' A factor is grouped by its levels rather than by quantiles, giving one
#' row per level. A count variable with few distinct values is the awkward
#' case in between: quantile breaks collapse, fewer bins than requested
#' come back, and the warning saying so is worth heeding - passing the
#' variable as a factor is usually the more honest treatment.
#'
#' \code{method} controls the band only:
#' \describe{
#'   \item{\code{"model"}}{binomial standard error implied by the fitted
#'     probabilities. Exact under the model and usable in small bins.}
#'   \item{\code{"empirical"}}{standard error from the spread within the
#'     bin. Assumes nothing about the model, but needs bins large enough to
#'     estimate a variance.}
#' }
#'
#' @param x a fitted logistic model of class \code{"FitMod"} (fitted with
#'   \code{fitfn = "logit"}) or a binomial \code{\link[stats]{glm}}.
#' @param var the binning variable. \code{NULL} (default) bins by the
#'   fitted probabilities; a character \emph{vector} names variables in the
#'   model frame (or, for a transformed term, in the data the model was
#'   fitted from); a numeric or factor vector of length \eqn{n} is used
#'   directly.
#' @param nBins number of bins. \code{NULL} (default) uses
#'   \eqn{\lfloor\sqrt{n}\rfloor} for \eqn{n \geq 100}, 10 for
#'   \eqn{10 < n < 100} and \eqn{n/2} below that. Ignored for a factor.
#' @param conf.level level of the pointwise band. Default \code{0.95}.
#' @param method standard error of the band, \code{"model"} (default) or
#'   \code{"empirical"}. See Details.
#'
#' @return for a single binning variable, a data frame with one row per bin
#'   and the columns
#'   \describe{
#'     \item{\code{bin}}{the bin label - the interval, or the factor level}
#'     \item{\code{x}}{mean of the binning variable in the bin}
#'     \item{\code{y}}{mean residual in the bin. Named \code{y} rather than
#'       \code{resid} so the result can go straight into
#'       \code{\link[pharos]{plotFacet}}, whose samples are \code{x}/\code{y}}
#'     \item{\code{n}}{number of observations in the bin}
#'     \item{\code{se}}{standard error of the mean residual}
#'     \item{\code{lci}, \code{uci}}{the band around zero}
#'   }
#'   with attributes \code{label} (the name of the binning variable),
#'   \code{categorical}, \code{method}, \code{conf.level} and
#'   \code{outside} (the number of bins outside the band).
#'
#'   For several binning variables, a named list of such data frames.
#'
#' @references
#' Gelman, A. and Hill, J. (2007) \emph{Data Analysis Using Regression and
#' Multilevel/Hierarchical Models}. Cambridge University Press, ch. 5.
#'
#' @seealso
#' [model-diagnostics-overview] for an overview of the diagnostics for
#' logistic models in alloy.
#'
#' @concept regression-diagnostics
#' @concept binary-outcome
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#'
#' binnedResid(fitLogit, var = "gre")
#'
#' # all predictors at once, ranked by how badly they fit
#' bins <- binnedResid(fitLogit, var = predictors(fitLogit))
#' sort(sapply(bins, attr, "outside"), decreasing = TRUE)
#'
#' @export
binnedResid <- function(x,
                        var = NULL,
                        nBins = NULL,
                        conf.level = 0.95,
                        method = c("model", "empirical")) {

  method     <- match.arg(method)
  conf.level <- checkConfLevel(conf.level)

  parts <- .logitParts(x)

  label <- if (is.character(var)) var else deparse1(substitute(var))

  # several names mean several tables; everything else is one
  if (is.character(var) && length(var) > 1L)
    return(stats::setNames(
      lapply(var, function(v)
        binnedResid(x, var = v, nBins = nBins, conf.level = conf.level,
                    method = method)),
      var))

  .binnedResidOne(parts, var, label, nBins, conf.level, method)
}


#' @keywords internal
.binnedResidOne <- function(parts, var, label, nBins, conf.level, method) {

  binVar <- .resolveBinVar(parts$fit, var, n = parts$n, label = label)

  if (is.null(nBins))
    nBins <- .defaultBins(parts$n)

  bin <- .binGroups(binVar$values, nBins)

  # a categorical binning variable has no numeric axis of its own; the bins
  # are placed at 1..k and the levels become the labels
  isCat <- is.factor(bin) && !is.numeric(binVar$values)
  xVal  <- if (isCat)
    as.numeric(as.integer(droplevels(factor(binVar$values))))
  else binVar$values

  z   <- stats::qnorm(1 - (1 - conf.level) / 2)
  tab <- .binStats(bin, xVal = xVal, resid = parts$y - parts$p,
                   p = parts$p, m = parts$m, method = method, z = z)

  attr(tab, "label")       <- binVar$label
  attr(tab, "categorical") <- isCat
  attr(tab, "method")      <- method
  attr(tab, "conf.level")  <- conf.level
  attr(tab, "outside")     <- sum(!is.na(tab$y) &
                                    (tab$y < tab$lci | tab$y > tab$uci))
  tab
}


#' Facet Panel for Binned Residuals
#'
#' A panel function for \code{\link[pharos]{plotFacet}} that draws one
#' binned residual panel: the band, the zero line and the bin means. Pass
#' the output of \code{\link{binnedResid}} as the samples.
#'
#' @param x,y the bin midpoints and mean residuals, supplied by
#'   \code{plotFacet} from the sample's \code{x} and \code{y} components.
#' @param lci,uci the band, supplied from the sample's components of the
#'   same name.
#' @param col,pch colour and symbol of the bin means, supplied by
#'   \code{plotFacet}.
#' @param bandCol fill colour of the band. \code{NULL} (default) takes it
#'   from the active theme.
#' @param ... further arguments passed to \code{\link{points}}.
#'
#' @return called for its side effect; returns \code{NULL} invisibly.
#'
#' @seealso
#' [model-diagnostics-overview] for an overview of the diagnostics for
#' logistic models in alloy.
#'
#' @concept regression-diagnostics
#' @concept binary-outcome
#' @concept panel
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#' bins <- binnedResid(fitLogit, var = predictors(fitLogit))
#' vars <- predictors(fitLogit)
#' 
#' pharos::plotFacet(bins, dim = c(1, 3), panelFun = panelBinnedResid,
#'                   xlim = lapply(bins, function(b) range(b$x)),
#'                   ylim = range(unlist(lapply(bins, function(b) c(b$lci, b$uci)))),
#'                   stripLabels = vars, ylab = "mean residual")
#'
#' @export
panelBinnedResid <- function(x, y, lci, uci, col, pch = 16,
                             bandCol = NULL, ...) {

  th <- getTheme()

  if (is.null(bandCol))
    bandCol <- addOpacity(th$twin[1L], 0.20)

  ord <- order(x)

  graphics::polygon(band(x = x[ord], y = cbind(uci[ord], lci[ord])),
                    col = bandCol, border = NA)

  graphics::abline(h = 0, col = th$twin[1L], lwd = 1.5)
  graphics::points(x, y, col = col, pch = pch, ...)

  invisible(NULL)
}


# -------------------------------------------------------------------------
# Resolve the binning variable and its label
# -------------------------------------------------------------------------

# var = NULL bins by the fitted probabilities, a string names a column of
# the model frame, and a vector is taken as is. The label follows the same
# three cases, so the axis always names what was actually binned.
#' @keywords internal
.resolveBinVar <- function(fit, var, n, label) {

  if (is.null(var))
    return(list(values = stats::fitted(fit), label = "fitted probability"))

  if (is.character(var)) {

    if (length(var) != 1L)
      stop("argument 'var' must name a single variable")

    mf <- stats::model.frame(fit)

    if (var %in% names(mf))
      return(list(values = mf[[var]], label = var))

    # A transformed term is stored under its expression ("I(x^2)"), so the
    # original variable is not in the model frame at all. Binning against
    # it is exactly what one wants after fitting a transformation, so fall
    # back to the data the model was fitted from.
    dat <- fit$data

    if (is.data.frame(dat) && var %in% names(dat)) {
      values <- dat[[var]]

      # Rows dropped by na.action are gone from the model frame but still
      # in the data; the row names are what lines the two up again.
      if (length(values) != n)
        values <- values[match(rownames(mf), rownames(dat))]

      if (length(values) != n)
        stop(gettextf(
          "'%s' comes from the model data but has %d values for %d fitted observations",
          var, length(values), n), domain = NA)
      return(list(values = values, label = var))
    }

    stop(gettextf("'%s' is neither in the model frame (%s) nor in the model data",
                  var, paste(names(mf)[-1L], collapse = ", ")), domain = NA)
  }

  if (length(var) != n)
    stop(gettextf(
      "argument 'var' has length %d, but the model has %d observations",
      length(var), n), domain = NA)

  list(values = var, label = label)
}
