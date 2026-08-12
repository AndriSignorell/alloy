
#' Lift and gain table for a fitted model or predictor vector
#'
#' Computes lift and cumulative gain over the score distribution of a binary
#' classifier.  Lift quantifies how much better a model concentrates the
#' positive class in the top-scored cases than random selection would, and is
#' the standard evaluation view wherever only a limited fraction of the cases
#' can be acted upon - direct marketing, fraud triage, churn prevention.
#'
#' @param x Either a fitted binary classification model of class
#'   \code{"FitMod"} (in which case predicted probabilities and the response
#'   are extracted automatically), or a numeric vector of predicted
#'   probabilities / scores when \code{resp} is supplied.
#' @param resp Optional factor or binary vector of true class labels.  If
#'   \code{NULL} (default), \code{x} must be a \code{"FitMod"} object and
#'   the response is extracted via \code{\link{response}}.
#' @param nBins Number of equally sized score groups.  The default \code{10}
#'   yields the customary decile table.
#'
#' @return An object of class \code{"Lift"}, a data frame with one row per
#'   group and the columns:
#'   \item{bin}{group number, 1 = highest scores.}
#'   \item{nObs}{number of observations in the group.}
#'   \item{nPos}{number of positives in the group.}
#'   \item{rate}{hit rate within the group.}
#'   \item{lift}{group hit rate divided by the base rate.}
#'   \item{depth}{cumulative share of all observations up to this group.}
#'   \item{cumPos}{cumulative number of positives.}
#'   \item{cumRate}{cumulative hit rate.}
#'   \item{cumLift}{cumulative hit rate divided by the base rate.}
#'   \item{gain}{cumulative share of all positives captured.}
#'   The attributes \code{baseRate}, \code{nObs}, \code{nPos} and
#'   \code{positive} carry the overall figures.
#'
#' @details
#' As in \code{\link{roc}}, the second column of
#' \code{predict(x, type = "prob")} is used as the predictor when \code{x} is
#' a \code{"FitMod"} object - the positive class is therefore the second
#' factor level of the response.  For models with non-standard probability
#' output, supply the predictor vector explicitly via \code{x} and
#' \code{resp}.
#'
#' Cases are ranked by decreasing score and cut into \code{nBins} groups of
#' equal size.  Within group \eqn{i}, lift is the hit rate divided by the
#' overall base rate; cumulative lift uses the pooled hit rate over groups
#' \eqn{1..i}.  A cumulative lift of 2 at depth 0.2 means the top-scored
#' fifth of the cases contains twice the share of positives that a random
#' fifth would.
#'
#' Ties in the score are broken by the sort order.  With heavily tied scores
#' - a coarse tree with few distinct leaf probabilities, for instance -
#' group boundaries fall inside tie groups and the per-group lift becomes
#' correspondingly unstable; the cumulative curve is unaffected in
#' expectation but jagged.  Reduce \code{nBins} in that situation rather
#' than interpreting individual bins.
#'
#' Incomplete cases are dropped without an \code{na.rm} switch: a
#' score/response pair is structurally unusable when either side is missing.
#'
#' Lift and ROC rest on the same ranking; neither says anything about
#' calibration.  A model can show excellent lift while its predicted
#' probabilities are systematically biased.
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#' lift(fitLogit)
#'
#' # Supply predictor and response directly
#' p <- predict(fitLogit)[, 2]
#' lift(p, resp = Admit$admit)
#'
#' # Coarser grouping
#' lift(fitLogit, nBins = 5)
#'
#' @seealso \code{\link{roc}}, \code{\link{bestCut}}, \code{\link{response}},
#'   \code{pharos::plotLift()}

#' @family roc
#' @concept classification
#' @concept model-evaluation
#' @concept prediction
#'
#'
#' @export
lift <- function(x, resp = NULL, nBins = 10) {

  if (is.null(resp)) {
    pred <- predict(x, type = "prob")[, 2]
    resp <- response(x)
  } else {
    pred <- x
  }

  if (!is.numeric(pred) || !length(pred))
    stop("Argument 'x' must be numeric and non-empty.")

  if (length(pred) != length(resp))
    stop("Arguments 'x' and 'resp' must have the same length.")

  if (!is.numeric(nBins) || length(nBins) != 1L || is.na(nBins) || nBins < 2)
    stop("Argument 'nBins' must be a single integer >= 2.")

  ok   <- !is.na(pred) & !is.na(resp)
  pred <- pred[ok]
  resp <- resp[ok]

  if (!length(pred))
    stop("No complete cases in 'x' and 'resp'.")

  resp <- factor(resp)

  if (nlevels(resp) != 2L)
    stop("Argument 'resp' must have exactly two levels.")

  positive <- levels(resp)[2L]
  y        <- as.integer(resp == positive)

  nObs <- length(y)
  nPos <- sum(y)

  if (nPos == 0L || nPos == nObs)
    stop("Argument 'resp' must contain both classes.")

  nBins <- min(as.integer(nBins), nObs)

  y <- y[order(pred, decreasing = TRUE)]

  # equal-sized groups; the remainder is spread over the bins by rounding
  # the cut points, so group sizes differ by at most one
  binN <- diff(round(seq(0, nObs, length.out = nBins + 1L)))
  bin  <- factor(rep(seq_len(nBins), times = binN), levels = seq_len(nBins))

  binPos <- as.vector(tapply(y, bin, sum))

  baseRate <- nPos / nObs
  cumN     <- cumsum(binN)
  cumPos   <- cumsum(binPos)
  cumRate  <- cumPos / cumN

  structure(
    data.frame(
      bin     = seq_len(nBins),
      nObs    = binN,
      nPos    = binPos,
      rate    = binPos / binN,
      lift    = (binPos / binN) / baseRate,
      depth   = cumN / nObs,
      cumPos  = cumPos,
      cumRate = cumRate,
      cumLift = cumRate / baseRate,
      gain    = cumPos / nPos
    ),
    class    = c("Lift", "data.frame"),
    baseRate = baseRate,
    nObs     = nObs,
    nPos     = nPos,
    positive = positive
  )
}


#' @param digits Number of significant digits used for printing.
#' @param ... Further arguments, currently unused.
#'
#' @rdname lift
#' @export
print.Lift <- function(x, digits = 3, ...) {

  cat(gettextf("\nLift table (positive class: %s, base rate: %s)\n\n",
               attr(x, "positive"),
               format(attr(x, "baseRate"), digits = digits)))

  print(format(as.data.frame(unclass(x)), digits = digits), row.names = FALSE)

  cat(gettextf("\nn = %d, positives = %d\n\n",
               attr(x, "nObs"), attr(x, "nPos")))

  invisible(x)
}
