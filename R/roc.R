
#' ROC curve for a fitted model or predictor vector
#'
#' Convenience wrapper around \code{\link[pROC]{roc}} that accepts either a
#' fitted \code{"FitMod"} binary classification model or a numeric predictor
#' vector directly.
#'
#' @param x Either a fitted binary classification model of class
#'   \code{"FitMod"} (in which case predicted probabilities and the response
#'   are extracted automatically), or a numeric vector of predicted
#'   probabilities / scores when \code{resp} is supplied.
#' @param resp Optional factor or binary vector of true class labels.  If
#'   \code{NULL} (default), \code{x} must be a \code{"FitMod"} object and
#'   the response is extracted via \code{\link{response}}.
#' @param ... Further arguments passed to \code{\link[pROC]{roc}}.
#'
#' @return An object of class \code{"roc"} as returned by
#'   \code{\link[pROC]{roc}}.
#'
#' @details
#' When \code{x} is a \code{"FitMod"} object, the second column of
#' \code{predict(x, type = "prob")} is used as the predictor (i.e. the
#' probability of the second factor level).  For models with non-standard
#' probability output, supply the predictor vector explicitly via \code{x}
#' and \code{resp}.
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#' r <- roc(fitLogit)
#' plot(r)
#'
#' # Supply predictor and response directly
#' p <- predict(fitLogit)[, 2]
#' r2 <- roc(p, resp = Admit$admit)
#'
#' @seealso \code{\link[pROC]{roc}}, \code{\link{bestCut}},
#'   \code{\link{response}}

#' @family roc  
#' @concept classification  
#' @concept model-evaluation  
#' @concept roc
#'
#'
#' @export
roc <- function(x, resp = NULL, ...) {
  if (is.null(resp))
    pROC::roc(predictor = predict(x, type = "prob")[, 2],
              response  = response(x),
              plot      = FALSE, ...)
  else
    pROC::roc(predictor = x,
              response  = resp,
              plot      = FALSE, ...)
}


#' Confidence intervals for ROC curve coordinates
#'
#' Computes confidence intervals for sensitivity, specificity, or other
#' coordinates of an ROC curve via \code{\link[pROC]{ci.coords}}.
#'
#' @param object An object of class \code{"roc"} as returned by
#'   \code{\link[pROC]{roc}} or \code{\link{roc}}.
#' @param parm Currently unused; present for compatibility with the generic
#'   \code{\link[stats]{confint}}.
#' @param x Coordinate at which to evaluate the confidence interval.
#'   Either a numeric value (e.g. a threshold or specificity value) or
#'   one of the special strings accepted by
#'   \code{\link[pROC]{ci.coords}}: \code{"best"} (default),
#'   \code{"all"}, or \code{"local maximas"}.
#' @param level Confidence level.  Default is \code{0.95}.
#' @param ... Further arguments passed to \code{\link[pROC]{ci.coords}}.
#'
#' @return A \code{"ci.coords"} object as returned by
#'   \code{\link[pROC]{ci.coords}}.
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#' r <- roc(fitLogit)
#' confint(r)                    # CI at best cut-point
#' confint(r, x = 0.5)          # CI at specificity = 0.5
#' 
#' @seealso \code{\link[pROC]{ci.coords}}, \code{\link{roc}}


#' @export
confint.roc <- function(object, parm, level = 0.95, x = "best", ...) {
  pROC::ci.coords(roc = object, x = x, conf.level = level, ...)
}
