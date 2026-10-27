#' ROC curve for a fitted model, a cross-validation or a predictor vector
#'
#' Convenience wrapper around \code{\link[pROC]{roc}} that accepts a
#' fitted \code{"FitMod"} binary classification model, the result of
#' [crossVal()] for such a model, or a numeric predictor vector directly.
#'
#' @param x Either a fitted binary classification model of class
#'   \code{"FitMod"} (in which case predicted probabilities and the response
#'   are extracted automatically), an object of class \code{"CrossVal"} from
#'   [crossVal()] (out-of-fold predictions), or a numeric vector of
#'   predicted probabilities / scores when \code{ref} is supplied.
#' @param ref Optional factor or binary vector of true class labels.  If
#'   \code{NULL} (default), \code{x} must be a \code{"FitMod"} or
#'   \code{"CrossVal"} object and the response is taken from it.
#' @param ... Further arguments passed to \code{\link[pROC]{roc}}; they
#'   override the defaults set here, e.g. \code{direction}.
#'
#' @return An object of class \code{"roc"} as returned by
#'   \code{\link[pROC]{roc}}.
#'
#' @details
#' For a \code{"FitMod"} object the predicted probability of the second
#' class (the second column of \code{predict(x)}) is the predictor. The
#' class order is known then, so \code{levels} is set to the classes and
#' \code{direction} to \code{"<"} (higher predictions for the second
#' class): \pkg{pROC} would otherwise choose the direction from the data
#' and turn a model worse than chance into one better than chance.
#'
#' For a \code{"CrossVal"} object the curve is drawn from the out-of-fold
#' predictions, each made by a model that had not seen the observation,
#' with the same \code{levels} and \code{direction}. With repeated
#' cross-validation each observation enters once, with the mean of its
#' out-of-fold predictions; the AUC of this curve can therefore differ
#' slightly from the pooled \code{auc} in the summary of [crossVal()],
#' which is computed per repetition and then averaged.
#'
#' For a predictor vector with \code{ref}, \pkg{pROC} determines levels
#' and direction itself unless they are given in \code{...}. For models
#' with non-standard probability output, supply the predictor vector
#' explicitly via \code{x} and \code{ref}.
#'
#' @examples
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")
#' r <- roc(fitLogit)
#' plot(r)
#'
#' # Supply predictor and response directly
#' p <- predict(fitLogit)[, 2]
#' r2 <- roc(p, ref = Admit$admit)
#'
#' # cross-validated ROC curve
#' set.seed(1)
#' rCV <- roc(crossVal(fitLogit, k = 10))
#' rCV$auc
#'
#' @seealso \code{\link[pROC]{roc}}, [rocTest()], \code{\link{bestCut}},
#'   \code{\link{response}}

#' @family roc  
#' @concept classification  
#' @concept model-evaluation  
#' @concept roc
#'
#'
#' @export
roc <- function(x, ref = NULL, ...) {

  .require_pkg("pROC")

  args <- if (inherits(x, "CrossVal")) {

    if (!is.null(ref))
      stop("'ref' cannot be given with a cross-validation; the response ",
           "is taken from 'x'.")
    oof <- .cvOutOfFold(x)
    list(predictor = oof$p, response = oof$y, levels = c(0, 1),
         direction = "<")

  } else if (is.null(ref)) {

    p <- stats::predict(x)
    if (NCOL(p) != 2L)
      stop("'x' must be a binary classification model.")
    y <- response(x)
    if (is.logical(y)) y <- as.integer(y)
    list(predictor = p[[2L]], response = y, levels = colnames(p),
         direction = "<")

  } else {
    list(predictor = x, response = ref)
  }

  dots <- list(...)
  args[names(dots)] <- dots

  do.call(pROC::roc, c(args, if (!("plot" %in% names(args))) list(plot = FALSE)))
}


#' Test for the Difference of Two ROC Curves
#'
#' Compares the areas under two ROC curves (or their partial areas,
#' sensitivities or specificities), with \code{\link[pROC]{roc.test}}.
#' The curves can be given as \code{"roc"} objects or as anything [roc()]
#' accepts without a reference: a fitted binary \code{"FitMod"} model or a
#' cross-validation from [crossVal()].
#'
#' @param roc1,roc2 the two ROC curves: objects of class \code{"roc"} (from
#'   [roc()] or \code{\link[pROC]{roc}}), binary \code{"FitMod"} models or
#'   \code{"CrossVal"} objects, which are passed to [roc()] first.
#' @param ... further arguments passed to \code{\link[pROC]{roc.test}},
#'   e.g. \code{method} (\code{"delong"}, \code{"bootstrap"},
#'   \code{"venkatraman"}, ...), \code{alternative} and \code{paired}.
#'
#' @details
#' Whether the curves are paired is decided by \pkg{pROC} from the data:
#' paired if both rest on the same observations with the same response, as
#' for two models fitted to, or cross-validated on, the same sample; unpaired
#' otherwise, e.g. for a development and an external validation sample. The
#' default DeLong test is available in both cases; set \code{paired}
#' explicitly where the automatic decision is not wanted.
#'
#' Two cross-validated curves are paired only if they cover the same
#' observations; a fold that failed in one of them breaks the pairing.
#'
#' The name follows the naming rules of the package suite; the function is
#' a thin wrapper, all statistics come from \pkg{pROC}.
#'
#' @return An object of class \code{"htest"} as returned by
#'   \code{\link[pROC]{roc.test}}, with \code{statistic},
#'   \code{p.value}, \code{estimate} and further components.
#'
#' @references
#' DeLong, E. R., DeLong, D. M. and Clarke-Pearson, D. L. (1988) Comparing
#' the areas under two or more correlated receiver operating characteristic
#' curves: a nonparametric approach. \emph{Biometrics}, \bold{44}(3),
#' 837--845.
#'
#' Robin, X. et al. (2011) pROC: an open-source package for R and S+ to
#' analyze and compare ROC curves. \emph{BMC Bioinformatics}, \bold{12},
#' 77.
#'
#' @examples
#' idx <- seq(1, nrow(Admit), by = 2)
#' dev <- Admit[idx, ]
#' ext <- Admit[-idx, ]
#'
#' fit <- fitMod(admit ~ gre + gpa + rank, dev, engine = "logit")
#'
#' # development against external validation: unpaired DeLong test
#' rDev <- roc(fit)
#' rExt <- roc(predict(fit, newdata = ext)[, 2], ref = ext$admit)
#' rocTest(rDev, rExt)
#'
#' # two models, cross-validated on the same sample: paired DeLong test
#' set.seed(1)
#' cvFull <- crossVal(fitMod(admit ~ gre + gpa + rank, Admit,
#'                           engine = "logit"))
#' set.seed(1)
#' cvSmall <- crossVal(fitMod(admit ~ gpa, Admit, engine = "logit"))
#' rocTest(cvFull, cvSmall)
#'
#' @seealso [roc()], \code{\link[pROC]{roc.test}}, [crossVal()]
#'
#' @family roc
#' @concept roc
#' @concept model-evaluation
#' @concept hypothesis-testing
#'
#' @export
rocTest <- function(roc1, roc2, ...) {

  .require_pkg("pROC")

  res <- pROC::roc.test(.asRoc(roc1, "roc1"), .asRoc(roc2, "roc2"), ...)
  res$data.names <- paste(deparse1(substitute(roc1)), "and",
                          deparse1(substitute(roc2)))
  res
}


# An ROC curve from anything roc() takes without a reference
#' @keywords internal
.asRoc <- function(x, argName) {
  if (inherits(x, "roc"))
    return(x)
  if (!inherits(x, c("FitMod", "CrossVal")))
    stop(gettextf("'%s' must be an ROC curve, a binary FitMod model or a cross-validation.",
                  argName), call. = FALSE, domain = NA)
  roc(x, quiet = TRUE)
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
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")
#' r <- roc(fitLogit)
#' confint(r)                    # CI at best cut-point
#' confint(r, x = 0.5)          # CI at specificity = 0.5
#' 
#' @seealso \code{\link[pROC]{ci.coords}}, \code{\link{roc}}


#' @export
confint.roc <- function(object, parm, level = 0.95, x = "best", ...) {
  pROC::ci.coords(roc = object, x = x, conf.level = level, ...)
}
