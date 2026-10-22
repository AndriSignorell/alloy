
#' Confusion Matrix of a Classification Model fitted with fitMod()
#'
#' Method of [DescToolsX::confusion()] for models fitted with [fitMod()].
#' Predicted classes come from [predict.FitMod()], so the method works
#' alike for every classification engine (logistic and Firth logistic
#' regression, multinomial models, trees, random forests, ...), on the
#' training data or on new data.
#'
#' @param x a classification model fitted with [fitMod()]
#' @param newdata an optional data frame to classify, containing the
#'   response. `NULL` (default) uses the training data.
#' @param cutoff the probability of the positive class above which an
#'   observation is classified as positive, for binary models. Default 0.5.
#' @param pos the positive class of a binary model, by default the second
#'   level of the response
#' @param ... further arguments passed to [DescToolsX::confusion()], e.g.
#'   `conf.level` or `na.rm`
#'
#' @details
#' For a binary model the observations are classified by comparing the
#' predicted probability of the positive class with `cutoff`, as in the
#' `glm` method of [DescToolsX::confusion()]; for more than two classes
#' the class with the highest probability is taken. The class labels are
#' those of the probability columns of [predict.FitMod()], so a 0/1 coded
#' response gives the classes `"0"` and `"1"`.
#'
#' @return An object of class `"Confusion"`, see [DescToolsX::confusion()].
#'
#' @examples
#' fit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")
#' confusion(fit)
#' confusion(fit, cutoff = 0.3)
#'
#' # out of sample
#' set.seed(1)
#' d <- splitTrainTest(Admit, p = 0.3)
#' confusion(update(fit, data = d$train), newdata = d$test)
#'
#' @family modelling
#' @concept classification
#' @concept model-evaluation
#' @exportS3Method DescToolsX::confusion
confusion.FitMod <- function(x, newdata = NULL, cutoff = 0.5, pos = NULL,
                             ...) {

  inner <- if (inherits(x, c("FitMod.lme4", "FitMod.xgboost"))) x$model
           else x
  if (!.is_classification(x, inner, x$engine))
    stop("confusion() needs a classification model, not engine = '",
         x$engine, "'.")

  prob <- predict(x, newdata = newdata, output = "prob")
  lev <- colnames(prob)

  pred <- if (length(lev) == 2L)
    lev[(prob[[2L]] > cutoff) + 1L]
  else
    lev[max.col(as.matrix(prob), ties.method = "first")]

  obs <- if (is.null(newdata)) {
    response(x)
  } else {
    fo <- stats::formula(x)
    eval(fo[[2L]], newdata,
         if (is.null(environment(fo))) parent.frame() else environment(fo))
  }

  ref <- factor(as.character(obs), levels = lev)
  if (sum(is.na(ref)) > sum(is.na(obs)))
    stop("the response has values not among the predicted classes ",
         paste(sQuote(lev), collapse = ", "), ".")

  if (length(lev) == 2L && is.null(pos))
    pos <- lev[2L]

  confusion(factor(pred, levels = lev), ref, pos = pos, ...)
}
