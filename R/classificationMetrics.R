
#' Classification Metrics of Fitted Models
#'
#' Methods of the classification metrics of \pkg{DescToolsX} for fitted
#' models: the confusion matrix ([DescToolsX::confusion()]), the
#' C-statistic ([DescToolsX::cStat()]), the Brier score
#' ([DescToolsX::brierScore()]), the average precision
#' ([DescToolsX::averagePrecision()]) and the log loss
#' ([DescToolsX::logLoss()]). The metrics themselves work on predictions
#' and observed outcomes; the methods here take both from a model.
#'
#' @param x a classification model fitted with [fitMod()], or a model
#'   object of one of the classes listed under Details
#' @param newdata an optional data frame to evaluate, containing the
#'   response. `NULL` (default) uses the training data. For models fitted
#'   with [fitMod()] only.
#' @param cutoff the probability of the positive class above which an
#'   observation is classified as positive, for binary models. Default 0.5.
#' @param pos the positive class of a binary model, by default the second
#'   level of the response
#' @param ... further arguments passed to the metric, e.g. `conf.level`,
#'   `sides`, `scaled` or `eps`
#'
#' @details
#' **Models fitted with [fitMod()].** Predictions come from
#' [predict.FitMod()], so the methods work alike for every classification
#' engine (logistic and Firth logistic regression, multinomial models,
#' trees, random forests, ...), on the training data or on `newdata`.
#' The observed classes are aligned with the probability columns by their
#' labels, so a 0/1 coded response gives the classes `"0"` and `"1"`, and
#' the second class is the positive one. A binary model is classified by
#' comparing the probability of the positive class with `cutoff`, a model
#' with more classes by the class of highest probability. The C-statistic,
#' the Brier score, the average precision and the log loss need a binary
#' model.
#'
#' **Other model objects.** `confusion()` has methods for `glm`,
#' `multinom` (\pkg{nnet}), `rpart`, `randomForest` (out-of-bag
#' predictions), `svm` (\pkg{e1071}), `lda` and `qda` (\pkg{MASS});
#' `cStat()`, `brierScore()`, `averagePrecision()` and `logLoss()` have
#' methods for binomial `glm` objects. These use the fitted values and the
#' response stored in the model. `logLoss()` additionally requires
#' ungrouped data (all prior weights 1) and refuses `quasibinomial`, which
#' has no full likelihood.
#'
#' Measures on the training data are optimistic; [crossVal()] reports
#' their cross-validated values.
#'
#' @return As the respective metric in \pkg{DescToolsX}.
#'
#' @examples
#' fit <- fitMod(admit ~ gre + gpa + rank, Admit, engine = "logit")
#'
#' confusion(fit)
#' confusion(fit, cutoff = 0.3)
#' cStat(fit)
#' brierScore(fit, scaled = TRUE)
#' averagePrecision(fit)
#' logLoss(fit)
#'
#' # out of sample
#' set.seed(1)
#' d <- splitTrainTest(Admit, p = 0.3)
#' m <- update(fit, data = d$train)
#' confusion(m, newdata = d$test)
#' cStat(m, newdata = d$test, conf.level = 0.95)
#'
#' # a plain glm
#' g <- glm(am ~ hp + wt, data = mtcars, family = binomial)
#' confusion(g)
#' logLoss(g)
#'
#' @seealso [crossVal()] for cross-validated metrics, [regression-metrics]
#' @name classification-metrics
#' @family modelling
#' @concept classification
#' @concept model-evaluation
NULL


# == models fitted with fitMod() =============================================


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::confusion
confusion.FitMod <- function(x, newdata = NULL, cutoff = 0.5, pos = NULL,
                             ...) {

  po <- .predObs(x, newdata, "confusion")
  lev <- colnames(po$prob)

  pred <- if (length(lev) == 2L)
    lev[(po$prob[[2L]] > cutoff) + 1L]
  else
    lev[max.col(as.matrix(po$prob), ties.method = "first")]

  if (length(lev) == 2L && is.null(pos))
    pos <- lev[2L]

  confusion(factor(pred, levels = lev), po$ref, pos = pos, ...)
}


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::cStat
cStat.FitMod <- function(x, newdata = NULL, ...) {
  b <- .binaryPredObs(x, newdata, "cStat")
  cStat(b$p, ref = b$y, ...)
}


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::brierScore
brierScore.FitMod <- function(x, newdata = NULL, ...) {
  b <- .binaryPredObs(x, newdata, "brierScore")
  brierScore(b$p, ref = b$y, ...)
}


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::averagePrecision
averagePrecision.FitMod <- function(x, newdata = NULL, ...) {
  b <- .binaryPredObs(x, newdata, "averagePrecision")
  averagePrecision(b$p, ref = b$y, ...)
}


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::logLoss
logLoss.FitMod <- function(x, newdata = NULL, ...) {
  b <- .binaryPredObs(x, newdata, "logLoss")
  logLoss(b$p, ref = b$y, ...)
}


# == binomial glm ==============================================================


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::confusion
confusion.glm <- function(x, cutoff = 0.5, pos = NULL, ...) {

  if (is.null(x$model))
    stop("'x' does not contain the model frame - refit with model = TRUE")

  resp <- stats::model.extract(x$model, "response")
  lvl  <- if (is.factor(resp)) levels(resp) else levels(factor(resp))

  if (length(lvl) != 2L)
    stop("confusion.glm requires a binary response - use confusion.multinom() for multiclass")

  prob <- predict(x, type = "response")
  pred <- lvl[(prob > cutoff) + 1L]

  if (is.null(pos)) pos <- lvl[2L]

  confusion(x = pred, ref = resp, pos = pos, ...)
}


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::cStat
cStat.glm <- function(x, ...) {
  b <- .glmPredObs(x, "cStat")
  cStat(b$p, ref = b$y, ...)
}


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::brierScore
brierScore.glm <- function(x, ...) {
  b <- .glmPredObs(x, "brierScore")
  brierScore(b$p, ref = b$y, ...)
}


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::averagePrecision
averagePrecision.glm <- function(x, ...) {
  b <- .glmPredObs(x, "averagePrecision")
  averagePrecision(b$p, ref = b$y, ...)
}


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::logLoss
logLoss.glm <- function(x, ...) {

  if (!identical(x$family$family, "binomial"))
    stop("logLoss() requires a model with family \"binomial\", got \"",
         x$family$family, "\".", call. = FALSE)

  if (any(x$prior.weights != 1))
    stop("logLoss() is defined for ungrouped Bernoulli data, but the ",
         "model has non-unit prior weights.", call. = FALSE)

  b <- .glmPredObs(x, "logLoss")
  logLoss(b$p, ref = b$y, ...)
}


# == other model classes, confusion() only =====================================


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::confusion
confusion.multinom <- function(x, ...) {
  if (is.null(x$model))
    stop("'x' does not contain model frame - refit with model = TRUE")
  resp <- stats::model.extract(x$model, "response")
  pred <- predict(x, type = "class")
  confusion(x = pred, ref = resp, ...)
}


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::confusion
confusion.rpart <- function(x, ...) {
  lvl <- attr(x, "ylevels")
  confusion(x   = lvl[x$frame$yval[x$where]],
            ref = lvl[x$y], ...)
}


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::confusion
confusion.randomForest <- function(x, ...) {
  confusion(x = x$predicted, ref = x$y, ...)
}


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::confusion
confusion.svm <- function(x, ...) {
  # predict.svm() has no 'type' argument - it was silently swallowed by
  # its dots and had no effect
  confusion(x   = predict(x),
            ref = model.response(model.frame(x)), ...)
}


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::confusion
confusion.lda <- function(x, ...) {
  confusion(x   = predict(x)$class,
            ref = stats::model.extract(model.frame(x), "response"), ...)
}


#' @rdname classification-metrics
#' @exportS3Method DescToolsX::confusion
confusion.qda <- function(x, ...) confusion.lda(x, ...)



# == internal helper functions ============================================


# Class probabilities and observed classes of a classification model fitted
# with fitMod(), on the training data or on newdata; 'caller' names the
# function in the error message. The observed classes are a factor with
# the levels of the probability columns, so both are aligned whatever the
# response coding.
#' @keywords internal
.predObs <- function(x, newdata, caller) {

  inner <- if (inherits(x, c("FitMod.lme4", "FitMod.xgboost"))) x$model
           else x
  if (!.is_classification(x, inner, x$engine))
    stop(caller, "() needs a classification model, not engine = '",
         x$engine, "'.", call. = FALSE)

  prob <- predict(x, newdata = newdata, output = "prob")
  lev <- colnames(prob)

  obs <- if (is.null(newdata)) response(x) else .responseOf(x, newdata)

  ref <- factor(as.character(obs), levels = lev)
  if (sum(is.na(ref)) > sum(is.na(obs)))
    stop("the response has values not among the predicted classes ",
         paste(sQuote(lev), collapse = ", "), ".", call. = FALSE)

  list(prob = prob, ref = ref)
}


# The probability of the positive (second) class and the outcome as 0/1,
# for the metrics of binary models
#' @keywords internal
.binaryPredObs <- function(x, newdata, caller) {

  po <- .predObs(x, newdata, caller)

  if (ncol(po$prob) != 2L)
    stop(caller, "() needs a binary model, not one with ", ncol(po$prob),
         " classes.", call. = FALSE)

  list(p = po$prob[[2L]],
       y = as.integer(po$ref == colnames(po$prob)[2L]))
}


# Fitted probabilities and the 0/1 response of a binomial glm. Both are
# taken as stored, unpadded: predict() and fitted() pad them under
# na.action = na.exclude and lose the alignment with $y
#' @keywords internal
.glmPredObs <- function(x, caller) {

  if (!(x$family$family %in% c("binomial", "quasibinomial")))
    stop(caller, "() needs a binomial model, got family \"",
         x$family$family, "\".", call. = FALSE)

  list(p = unname(x$fitted.values),
       y = if (!is.null(x$y)) as.numeric(x$y) else .numericResponse(x))
}


# The response of a model evaluated in new data
#' @keywords internal
.responseOf <- function(x, newdata) {
  fo <- stats::formula(x)
  eval(fo[[2L]], newdata,
       if (is.null(environment(fo))) parent.frame() else environment(fo))
}
