
#' Prediction Errors of Fitted Regression Models
#'
#' Methods of the prediction error measures of \pkg{DescToolsX} for fitted
#' models: the mean absolute error ([DescToolsX::mae()]), the mean squared
#' error ([DescToolsX::mse()]) and its root ([DescToolsX::rmse()]), and
#' the mean absolute percentage error, plain ([DescToolsX::mape()]) and
#' symmetric ([DescToolsX::smape()]). The measures themselves work on
#' predictions and observed values; the methods here take both from a
#' model.
#'
#' @param x a regression model fitted with [fitMod()], or an `lm` (incl.
#'   `glm`) object
#' @param newdata an optional data frame to evaluate, containing the
#'   response. `NULL` (default) uses the training data. For models fitted
#'   with [fitMod()] only.
#' @param ... further arguments passed to the measure, e.g. `na.rm`
#'
#' @details
#' For a model fitted with [fitMod()] the predictions come from
#' [predict.FitMod()] on the response scale, so the methods work alike for
#' every regression engine (linear, generalised linear, robust, trees,
#' random forests, ...), on the training data or on `newdata`.
#'
#' For an `lm` or `glm` object the fitted values on the response scale
#' and the stored response are used. The response must be a numeric
#' vector: a logistic `glm` with a factor or two-column response is
#' refused.
#'
#' Measures on the training data are optimistic; [crossVal()] reports the
#' cross-validated root mean squared and mean absolute error.
#'
#' @return As the respective measure in \pkg{DescToolsX}.
#'
#' @examples
#' fit <- fitMod(mpg ~ hp + wt, mtcars, engine = "lm")
#' mae(fit)
#' rmse(fit)
#' mape(fit)
#'
#' # a plain lm
#' rmse(lm(mpg ~ hp + wt, mtcars))
#'
#' # out of sample
#' set.seed(1)
#' d <- splitTrainTest(mtcars, p = 0.3)
#' rmse(update(fit, data = d$train), newdata = d$test)
#'
#' @seealso [crossVal()] for cross-validated measures,
#'   [classification-metrics]
#' @name regression-metrics
#' @family modelling
#' @concept regression
#' @concept model-evaluation
NULL


#' @rdname regression-metrics
#' @exportS3Method DescToolsX::mae
mae.FitMod <- function(x, newdata = NULL, ...)
  .regMetric(x, newdata, DescToolsX::mae, "mae", ...)

#' @rdname regression-metrics
#' @exportS3Method DescToolsX::mse
mse.FitMod <- function(x, newdata = NULL, ...)
  .regMetric(x, newdata, DescToolsX::mse, "mse", ...)

#' @rdname regression-metrics
#' @exportS3Method DescToolsX::rmse
rmse.FitMod <- function(x, newdata = NULL, ...)
  .regMetric(x, newdata, DescToolsX::rmse, "rmse", ...)

#' @rdname regression-metrics
#' @exportS3Method DescToolsX::mape
mape.FitMod <- function(x, newdata = NULL, ...)
  .regMetric(x, newdata, DescToolsX::mape, "mape", ...)

#' @rdname regression-metrics
#' @exportS3Method DescToolsX::smape
smape.FitMod <- function(x, newdata = NULL, ...)
  .regMetric(x, newdata, DescToolsX::smape, "smape", ...)


#' @rdname regression-metrics
#' @exportS3Method DescToolsX::mae
mae.lm <- function(x, ...) .lmMetric(x, DescToolsX::mae, "mae", ...)

#' @rdname regression-metrics
#' @exportS3Method DescToolsX::mse
mse.lm <- function(x, ...) .lmMetric(x, DescToolsX::mse, "mse", ...)

#' @rdname regression-metrics
#' @exportS3Method DescToolsX::rmse
rmse.lm <- function(x, ...) .lmMetric(x, DescToolsX::rmse, "rmse", ...)

#' @rdname regression-metrics
#' @exportS3Method DescToolsX::mape
mape.lm <- function(x, ...) .lmMetric(x, DescToolsX::mape, "mape", ...)

#' @rdname regression-metrics
#' @exportS3Method DescToolsX::smape
smape.lm <- function(x, ...) .lmMetric(x, DescToolsX::smape, "smape", ...)



# == internal helper functions ============================================


# A measure fun(pred, ref) for a regression model fitted with fitMod(),
# on the training data or on newdata
#' @keywords internal
.regMetric <- function(x, newdata, fun, caller, ...) {

  inner <- if (inherits(x, c("FitMod.lme4", "FitMod.xgboost"))) x$model
           else x
  if (.is_classification(x, inner, x$engine))
    stop(caller, "() needs a regression model, not engine = '",
         x$engine, "'.", call. = FALSE)

  pred <- as.numeric(predict(x, newdata = newdata))
  obs  <- if (is.null(newdata)) response(x) else .responseOf(x, newdata)

  if (!is.numeric(obs) || !is.null(dim(obs)))
    stop("the model response must be a numeric vector to compute ",
         caller, "()", call. = FALSE)

  fun(pred, ref = as.numeric(obs), ...)
}


# A measure fun(pred, ref) for an lm or glm object: fitted values on the
# response scale and the stored response, both unpadded (predict() pads
# under na.action = na.exclude). glm objects inherit from lm, so a
# logistic fit arrives here as well, with a factor or matrix response.
#' @keywords internal
.lmMetric <- function(x, fun, caller, ...) {

  ref <- stats::model.response(stats::model.frame(x))

  if (!is.numeric(ref) || !is.null(dim(ref)))
    stop("the model response must be a numeric vector to compute ",
         caller, "()", call. = FALSE)

  fun(unname(x$fitted.values), ref = unname(ref), ...)
}
