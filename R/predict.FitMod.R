
#' Predict method for FitMod objects
#'
#' Unified predict interface for all models fitted via \code{\link{FitMod}}.
#' For regression models the predicted values are returned as a numeric
#' vector.  For classification models either class probabilities, predicted
#' classes, or both are returned as a \code{data.frame}.
#'
#' @param object A fitted model of class \code{"FitMod"}.
#' @param newdata Optional data frame of new observations.  If omitted,
#'   fitted values on the training data are returned.
#' @param output Character string controlling the output for classification
#'   models.  One of \code{"prob"} (default), \code{"class"}, or
#'   \code{"both"}.  Ignored for regression models.
#' @param ... Further arguments passed to the underlying predict method.
#'
#' @return For regression models: a numeric vector of predicted values.
#'   For classification models: a \code{data.frame} with one column per
#'   class (if \code{output = "prob"}), a single column \code{class} (if
#'   \code{output = "class"}), or both combined (if \code{output = "both"}).
#'
#' @export
predict.FitMod <- function(object, newdata = NULL,
                           output = c("prob", "class", "both"),
                           s = "lambda.1se",
                           type = NULL,
                           ...) {

  output  <- match.arg(output)
  fitfn   <- object$fitfn
  is_reg  <- fitfn %in% c("lm", "lmrob", "tobit", "poisson",
                           "quasipoisson", "gamma", "negbin", "zeroinfl")

  obj <- object
  class(obj) <- class(obj)[class(obj) != "FitMod"]

  # --- regression: pass type through if explicitly given ---
  if (is_reg) {
    if (is.null(newdata) && is.null(type))
      return(fitted(obj))
    args <- list(obj)
    if (!is.null(newdata))  args$newdata <- newdata
    if (!is.null(type))     args$type    <- type
    return(do.call(predict, c(args, list(...))))
  }

  # --- classification: type is handled internally, not passed through ---
  .pred_prob  <- function() .predict_prob(obj, fitfn, newdata, s = s, ...)
  .pred_class <- function() .predict_class(obj, fitfn, newdata, s = s, ...)

  switch(output,
    prob  = .pred_prob(),
    class = data.frame(class = .pred_class()),
    both  = data.frame(.pred_prob(), class = .pred_class())
  )
}

# -------------------------------------------------------------------------
# Internal: extract probability matrix, always as data.frame
# -------------------------------------------------------------------------

#' @keywords internal
.predict_prob <- function(object, fitfn, newdata, ...) {
  
  # Default args — overridden below for models requiring explicit newdata
  args <- if (is.null(newdata)) list(object)
             else list(object, newdata = newdata)
  
  # Lazy evaluation — only computed for models that need it
  args_explicit <- function()
    list(object, newdata = .resolve_newdata(object, newdata))
  
  mat <- switch(fitfn,
                
                logit = ,
                glm   = {
                  p <- do.call(predict, c(args, list(type = "response", ...)))
                  cbind("0" = 1 - p, "1" = p)
                },
                
                multinom = {
                  do.call(predict, c(args, list(type = "probs", ...)))
                },
                
                polr = {
                  do.call(predict, c(args, list(type = "probs", ...)))
                },
                
                rpart = {
                  do.call(predict, c(args, list(type = "prob", ...)))
                },
                
                lda = ,
                qda = {
                  do.call(predict, c(args, ...))$posterior
                },
                
                svm = {
                  p <- do.call(predict, c(args_explicit(), list(probability = TRUE, ...)))
                  attr(p, "probabilities")
                },
                
                nnet = {
                  p <- do.call(predict, c(args, list(type = "raw", ...)))
                  if (is.vector(p) || ncol(p) == 1L) {
                    p <- as.numeric(p)
                    cbind("0" = 1 - p, "1" = p)
                  } else {
                    p
                  }
                },
                
                naiveBayes = {
                  do.call(predict, c(args, list(type = "prob", ...)))
                },
                
                C5.0 = {
                  do.call(predict, c(args_explicit(), list(type = "prob", ...)))
                },
                
                randomForest = {
                  do.call(predict, c(args_explicit(), list(type = "prob", ...)))
                },
                
                lb = {
                  do.call(predict, c(args_explicit(), list(type = "raw", ...)))
                },
                
                
                glmnet = {
                  nd <- if (is.null(newdata)) {
                    object$x_train
                  } else {
                    model.matrix(object$formula[-2L], data = newdata)[, -1L, drop = FALSE]
                  }
                  p <- predict(object, newx = nd, s = "lambda.1se", type = "response")
                  if (length(dim(p)) == 3L)
                    p <- p[, , 1L]
                  as.data.frame(p)
                },
                
                xgboost = {
                  obj <- if (inherits(object, "FitMod.xgboost")) object$model else object
                  nd  <- if (is.null(newdata)) object$x_train
                  else model.matrix(object$formula[-2L], data = newdata)[, -1L, drop = FALSE]
                  p   <- predict(obj, nd)
                  lvl <- object$y_levels
                  
                  # New xgboost API returns matrix directly for multi:softprob
                  if (is.matrix(p)) {
                    colnames(p) <- lvl
                  } else if (!is.null(lvl)) {
                    # Fallback for old flat vector format
                    p <- matrix(p, ncol = length(lvl), byrow = TRUE)
                    colnames(p) <- lvl
                  } else {
                    p <- cbind("0" = 1 - p, "1" = p)
                  }
                  as.data.frame(p)
                },                
                
                stop(sprintf("No probability prediction implemented for fitfn = '%s'", fitfn))
  )
  
  mat <- as.data.frame(mat)
  
  # Preserve original level names — only fix truly invalid characters
  # (spaces, slashes etc.) but keep numeric-looking names as-is
  colnames(mat) <- gsub("[^a-zA-Z0-9._|]", ".", colnames(mat))
  
  mat <- .normalise_prob_cols(mat, object)
  mat
  
}


# -------------------------------------------------------------------------
# Internal: extract predicted class, always as factor
# -------------------------------------------------------------------------

#' @keywords internal
.predict_class <- function(object, fitfn, newdata, ...) {
  
  args          <- if (is.null(newdata)) list(object)
  else                  list(object, newdata = newdata)
  
  # Lazy evaluation — only computed for models that need it
  args_explicit <- function()
    list(object, newdata = .resolve_newdata(object, newdata))
  
  cls <- switch(fitfn,
                
                logit = ,
                glm   = {
                  p   <- do.call(predict, c(args, list(type = "response", ...)))
                  lvl <- levels(response(object))
                  factor(ifelse(p > 0.5, lvl[2L], lvl[1L]), levels = lvl)
                },
                
                multinom = ,
                polr     = {
                  do.call(predict, c(args, list(type = "class", ...)))
                },
                
                rpart = {
                  do.call(predict, c(args, list(type = "class", ...)))
                },
                
                lda = ,
                qda = {
                  do.call(predict, c(args, ...))$class
                },
                
                svm = {
                  do.call(predict, c(args_explicit(), ...))
                },
                
                C5.0 = ,
                randomForest = ,
                lb = {
                  do.call(predict, c(args_explicit(), list(type = "class", ...)))
                },
                
                nnet = {
                  p <- do.call(predict, c(args, list(type = "raw", ...)))
                  if (is.vector(p) || ncol(p) == 1L) {
                    lvl <- levels(response(object))
                    factor(ifelse(as.numeric(p) > 0.5, lvl[2L], lvl[1L]), levels = lvl)
                  } else {
                    factor(colnames(p)[max.col(p)])
                  }
                },
                
                naiveBayes = {
                  do.call(predict, c(args, list(type = "class", ...)))
                },
                
                glmnet = {
                  nd <- if (is.null(newdata)) {
                    object$x_train
                  } else {
                    model.matrix(object$formula[-2L], data = newdata)[, -1L, drop = FALSE]
                  }
                  p <- predict(object, newx = nd, s = "lambda.1se", type = "class")
                  # predict.cv.glmnet returns a matrix — extract first column as character
                  as.factor(as.character(p[, 1L]))
                },
                
                xgboost = {
                  obj <- if (inherits(object, "FitMod.xgboost")) object$model else object
                  nd  <- if (is.null(newdata)) object$x_train
                  else model.matrix(object$formula[-2L], data = newdata)[, -1L, drop = FALSE]
                  p   <- predict(obj, nd)
                  lvl <- object$y_levels
                  
                  if (is.matrix(p)) {
                    cls <- lvl[max.col(p)]
                  } else if (!is.null(lvl)) {
                    p   <- matrix(p, ncol = length(lvl), byrow = TRUE)
                    cls <- lvl[max.col(p)]
                  } else {
                    cls <- ifelse(p > 0.5, "1", "0")
                  }
                  as.factor(cls)
                },

                stop(sprintf("No class prediction implemented for fitfn = '%s'", fitfn))
  )
  
  as.factor(cls)
}



# Ensure newdata is set — some packages (e1071, C50) require explicit
# newdata even for training data predictions
#' @keywords internal
.resolve_newdata <- function(object, newdata) {
  if (!is.null(newdata))
    return(newdata)
  
  # Try model.frame first (works for lm, glm, rpart, ...)
  nd <- tryCatch(
    model.frame(object),
    error = function(e) NULL
  )
  
  # Fallback for models without standard terms (C5.0, naive_bayes, ...)
  if (is.null(nd)) {
    if (is.null(object$call$data))
      stop("Cannot recover training data: no model.frame and no data= in call")
    nd <- eval(object$call$data, envir = environment(formula(object)))
  }
  
  # Drop response column
  resp_name <- tryCatch(
    attr(attr(object$terms, "dataClasses"), "names")[1L],
    error = function(e) {
      # Fallback: first variable in formula
      all.vars(formula(object))[1L]
    }
  )
  
  nd[, setdiff(names(nd), resp_name), drop = FALSE]
}


# Normalise column order to match factor levels of the response
.normalise_prob_cols <- function(mat, object) {
  lvl <- tryCatch({
    if (!is.null(object$glmnet.fit$classnames))
      object$glmnet.fit$classnames
    else if (!is.null(object$y_levels))
      object$y_levels
    else
      levels(response(object))
  }, error = function(e) NULL)
  
  if (is.null(lvl) || !all(lvl %in% names(mat)))
    return(mat)
  mat[, lvl, drop = FALSE]
}



# Helper: get optimal n.trees for gbm
.gbm_ntrees <- function(object) {
  tryCatch(
    gbm::gbm.perf(object, method = "cv", plot.it = FALSE),
    error = function(e) object$n.trees
  )
}

