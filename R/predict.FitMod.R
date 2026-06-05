#' Predict method for FitMod objects
#'
#' Unified predict interface for all models fitted via \code{\link{fitMod}}.
#' For regression and survival models the predicted values are returned as a
#' numeric vector.  For classification models either class probabilities,
#' predicted classes, or both are returned as a \code{data.frame} with
#' consistent column names across all model types.
#'
#' @param object A fitted model of class \code{"FitMod"}.
#' @param newdata Optional data frame of new observations.  If omitted,
#'   fitted values on the training data are returned.
#' @param output Character string controlling the output for classification
#'   models.  One of \code{"prob"} (default), \code{"class"}, or
#'   \code{"both"}.  Ignored for regression and survival models.
#' @param s For \code{fitfn = "glmnet"} only: the value of the penalty
#'   parameter \eqn{\lambda} at which predictions are made.  Passed to
#'   \code{\link[glmnet]{predict.cv.glmnet}}.  Default is
#'   \code{"lambda.1se"}.
#' @param type For regression models (\code{lm}, \code{glm}, etc.): the
#'   \code{type} argument passed to the underlying \code{predict} method
#'   (e.g. \code{"response"}, \code{"link"}).  For Cox models the default
#'   is \code{"risk"}; for parametric survival models the default is
#'   \code{"response"}.  Ignored for classification models (use
#'   \code{output} instead).
#' @param ... Further arguments passed to the underlying predict method.
#'
#' @return
#' \describe{
#'   \item{Regression models (\code{lm}, \code{lmrob}, \code{poisson},
#'     \code{quasipoisson}, \code{gamma}, \code{negbin}, \code{zeroinfl},
#'     \code{tobit})}{A named numeric vector of fitted/predicted values.}
#'   \item{Survival models (\code{coxph})}{A numeric vector of predicted
#'     risk scores (\code{type = "risk"} by default).}
#'   \item{Parametric survival models (\code{weibull}, \code{exponential},
#'     \code{lognormal}, \code{loglogistic})}{A numeric vector of predicted
#'     survival times (\code{type = "response"} by default).}
#'   \item{Classification models}{
#'     \describe{
#'       \item{\code{output = "prob"}}{A \code{data.frame} with one column
#'         per class containing predicted probabilities.  Column names match
#'         the factor levels of the response variable.}
#'       \item{\code{output = "class"}}{A \code{data.frame} with a single
#'         column \code{class} (factor) containing the predicted class.}
#'       \item{\code{output = "both"}}{The probability columns and the
#'         \code{class} column combined in one \code{data.frame}.}
#'     }
#'   }
#' }
#'
#' @details
#' For classification models the column order of probability outputs is
#' always aligned with the factor levels of the response variable,
#' regardless of which model type is used.  This ensures that
#' \code{predict(fitLogit)} and \code{predict(fitRf)} return columns in
#' the same order.
#'
#' Models that require explicit \code{newdata} even for training-data
#' predictions (e.g. \code{svm}, \code{C5.0}, \code{randomForest}) are
#' handled transparently via an internal helper.
#'
#' For \code{fitfn = "logit"}, calling \code{predict(object)} returns a
#' two-column probability \code{data.frame} (like all other classifiers).
#' To obtain the linear predictor (log-odds), use
#' \code{predict(object, type = "link")}.
#'
#' @examples
#' # Regression
#' fitLm <- fitMod(Fertility ~ ., swiss)
#' head(predict(fitLm))
#'
#' # Binary classification - probabilities
#' fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#' head(predict(fitLogit))
#' head(predict(fitLogit, output = "both"))
#'
#' # Multinomial classification
#' fitMult <- fitMod(ice_cream ~ video + puzzle + female,
#'                   IceCream, fitfn = "multinom")
#' head(predict(fitMult, output = "both"))
#'
#' # Cox model - risk scores
#' fitCox <- fitMod(Surv(foltime, folstatus) ~ gender, Whas100, fitfn = "coxph")
#' head(predict(fitCox))
#'
#' # Parametric survival - expected survival time
#' fitWei <- fitMod(Surv(foltime, folstatus) ~ gender + age, Whas100,
#'                  fitfn = "weibull")
#' head(predict(fitWei))
#'
#' @seealso \code{\link{fitMod}}, \code{\link{print.FitMod}}

#' @export
predict.FitMod <- function(object, newdata = NULL,
                           output = c("prob", "class", "both", "where", "leaf"),
                           s = "lambda.1se",
                           type = NULL,
                           ...) {
  output <- match.arg(output)
  fitfn  <- object$fitfn
  is_reg <- fitfn %in% c("lm", "lmrob", "tobit", "poisson",
                         "quasipoisson", "gamma", "negbin", "zeroinfl",
                         "lmMixed", "poissonMixed", "negbinMixed", "gammaMixed")
  
  # Strip FitMod class to avoid infinite recursion
  obj <- object
  class(obj) <- class(obj)[class(obj) != "FitMod"]
  
  # Unwrap S4/special wrappers
  if (inherits(obj, "FitMod.lme4") || inherits(obj, "FitMod.xgboost"))
    obj <- obj$model
  
  # --- cox: risk scores by default, type overrideable ---
  if (fitfn == "coxph") {
    args <- list(obj, type = if (!is.null(type)) type else "risk")
    if (!is.null(newdata)) args$newdata <- newdata
    return(do.call(predict, c(args, list(...))))
  }
  
  # --- parametric survival ---
  is_survreg <- fitfn %in% c("weibull", "exponential", "lognormal", "loglogistic")
  if (is_survreg) {
    args <- list(obj, type = if (!is.null(type)) type else "response")
    if (!is.null(newdata)) args$newdata <- newdata
    return(do.call(predict, c(args, list(...))))
  }
  
  # --- rpart: leaf/where output ---
  if (fitfn == "rpart" && output %in% c("where", "leaf")) {
    if (is.null(newdata))
      return(if (output == "where") obj$where
             else rownames(obj$frame)[obj$where])
    else
      return(.predict.leaves(obj, newdata = newdata, type = output))
  }
  
  # --- regression (including lme4 regression models) ---
  if (is_reg) {
    if (is.null(newdata) && is.null(type))
      return(fitted(obj))
    args <- list(obj)
    if (!is.null(newdata)) args$newdata <- newdata
    if (!is.null(type))    args$type    <- type
    return(do.call(predict, c(args, list(...))))
  }
  
  # --- logit special case: type = "link" returns raw vector ---
  if (fitfn == "logit" && !is.null(type) && type == "link") {
    args <- list(obj, type = "link")
    if (!is.null(newdata)) args$newdata <- newdata
    return(do.call(predict, c(args, list(...))))
  }
  
  # --- classification ---
  if (!is.null(type))
    warning("'type' is ignored for classification models in predict.FitMod; ",
            "use 'output' to control the return format.", call. = FALSE)
  

  # für xgboost das wrapper-objekt behalten:
  xgb_obj <- if (fitfn == "xgboost") object else obj
  
  .pred_prob  <- function() .predict_prob(xgb_obj, fitfn, newdata, s = s, ...)
  .pred_class <- function() .predict_class(xgb_obj, fitfn, newdata, s = s, ...)

  switch(output,
         prob  = {
           result <- .pred_prob()
           if (!is.data.frame(result)) as.data.frame(result) else result
         },
         class = data.frame(class = .pred_class(), check.names = FALSE),
         both  = data.frame(.pred_prob(), class = .pred_class(), check.names = FALSE)
  )
}



# -------------------------------------------------------------------------
# Internal: extract probability matrix, always as data.frame
# -------------------------------------------------------------------------

#' @keywords internal
.predict_prob <- function(object, fitfn, newdata, ...) {
  
  # Remove arguments handled internally to avoid conflicts
  dots <- list(...)
  dots[["type"]] <- NULL
  dots[["s"]]    <- NULL
  
  # Default args - overridden below for models requiring explicit newdata
  args <- if (is.null(newdata)) list(object)
  else                  list(object, newdata = newdata)
  
  # Lazy evaluation - only computed for models that need it
  args_explicit <- function()
    list(object, newdata = .resolve_newdata(object, newdata))
  
  mat <- switch(fitfn,
                
                logit = ,
                glm   = {
                  p <- do.call(predict, c(args, list(type = "response"), dots))
                  cbind("0" = 1 - p, "1" = p)
                },
                
                multinom = {
                  do.call(predict, c(args, list(type = "probs"), dots))
                },
                
                polr = {
                  do.call(predict, c(args, list(type = "probs"), dots))
                },
                
                rpart = {
                  do.call(predict, c(args, list(type = "prob"), dots))
                },
                
                lda = ,
                qda = {
                  do.call(predict, c(args, dots))$posterior
                },
                
                svm = {
                  p <- do.call(predict, c(args_explicit(), list(probability = TRUE), dots))
                  attr(p, "probabilities")
                },
                
                nnet = {
                  p <- do.call(predict, c(args, list(type = "raw"), dots))
                  if (is.vector(p) || ncol(p) == 1L) {
                    p <- as.numeric(p)
                    cbind("0" = 1 - p, "1" = p)
                  } else {
                    p
                  }
                },
                
                naiveBayes = {
                  do.call(predict, c(args, list(type = "prob"), dots))
                },
                
                C5.0 = {
                  do.call(predict, c(args_explicit(), list(type = "prob"), dots))
                },
                
                randomForest = {
                  do.call(predict, c(args_explicit(), list(type = "prob"), dots))
                },
                
                lb = {
                  do.call(predict, c(args_explicit(), list(type = "raw"), dots))
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
                  nd  <- if (is.null(newdata)) {
                    if (is.null(object$x_train))
                      stop("x_train is NULL - the model may not have been fitted via fitMod()")
                    object$x_train
                  } else {
                    model.matrix(object$formula[-2L], data = newdata)[, -1L, drop = FALSE]
                  }
                  
                  p   <- predict(obj, nd)
                  lvl <- object$y_levels
                  if (is.matrix(p)) {
                    colnames(p) <- lvl
                  } else if (!is.null(lvl)) {
                    p <- matrix(p, ncol = length(lvl), byrow = TRUE)
                    colnames(p) <- lvl
                  } else {
                    p <- cbind("0" = 1 - p, "1" = p)
                  }
                  as.data.frame(p)
                },
                
                logitMixed = {
                  p <- do.call(predict, c(args, list(type = "response"), dots))
                  cbind("0" = 1 - p, "1" = p)
                },
                
                stop(sprintf("No probability prediction implemented for fitfn = '%s'", fitfn))
  )
  
  mat <- as.data.frame(mat)
  colnames(mat) <- gsub("[^a-zA-Z0-9._|]", ".", colnames(mat))
  mat <- .normalise_prob_cols(mat, object)
  mat
}



# -------------------------------------------------------------------------
# Internal: extract predicted class, always as factor
# -------------------------------------------------------------------------

#' @keywords internal
.predict_class <- function(object, fitfn, newdata, ...) {
  
  # Remove arguments handled internally to avoid conflicts
  dots <- list(...)
  dots[["type"]] <- NULL
  dots[["s"]]    <- NULL
  
  args <- if (is.null(newdata)) list(object)
  else                  list(object, newdata = newdata)
  
  # Lazy evaluation - only computed for models that need it
  args_explicit <- function()
    list(object, newdata = .resolve_newdata(object, newdata))
  
  cls <- switch(fitfn,
                
                logit = ,
                glm   = {
                  p   <- do.call(predict, c(args, list(type = "response"), dots))
                  # Use model response levels directly from the fitted values
                  lvl <- levels(model.response(model.frame(object)))
                  if (is.null(lvl))
                    lvl <- c("0", "1")
                  factor(ifelse(p > 0.5, lvl[2L], lvl[1L]), levels = lvl)
                },
                
                multinom = ,
                polr     = {
                  do.call(predict, c(args, list(type = "class"), dots))
                },
                
                rpart = {
                  do.call(predict, c(args, list(type = "class"), dots))
                },
                
                lda = ,
                qda = {
                  do.call(predict, c(args, dots))$class
                },
                
                svm = {
                  do.call(predict, c(args_explicit(), dots))
                },
                
                C5.0 = ,
                randomForest = ,
                lb = {
                  do.call(predict, c(args_explicit(), list(type = "class"), dots))
                },
                
                nnet = {
                  p <- do.call(predict, c(args, list(type = "raw"), dots))
                  if (is.vector(p) || ncol(p) == 1L) {
                    lvl <- levels(response(object))
                    factor(ifelse(as.numeric(p) > 0.5, lvl[2L], lvl[1L]), levels = lvl)
                  } else {
                    factor(colnames(p)[max.col(p)])
                  }
                },
                
                naiveBayes = {
                  do.call(predict, c(args, list(type = "class"), dots))
                },
                
                glmnet = {
                  nd <- if (is.null(newdata)) {
                    object$x_train
                  } else {
                    model.matrix(object$formula[-2L], data = newdata)[, -1L, drop = FALSE]
                  }
                  p <- predict(object, newx = nd, s = "lambda.1se", type = "class")
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
                
                logitMixed = {
                  p <- do.call(predict, c(args, list(type = "response"), dots))
                  factor(ifelse(p > 0.5, "1", "0"), levels = c("0", "1"))
                },
                
                stop(sprintf("No class prediction implemented for fitfn = '%s'", fitfn))
  )
  
  if (is.factor(cls)) cls else as.factor(cls)
}


# -------------------------------------------------------------------------
# Internal: extract predicted leaf for an rpart
# -------------------------------------------------------------------------


#' @keywords internal
.predict.leaves <- function(rp, newdata, type = "where") {
  
  if (type == "where") {
    rp$frame$yval    <- seq_len(nrow(rp$frame))
    should.be.leaves <- which(rp$frame[, 1L] == "<leaf>")
  } else if (type == "leaf") {
    rp$frame$yval    <- rownames(rp$frame)
    should.be.leaves <- rownames(rp$frame)[rp$frame[, 1L] == "<leaf>"]
  } else {
    stop("type must be 'where' or 'leaf'")
  }
  
  leaves           <- predict(rp, newdata = newdata, type = "vector")
  should.be.leaves <- which(rp$frame[, 1L] == "<leaf>")
  bad.leaves       <- leaves[!leaves %in% should.be.leaves]
  
  if (length(bad.leaves) == 0L)
    return(leaves)
  
  u.bad.leaves <- unique(bad.leaves)
  u.bad.nodes  <- rownames(rp$frame)[u.bad.leaves]
  all.nodes    <- rownames(rp$frame)[rp$frame[, 1L] == "<leaf>"]
  
  # Find nearest leaf ancestor for misclassified observations
  is.descendant <- function(all.leaves, node) {
    if (length(all.leaves) == 0L) return(logical(0L))
    all.leaves <- as.numeric(all.leaves)
    node       <- as.numeric(node)
    result     <- logical(length(all.leaves))
    for (i in seq_along(all.leaves)) {
      leaf <- all.leaves[i]
      while (leaf > node) {
        leaf <- trunc(leaf / 2L)
        if (leaf == node) { result[i] <- TRUE; break }
      }
    }
    result
  }
  
  where.tbl <- table(rp$where)
  names(where.tbl) <- rownames(rp$frame)[as.integer(names(where.tbl))]
  
  for (u in seq_along(u.bad.nodes)) {
    desc.vec <- is.descendant(all.nodes, u.bad.nodes[u])
    me       <- where.tbl[all.nodes][desc.vec]
    winner   <- names(me)[me == max(me)][1L]
    leaves[leaves == u.bad.leaves[u]] <- which(rownames(rp$frame) == winner)
  }
  leaves
}



# -------------------------------------------------------------------------
# Internal: normalizing
# -------------------------------------------------------------------------


# Ensure newdata is set - some packages (e1071, C50) require explicit
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





